open Import
open Apron
open Option.Monad_infix

module Addr_map = struct
  type t = Addr.Abstract.t Map.M(String).t [@@deriving compare, sexp]

  let join =
    Map.merge ~f:(fun ~key:_ ->
      function `Both (a1, a2) -> Some (Addr.Abstract.union a1 a2) | `Left a | `Right a -> Some a)

  let implies l r =
    List.for_all (Map.to_alist l) ~f:(fun (k, lv) ->
        match Map.find r k with Some rv -> Addr.Abstract.is_subset lv ~of_:rv | None -> false)

  let equal = Map.equal Addr.Abstract.equal

  let pp fs =
    Map.to_alist >> (List.pp ", " ~pre:"{" ~suf:"}" (pp_pair String.pp Addr.Abstract.pp)) fs
end

(* Keep a map of variable names to array abstract addresses, along with an APRON interval. None indicates bottom. *)
type t = (Addr_map.t * Itv.t) option [@@deriving compare]

let is_bot = Option.fold ~init:true ~f:(fun _ (_, itv) -> Itv.is_bot itv)

let init () = pair (Map.empty (module String)) (Itv.init ()) |> Option.some

let mk_binary_op a_op i_op l r =
  match (l, r) with
  | Some (al, il), Some (ar, ir) -> Some (a_op al ar, i_op il ir)
  | Some (a, i), _ | _, Some (a, i) -> Some (a, i)
  | _ -> None

let widen = mk_binary_op Addr_map.join Itv.widen

let join = mk_binary_op Addr_map.join Itv.join

let implies l r =
  match (l, r) with
  | None, _ -> true
  | _, None -> false
  | Some (al, il), Some (ar, ir) -> Addr_map.implies al ar && Itv.implies il ir

let apron_var_of_array_cell addr idx = Var.of_string (Format.asprintf "%a.%i" Addr.pp addr idx)

let apron_var_of_array_len addr = Var.of_string (Format.asprintf "%a.len" Addr.pp addr)

let texpr_of_expr expr (am, itv) =
  let man = Itv.get_man () in
  let join_intervals (itv1 : Interval.t) (itv2 : Interval.t) =
    let inf = if Scalar.cmp itv1.inf itv2.inf < 0 then itv1.inf else itv2.inf in
    let sup = if Scalar.cmp itv1.sup itv2.sup > 0 then itv1.sup else itv2.sup in
    Interval.of_infsup inf sup
  in
  let rec handle_array_expr itv = function
    | Ast.Expr.Deref { rcvr = Ast.Expr.Var rcvr_ident; field = Ast.Expr.Var "length" } ->
        Map.find am rcvr_ident >>= fun aaddr ->
        if Set.is_empty aaddr then None
        else
          Set.fold aaddr ~init:Interval.bottom ~f:(fun acc addr ->
              let len = apron_var_of_array_len addr in
              if Environment.mem_var (Abstract1.env itv) len then
                join_intervals acc (Abstract1.bound_variable man itv len)
              else acc)
          |> fun v -> Some (Texpr1.Cst (Coeff.Interval v))
    | Ast.Expr.Deref { rcvr = Ast.Expr.Var rcvr_ident; field } ->
        Itv.texpr_of_expr ~fallback:handle_array_expr itv field >>| Itv.eval_texpr itv
        >>= fun { inf; sup } ->
        Map.find am rcvr_ident >>= fun aaddr ->
        let min_idx, max_idx =
          pair
            ( ( match inf with
              | Scalar.Float f -> f
              | Scalar.Mpqf m -> Mpqf.to_float m
              | Scalar.Mpfrf m -> Mpfrf.to_float m )
            |> Float.(round_up >> to_int) )
            ( ( match sup with
              | Scalar.Float f -> f
              | Scalar.Mpqf m -> Mpqf.to_float m
              | Scalar.Mpfrf m -> Mpfrf.to_float m )
            |> Float.(round_down >> to_int) )
        in
        List.fold (range min_idx max_idx) ~init:Interval.bottom ~f:(fun a idx ->
            Addr.Abstract.fold aaddr ~init:a ~f:(fun a addr ->
                let av = apron_var_of_array_cell addr idx in
                if Environment.mem_var (Abstract1.env itv) av then
                  let value = Abstract1.bound_variable man itv av in
                  join_intervals a value
                else a))
        |> fun v -> Some (Texpr1.Cst (Coeff.Interval v))
    | Ast.Expr.Array { elts = _; alloc_site = _ } ->
        failwith
          "Unreachable by construction; array literals only occur at top level of assignment."
    | _ -> None
  in
  Itv.texpr_of_expr itv ~fallback:handle_array_expr expr

let extend_env_with_uses stmt (am, itv) =
  let open Ast in
  let env = Abstract1.env itv in
  let man = Itv.get_man () in
  let rec uses = function
    | Expr.Var v when not @@ Map.mem am v -> String.Set.singleton v
    | Expr.Binop { l; op = _; r } -> Set.union (uses l) (uses r)
    | Expr.Unop { op = _; e } -> uses e
    | Expr.Deref { rcvr; field } -> Set.union (uses rcvr) (uses field)
    | Expr.Array { elts; alloc_site = _ } ->
        List.fold elts ~init:String.Set.empty ~f:(fun a c -> Set.union a (uses c))
    | _ -> String.Set.empty
  in
  ( match stmt with
  | Stmt.Assign { lhs = _; rhs } -> uses rhs
  | Stmt.Write { rcvr = _; field; rhs } -> Set.union (uses field) (uses rhs)
  | Stmt.Throw { exn } -> uses exn
  | Stmt.Expr e | Stmt.Assume e -> uses e
  | Stmt.Skip -> String.Set.empty )
  |> Set.filter ~f:(Var.of_string >> Environment.mem_var env >> not)
  |> Set.to_array |> Array.map ~f:Var.of_string
  |> Environment.add env [||]
  |> fun new_env -> (am, Abstract1.change_environment man itv new_env false)

let interpret stmt phi =
  let man = Itv.get_man () in
  phi >>| extend_env_with_uses stmt >>= fun (am, itv) ->
  let open Ast.Stmt in
  match stmt with
  | Assign { lhs; rhs = Ast.Expr.Var v } when Map.mem am v ->
      Some (Map.set am ~key:lhs ~data:(Map.find_exn am v), itv)
  | Assign { lhs; rhs = Ast.Expr.Array { elts; alloc_site } } ->
      let addr = Addr.of_alloc_site alloc_site in
      let am =
        Map.change am lhs ~f:(function
          | Some old_aaddr -> Some (Addr.Abstract.add old_aaddr addr)
          | None -> Some (Addr.Abstract.singleton addr))
      in
      let array_len_var = [| apron_var_of_array_len addr |] in
      let env = Environment.lce (Abstract1.env itv) (Environment.make [||] array_len_var) in
      let array_length_binding =
        Abstract1.of_box man env array_len_var
          [| Interval.of_int (List.length elts) (List.length elts) |]
      in
      let itv = Abstract1.change_environment man itv env false in
      Abstract1.meet man itv array_length_binding |> pair am |> Option.some
      (* the below snippet tracks actual array contents; just tracking lengths for now though
     If tracking of contents is enabled, should also write a case for Ast.Stmt.Write, currently treated as skip.
   *)
      (*
      Some
        ( am,
          List.foldi elts ~init:([], []) ~f:(fun i (acc_vs, acc_itvs) elt ->
              match texpr_of_expr elt (am, itv) >>| Itv.eval_texpr itv with
              | Some elt_itv ->
                  let v = apron_var_of_array_cell addr i in
                  (v :: acc_vs, elt_itv :: acc_itvs)
              | _ -> (acc_vs, acc_itvs))
          |> fun (vs, itvs) ->
          let vs = Array.of_list vs in
          let itvs = Array.of_list itvs in

          let new_env = Environment.make [||] vs in
          let old_env = Abstract1.env itv in
          let env = Environment.lce old_env new_env in
          let itv = Abstract1.change_environment man itv env false in
          Abstract1.meet man itv @@ Abstract1.of_box man env vs itvs )
      *)
  | Assign { lhs; rhs } -> (
      let lhs = Var.of_string lhs in
      let env = Abstract1.env itv in
      let new_env =
        if Environment.mem_var env lhs then env else Environment.add env [||] [| lhs |]
      in
      let itv_new_env = Abstract1.change_environment man itv new_env true in
      match texpr_of_expr rhs (am, itv) with
      | Some rhs_texpr ->
          Some
            (am, Abstract1.assign_texpr man itv_new_env lhs (Texpr1.of_expr new_env rhs_texpr) None)
      | None ->
          if Environment.mem_var env lhs then
            (* lhs was constrained, quantify that out *)
            Some (am, Abstract1.forget_array man itv [| lhs |] false)
          else (* lhs was unconstrained, treat as a `skip`*) Some (am, itv) )
  | Throw { exn = _ } -> None
  | Assume e -> (
      match Itv.meet_with_constraint ~fallback:(fun itv e -> texpr_of_expr e (am, itv)) itv e with
      | itv when Abstract1.is_bottom man itv -> None
      | itv -> Some (am, itv) )
  | Expr _ | Write _ | Skip -> Some (am, itv)

let pp = Option.pp (pp_pair Addr_map.pp Itv.pp) "bottom"

let sanitize x = x

let show x =
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

let hash = seeded_hash

let equal l r =
  match (l, r) with
  | None, None -> true
  | Some (al, il), Some (ar, ir) -> Addr_map.equal al ar && Itv.equal il ir
  | _ -> false

let sexp_of_t = function
  | Some (a, i) -> Sexp.List [ Addr_map.sexp_of_t a; Itv.sexp_of_t i ]
  | _ -> Sexp.Atom "bottom"

let t_of_sexp = function
  | Sexp.Atom "bottom" -> None
  | Sexp.List [ a_sexp; i_sexp ] -> Some (Addr_map.t_of_sexp a_sexp, Itv.t_of_sexp i_sexp)
  | _ -> failwith "malformed abstract state s-expression"

let hash_fold_t seed = function
  | Some (a, i) ->
      let i_hash = Itv.hash_fold_t seed i in
      Ppx_hash_lib.Std.Hash.fold_int i_hash (Addr.Abstract.hash 13 a)
  | None -> seed

(** Some(true/false) indicates [idx] is definitely in/out-side of [addr]'s bounds;
    None indicates it could be either
*)
let is_in_bounds addr (idx : Interval.t) itv =
  let env = Abstract1.env itv in
  let len_itv =
    Texpr1.of_expr env (Texpr1.Var (apron_var_of_array_len addr))
    |> Abstract1.bound_texpr (Itv.get_man ()) itv
  in
  let min_len, max_len = (len_itv.inf, len_itv.sup) in
  let zero = Scalar.of_int 0 in
  if zero <= idx.inf && idx.sup < min_len then Some true
  else if idx.sup < zero || idx.inf >= max_len then Some false
  else None

(** Lift [is_in_bounds] to take a receiver variable and index expression, and return a value of the same form *)
let is_safe var idx state =
  match state with
  | None -> Some true (* state is unreachable so access is safe *)
  | Some (am, itv) ->
      if not @@ Map.mem am var then (
        Format.fprintf Format.std_formatter "No array address information available for %s\n" var;
        None )
      else
        Map.find am var >>= fun aaddr ->
        texpr_of_expr idx (am, itv) >>| Itv.eval_texpr itv >>= fun idx ->
        assert (not @@ Set.is_empty aaddr);
        Set.to_list aaddr
        |> List.map ~f:(fun a -> is_in_bounds a idx itv)
        |> List.reduce_exn ~f:(fun x y ->
               match (x, y) with Some a, Some b when Bool.equal a b -> Some a | _ -> None)
