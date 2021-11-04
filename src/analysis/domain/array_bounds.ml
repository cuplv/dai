open Dai
open Import
open Apron
open Option.Monad_infix
open Syntax

module Addr_map = struct
  type t = Addr.Abstract.t Map.M(String).t [@@deriving compare, equal, hash, sexp]

  let join =
    Map.merge ~f:(fun ~key:_ -> function
      | `Both (a1, a2) -> Some (Addr.Abstract.union a1 a2) | `Left a | `Right a -> Some a)

  let implies l r =
    List.for_all (Map.to_alist l) ~f:(fun (k, lv) ->
        match Map.find r k with Some rv -> Addr.Abstract.is_subset lv ~of_:rv | None -> false)

  let empty = Map.empty (module String)

  let pp fs =
    let pp_assoc fs (k, v) = Format.fprintf fs "%s -> %a" k Addr.Abstract.pp v in
    Map.to_alist >> (List.pp ", " ~pre:"{" ~suf:"}" pp_assoc) fs

  let add ~key ~addr amap =
    let data =
      match Map.find amap key with
      | Some addrs -> Addr.Abstract.add addrs addr
      | None -> Addr.Abstract.singleton addr
    in
    Map.set ~key ~data amap

  let set ~key ~aaddr amap = Map.set amap ~key ~data:aaddr

  let forget vars amap = List.fold vars ~init:amap ~f:Map.remove
end

(* Keep a map of variable names to array abstract addresses, along with an APRON interval. *)
type t = Addr_map.t * Itv.t [@@deriving compare, equal, hash, sexp]

let is_bot = snd >> Itv.is_bot

let init () = (Map.empty (module String), Itv.init ())

let bottom () = (Addr_map.empty, Itv.bottom ())

let widen (al, il) (ar, ir) = (Addr_map.join al ar, Itv.widen il ir)

let join (al, il) (ar, ir) = (Addr_map.join al ar, Itv.join il ir)

let implies (al, il) (ar, ir) = Addr_map.implies al ar && Itv.implies il ir

let pp fs (a, i) = Format.fprintf fs "[%a; %a]" Addr_map.pp a Itv.pp i

let sanitize x = x

let show x =
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

let hash = seeded_hash

let _apron_var_of_array_cell addr idx =
  Var.of_string (Format.asprintf "__dai_%a.%i" Addr.pp addr idx)

let apron_var_of_field addr field = Var.of_string (Format.asprintf "__dai_%a.%s" Addr.pp addr field)

let apron_var_of_array_len addr = Var.of_string (Format.asprintf "__dai_%a.len" Addr.pp addr)

let project_fields itv addrs =
  Itv.filter_env itv
    ~f:
      (String.chop_prefix ~prefix:"__dai_alloc_"
      >> Option.exists
           ~f:(String.take_while ~f:Char.is_digit >> Int.of_string >> Addr.of_int >> Set.mem addrs)
      )

let forget_used_tmp_vars stmt (am, itv) =
  let used_tmps = Ast.Stmt.uses stmt |> Set.filter ~f:(String.is_prefix ~prefix:"__dai_tmp") in
  let new_am =
    let vars_to_forget = Map.keys am |> List.filter ~f:(Set.mem used_tmps) in
    Addr_map.forget vars_to_forget am
  in
  let new_itv =
    let vars_to_forget =
      Abstract1.env itv |> Environment.vars |> snd
      |> Array.filter ~f:(Var.to_string >> Set.mem used_tmps)
    in
    Itv.forget vars_to_forget itv
  in
  (new_am, new_itv)

let texpr_of_expr (am, itv) expr =
  let man = Itv.get_man () in
  let join_intervals (itv1 : Interval.t) (itv2 : Interval.t) =
    let inf = if Scalar.cmp itv1.inf itv2.inf < 0 then itv1.inf else itv2.inf in
    let sup = if Scalar.cmp itv1.sup itv2.sup > 0 then itv1.sup else itv2.sup in
    Interval.of_infsup inf sup
  in
  let handle_array_or_field_expr itv = function
    | Ast.Expr.Deref { rcvr; field = "length" } ->
        Map.find am rcvr >>= fun aaddr ->
        if Set.is_empty aaddr then None
        else
          Set.fold aaddr ~init:Interval.bottom ~f:(fun acc addr ->
              let len = apron_var_of_array_len addr in
              if Environment.mem_var (Abstract1.env itv) len then
                join_intervals acc (Abstract1.bound_variable man itv len)
              else acc)
          |> fun v -> Some (Texpr1.Cst (Coeff.Interval v))
    | Ast.Expr.Deref { rcvr; field } ->
        ( Map.find am rcvr >>| fun aaddr ->
          Set.fold aaddr ~init:Interval.bottom ~f:(fun acc addr ->
              let field_var = apron_var_of_field addr field in
              join_intervals acc (Itv.lookup itv field_var)) )
        >>| fun interval -> Texpr1.Cst (Coeff.Interval interval)
        (*      Map.find am rcvr >>= fun aaddr -> *)
        (*
        Itv.texpr_of_expr ~fallback:handle_array_expr itv field >>| Itv.eval_texpr itv
        >>= fun { inf; sup } ->
        Map.find am rcvr_ident >>= fun aaddr ->
        if Scalar.is_infty inf < 0 || Scalar.is_infty sup > 0 then
          Some (Texpr1.Cst (Coeff.Interval Interval.top))
        else if Scalar.is_infty inf > 0 || Scalar.is_infty sup < 0 then
          Some (Texpr1.Cst (Coeff.Interval Interval.bottom))
        else
          let to_float scalar =
            match scalar with
            | Scalar.Float f -> f
            | Scalar.Mpqf m -> Mpqf.to_float m
            | Scalar.Mpfrf m -> Mpfrf.to_float m
          in
          let min_idx = Float.(round_up >> to_int) (to_float inf) in
          let max_idx = Float.(round_down >> to_int) (to_float sup) in
          List.fold (range min_idx max_idx) ~init:Interval.bottom ~f:(fun a idx ->
              Addr.Abstract.fold aaddr ~init:a ~f:(fun a addr ->
                  let av = apron_var_of_array_cell addr idx in
                  if Environment.mem_var (Abstract1.env itv) av then
                    let value = Abstract1.bound_variable man itv av in
                    join_intervals a value
                  else a))
          |> fun v -> Some (Texpr1.Cst (Coeff.Interval v))*)
    | Ast.Expr.Array_literal { elts = _; alloc_site = _ } ->
        failwith
          "Unreachable by construction; array literals only occur at top level of assignment."
    | Ast.Expr.Array_access _ -> failwith "todo: texpr_of_expr array_access"
    | Ast.Expr.Array_create _ -> failwith "todo: texpr_of_expr array_create"
    | _ -> None
  in
  Itv.texpr_of_expr itv ~fallback:handle_array_or_field_expr expr

let extend_env_by_uses stmt (am, itv) =
  let env = Abstract1.env itv in
  let man = Itv.get_man () in
  Ast.Stmt.uses stmt
  |> Set.filter ~f:(Var.of_string >> Environment.mem_var env >> not)
  |> Set.to_array |> Array.map ~f:Var.of_string
  |> Environment.add env [||]
  |> fun new_env -> (am, Abstract1.change_environment man itv new_env false)

open Ast

let interpret stmt phi =
  let man = Itv.get_man () in

  (* environment wrangling:
   * first, extend input state [phi]'s env to include all variables used in [stmt];
   * then, forget any used temporary variables after applying the transfer function
      (temporary variables are added by Dai.Frontend.Cfg_parser to extract complicated Java expressions to equivalent sequences of simpler statements;
         e.g. `x[y] = foo(bar(o.f));` parses to `tmp1 = o.f; tmp2 = bar(tmp1); tmp3 = foo(tmp2); x[y] = tmp3;`
         each such temp variable is used exactly once so can be forgotten after that use) *)
  let am, itv = extend_env_by_uses stmt phi in
  forget_used_tmp_vars stmt
  @@
  (* transfer functions for non-procedure-call statements *)
  match stmt with
  | Assign { lhs; rhs = Expr.Var v } when Map.mem am v ->
      (Map.set am ~key:lhs ~data:(Map.find_exn am v), itv)
  | Assign { lhs; rhs = Expr.Array_literal { elts; alloc_site } } ->
      let am = Addr_map.add am ~key:lhs ~addr:alloc_site in
      let len = List.length elts in
      let itv =
        Itv.assign itv
          (apron_var_of_array_len alloc_site)
          Texpr1.(Cst (Coeff.Scalar (Scalar.of_int len)))
      in
      (am, itv)
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
      match texpr_of_expr (am, itv) rhs with
      | Some rhs_texpr ->
          (am, Abstract1.assign_texpr man itv_new_env lhs (Texpr1.of_expr new_env rhs_texpr) None)
      | None ->
          if Environment.mem_var env lhs then
            (* lhs was constrained, quantify that out *)
            (am, Abstract1.forget_array man itv [| lhs |] false)
          else (* lhs was unconstrained, treat as a `skip`*) (am, itv))
  | Assume e ->
      (am, Itv.meet_with_constraint ~fallback:(fun itv e -> texpr_of_expr (am, itv) e) itv e)
  | Skip -> (am, itv)
  | Expr _ | Write _ | Array_write _ | Call _ | Exceptional_call _ ->
      failwith "todo: Array_bounds#interpret"

let array_accesses : Stmt.t -> (Expr.t * Expr.t) list =
  let rec expr_derefs = function
    | Expr.Deref { rcvr = _; field = _ } -> failwith "todo"
    | Expr.Lit _ | Expr.Var _ -> []
    | Expr.Binop { l; op = _; r } -> expr_derefs l @ expr_derefs r
    | Expr.Unop { op = _; e } -> expr_derefs e
    | Expr.Array_literal { elts; alloc_site = _ } -> List.bind elts ~f:expr_derefs
    | Expr.Array_access _ | Expr.Array_create _ -> failwith "todo"
  in
  function
  | Assign { lhs = _; rhs } -> expr_derefs rhs
  | Write { rcvr = _; field = _; rhs = _ } ->
      failwith "Todo" (*(Expr.Var rcvr, field) :: expr_derefs rhs*)
  | Expr e | Assume e -> expr_derefs e
  | Call { actuals; _ } -> List.bind actuals ~f:expr_derefs
  | Skip -> []
  | Array_write _ | Exceptional_call _ -> failwith "todo"

(** Some(true/false) indicates [idx] is definitely in/out-side of [addr]'s bounds;
    None indicates it could be either
*)
let is_in_bounds addr (idx : Interval.t) itv =
  let env = Abstract1.env itv in
  let arr_len = apron_var_of_array_len addr in
  if not (Environment.mem_var env arr_len) then None
  else
    let len_itv =
      Texpr1.of_expr env (Texpr1.Var arr_len) |> Abstract1.bound_texpr (Itv.get_man ()) itv
    in
    let min_len, max_len = (len_itv.inf, len_itv.sup) in
    let zero = Scalar.of_int 0 in
    if zero <= idx.inf && idx.sup < min_len then Some true
    else if idx.sup < zero || idx.inf >= max_len then Some false
    else None

(** Lift [is_in_bounds] to take a receiver variable and index expression,
    and return a value of the same form as [is_in_bounds] *)
let is_safe (var : string) (idx : Ast.Expr.t) ((am, itv) : t) =
  if not @@ Map.mem am var then (
    Format.(fprintf std_formatter) "WARNING: No array address information available for %s\n" var;
    None)
  else
    Map.find am var >>= fun aaddr ->
    texpr_of_expr (am, itv) idx >>| Itv.eval_texpr itv >>= fun idx ->
    assert (not @@ Set.is_empty aaddr);
    Set.to_list aaddr
    |> List.map ~f:(fun a -> is_in_bounds a idx itv)
    |> List.reduce_exn ~f:(fun x y ->
           match (x, y) with Some a, Some b when Bool.equal a b -> Some a | _ -> None)

let arrayify_varargs (actuals : Expr.t list) (formals : int) (phi : t) : Expr.t list * t =
  let tmp_var = "__DAI_array_for_varargs" in
  let varargs = List.drop actuals (formals - 1) in
  let arrayify =
    Stmt.Assign
      {
        lhs = tmp_var;
        rhs = Expr.Array_literal { elts = varargs; alloc_site = Alloc_site.fresh () };
      }
  in
  let phi' = interpret arrayify phi in
  (List.take actuals (formals - 1) @ [ Expr.Var tmp_var ], phi')

let call ~(callee : Cfg.Fn.t) ~callsite ~caller_state ~fields =
  let caller_am, caller_itv = extend_env_by_uses callsite caller_state in
  match callsite with
  | Ast.Stmt.Call { rcvr; actuals; alloc_site = _; _ } ->
      let actuals, (caller_am, caller_itv) =
        if List.(length actuals = length callee.formals) then (actuals, (caller_am, caller_itv))
        else arrayify_varargs actuals (List.length callee.formals) (caller_am, caller_itv)
      in
      (* re-scope the address map to include only the formal parameters *)
      let callee_am =
        List.(fold (zip_exn callee.formals actuals)) ~init:Addr_map.empty
          ~f:(fun am (formal, actual) ->
            match actual with
            | Ast.Expr.Var v ->
                Option.fold (Map.find caller_am v) ~init:am ~f:(fun am actual_aaddr ->
                    Map.add_exn am ~key:formal ~data:actual_aaddr)
            | _ -> am)
      in
      (* project the caller state's interval down to just formal parameters and fields thereof and rebind receiver fields to callee-local variables *)
      let callee_itv =
        let param_fields_itv =
          Map.data callee_am
          |> List.fold ~init:(Set.empty (module Addr)) ~f:Set.union
          |> project_fields caller_itv
        in
        let param_bindings =
          List.(filter_map (zip_exn callee.formals actuals)) ~f:(fun (f, a) ->
              texpr_of_expr caller_state a >>| Itv.eval_texpr caller_itv >>| pair (Var.of_string f))
        in
        let params_itv =
          List.fold param_bindings ~init:param_fields_itv ~f:(fun itv (formal, bound) ->
              Itv.assign itv formal Texpr1.(Cst (Coeff.Interval bound)))
        in
        let rcvr_field_bindings =
          if String.equal rcvr "this" then
            let ({ package; class_name; _ } : Method_id.t) = callee.method_id in
            Declared_fields.lookup ~package ~class_name fields |> fun { instance; _ } ->
            Set.fold instance ~init:[] ~f:(fun acc var_str ->
                let var = Var.of_string var_str in
                if Environment.mem_var (Abstract1.env caller_itv) var then
                  (var, Itv.lookup caller_itv var) :: acc
                else acc)
          else
            let rcvr_aaddr = Map.find caller_am rcvr |> Option.value ~default:Addr.Abstract.empty in
            Abstract1.env caller_itv |> Environment.vars |> snd
            |> Array.fold ~init:[] ~f:(fun acc var ->
                   match String.chop_prefix (Var.to_string var) ~prefix:"__dai_alloc_" with
                   | Some var_str
                     when String.take_while var_str ~f:Char.is_digit
                          |> Int.of_string |> Addr.of_int |> Set.mem rcvr_aaddr ->
                       let fld_name =
                         String.(index_exn var_str '.' |> Int.succ |> drop_prefix var_str)
                       in
                       (Var.of_string fld_name, Itv.lookup caller_itv var) :: acc
                   | _ -> acc)
        in
        List.fold ~init:params_itv rcvr_field_bindings ~f:(fun itv (var, value) ->
            Itv.assign itv var Texpr1.(Cst (Coeff.Interval value)))
      in
      (callee_am, callee_itv)
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)

let return ~(callee : Cfg.Fn.t) ~(caller : Cfg.Fn.t) ~callsite
    ~caller_state:(caller_amap, caller_itv) ~return_state:(return_amap, return_itv) ~fields =
  forget_used_tmp_vars callsite
  @@
  match callsite with
  | Ast.Stmt.Call { lhs; rcvr; meth; alloc_site; actuals = _ } ->
      let man = Itv.get_man () in
      (* [rcvr_aaddr] is one of:
         * [`This], indicating an absent or "this" receiver;
         * [`AAddr aaddr], indicating a receiver with abstract address [aaddr]; or
         * [`Static], indicating a static call
      *)
      let rcvr_aaddr =
        if callee.method_id.static then `Static
        else if String.equal rcvr "this" then `This
        else
          `AAddr
            (match alloc_site with
            | Some a -> Addr.Abstract.of_alloc_site a
            | None -> (
                match Map.find caller_amap rcvr with
                | Some aaddr -> aaddr
                | None ->
                    failwith
                      (Format.asprintf "error: no materialized address for receiver of callsite %a"
                         Ast.Stmt.pp callsite)))
      in
      let callee_instance_fields =
        if callee.method_id.static then String.Set.empty
        else
          Declared_fields.lookup_instance fields ~package:callee.method_id.package
            ~class_name:callee.method_id.class_name
      in
      let retvar = Var.of_string Cfg.retvar in
      let return_val =
        if Environment.mem_var (Abstract1.env return_itv) retvar then
          Abstract1.bound_variable man return_itv retvar
        else Interval.top
      in
      (* (1) transfer constraints on the return value (and its fields) to the lhs of the callsite, then
         (2) transfer constraints on instance fields at the callee return to equivalent constraints on the receiver's fields
         (3) transfer constraints on static fields at the callee return to equivalents on the receiver, if they share a class
      *)
      let itv =
        (* (1) *)
        ((match lhs with
         | Some lhs ->
             Itv.assign caller_itv (Var.of_string lhs) Texpr1.(Cst (Coeff.Interval return_val))
         | None -> caller_itv)
        |>
        match Map.find return_amap Cfg.retvar with
        | None -> Fn.id
        | Some retval_aaddr ->
            let retval_fields_itv = project_fields return_itv retval_aaddr in
            Itv.meet retval_fields_itv)
        (* (2) *)
        |> fun itv ->
        Set.fold callee_instance_fields ~init:itv ~f:(fun itv fld ->
            let fld_var = Var.of_string fld in
            if Environment.mem_var (Abstract1.env return_itv) fld_var then
              let fld_val = Itv.lookup return_itv fld_var in
              match rcvr_aaddr with
              | `This -> Itv.assign itv (Var.of_string fld) Texpr1.(Cst (Coeff.Interval fld_val))
              | `AAddr aaddr ->
                  Set.fold aaddr ~init:itv ~f:(fun itv addr ->
                      Itv.assign itv (apron_var_of_field addr fld)
                        Texpr1.(Cst (Coeff.Interval fld_val)))
              | `Static -> itv
            else itv)
        (* (3) *)
        |>
        if Cfg.Fn.is_same_class callee caller then fun itv ->
          let static_fields =
            Declared_fields.lookup_static fields ~package:callee.method_id.package
              ~class_name:callee.method_id.class_name
          in
          Set.fold static_fields ~init:itv ~f:(fun itv fld ->
              let fld_var = Var.of_string fld in
              let fld_val = Itv.lookup itv fld_var in
              if Interval.is_top fld_val then itv
              else Itv.assign itv fld_var Texpr1.(Cst (Coeff.Interval fld_val)))
        else Fn.id
      in
      (* (1) bind the receiver's abstract address if needed, then
         (2) transfer any return-value address binding to the callsite's lhs *)
      let amap =
        (* (1) *)
        (match rcvr_aaddr with
        | `This | `Static -> caller_amap
        | `AAddr rcvr_aaddr ->
            if String.equal "<init>" meth && Option.is_some lhs then
              Addr_map.set caller_amap ~key:(Option.value_exn lhs) ~aaddr:rcvr_aaddr
            else Addr_map.set caller_amap ~key:rcvr ~aaddr:rcvr_aaddr)
        (* (2) *)
        |>
        match Map.find return_amap Cfg.retvar with
        | Some retval_aaddr when Option.is_some lhs ->
            Addr_map.set ~key:(Option.value_exn lhs) ~aaddr:retval_aaddr
        | _ -> Fn.id
      in
      (amap, itv)
  | Ast.Stmt.Exceptional_call _ -> failwith "todo: exceptional Array_bounds#return"
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)

let noop_library_methods : String.Set.t = String.Set.of_list [ "toString"; "hashCode"; "println" ]

let approximate_missing_callee ~caller_state ~callsite =
  Format.(fprintf std_formatter)
    "WARNING: missing callee code for callsite %a\n" Ast.Stmt.pp callsite;
  (* as described in [interpret] above, forget used tmp vars after they are used *)
  forget_used_tmp_vars callsite
  @@
  match callsite with
  (* unknown constructor -- just bind the address and ignore the rest *)
  | Ast.Stmt.Call { lhs = Some lhs; rcvr = _; meth = _; actuals = _; alloc_site = Some a } ->
      let caller_am, caller_itv = caller_state in
      let am = Addr_map.set caller_am ~key:lhs ~aaddr:(Addr.Abstract.singleton a) in
      (am, caller_itv)
  (* unknown constructor, value being discarded -- no-op*)
  | Ast.Stmt.Call { lhs = None; alloc_site = Some _; _ } -> caller_state
  (* call to assumed-no-op library methods -- no-op*)
  | Ast.Stmt.Call { lhs = _; rcvr = _; meth; actuals = _; alloc_site = _ }
    when Set.mem noop_library_methods meth ->
      caller_state
  | Ast.Stmt.Call { lhs = _; rcvr = _; meth = _; actuals = _; alloc_site = _ } ->
      failwith "todo: approximate_missing_callee"
  | Ast.Stmt.Exceptional_call _ ->
      failwith "todo: approximate exceptional control flow through missing procedure"
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)

(** deferred to end of this file to avoid colliding with Dai.Import.(<=) *)
let ( <= ) = implies
