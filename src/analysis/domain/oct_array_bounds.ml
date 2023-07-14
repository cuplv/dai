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

  let get_addrs (vars : string list) amap =
    let vars = String.Set.of_list vars in
    Map.filter_keys amap ~f:(Set.mem vars) |> Map.data |> List.bind ~f:Set.to_list

  let forget vars amap = List.fold vars ~init:amap ~f:Map.remove
end

(* Keep a map of variable names to array abstract addresses, along with an APRON interval. *)
type t = Addr_map.t * Octagon.t [@@deriving compare, equal, hash, sexp]

let is_bot = snd >> Octagon.is_bot

let init () = (Map.empty (module String), Octagon.init ())

let bottom () = (Addr_map.empty, Octagon.bottom ())

let top () = (Addr_map.empty, Octagon.top ())

let widen (al, il) (ar, ir) = (Addr_map.join al ar, Octagon.widen il ir)

let join (al, il) (ar, ir) = (Addr_map.join al ar, Octagon.join il ir)

let implies (al, il) (ar, ir) = Addr_map.implies al ar && Octagon.implies il ir

let pp fs (a, i) = Format.fprintf fs "[%a; %a]" Addr_map.pp a Octagon.pp i

let sanitize x = x

let show x =
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

let hash = seeded_hash

let apron_var_of_array_cell addr idx =
  Var.of_string (Format.asprintf "__dai_%a.%a" Addr.pp addr Int64.pp idx)

let array_cell_of_apron_var var =
  Var.to_string var |> String.split ~on:'.' |> function
  | [ alloc_str; idx_str ] -> (
      String.chop_prefix alloc_str ~prefix:"__dai_alloc_" >>| (Int.of_string >> Addr.of_int)
      >>= fun addr -> try Some (addr, Int64.of_string idx_str) with _ -> None)
  | _ -> None

let apron_var_of_field addr field = Var.of_string (Format.asprintf "__dai_%a.%s" Addr.pp addr field)

let apron_var_of_array_len addr = Var.of_string (Format.asprintf "__dai_%a.len" Addr.pp addr)

let project_fields oct addrs =
  Octagon.filter_env oct
    ~f:
      (String.chop_prefix ~prefix:"__dai_alloc_"
      >> Option.exists
           ~f:(String.take_while ~f:Char.is_digit >> Int.of_string >> Addr.of_int >> Set.mem addrs)
      )

let forget_vars vars (am, oct) =
  let new_am =
    let vars_to_forget = Map.keys am |> List.filter ~f:(Set.mem vars) in
    Addr_map.forget vars_to_forget am
  in
  let new_oct =
    let vars_to_forget =
      Abstract1.env oct |> Environment.vars |> snd
      |> Array.filter ~f:(Var.to_string >> fun v -> Set.mem vars v)
    in
    Octagon.forget vars_to_forget oct
  in
  (new_am, new_oct)

let forget_used_tmp_vars stmt state =
  let used_tmps = Ast.Stmt.uses stmt |> Set.filter ~f:(String.is_prefix ~prefix:"__dai_tmp") in
  forget_vars used_tmps state

let texpr_of_expr (am, oct) expr =
  let man = Octagon.get_man () in
  let join_intervals (itv1 : Interval.t) (itv2 : Interval.t) =
    let inf = if Scalar.cmp itv1.inf itv2.inf < 0 then itv1.inf else itv2.inf in
    let sup = if Scalar.cmp itv1.sup itv2.sup > 0 then itv1.sup else itv2.sup in
    Interval.of_infsup inf sup
  in
  let handle_array_or_field_expr oct = function
    | Ast.Expr.Deref { rcvr; field = "length" } ->
        Map.find am rcvr >>= fun aaddr ->
        if Set.is_empty aaddr then None
        else
          Set.fold aaddr ~init:Interval.bottom ~f:(fun acc addr ->
              let len = apron_var_of_array_len addr in
              if Environment.mem_var (Abstract1.env oct) len then
                join_intervals acc (Abstract1.bound_variable man oct len)
              else acc)
          |> fun v -> Some (Texpr1.Cst (Coeff.Interval v))
    | Ast.Expr.Deref { rcvr; field } ->
        ( Map.find am rcvr >>| fun aaddr ->
          Set.fold aaddr ~init:Interval.bottom ~f:(fun acc addr ->
              let field_var = apron_var_of_field addr field in
              join_intervals acc (Octagon.lookup oct field_var)) )
        >>| fun interval -> Texpr1.Cst (Coeff.Interval interval)
        (*      Map.find am rcvr >>= fun aaddr -> *)
        (*
        Octagon.texpr_of_expr ~fallback:handle_array_expr oct field >>| Octagon.eval_texpr oct
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
                  if Environment.mem_var (Abstract1.env oct) av then
                    let value = Abstract1.bound_variable man oct av in
                    join_intervals a value
                  else a))
          |> fun v -> Some (Texpr1.Cst (Coeff.Interval v))*)
    | Ast.Expr.Array_literal { elts = _; alloc_site = _ } ->
        failwith
          "Unreachable by construction; array literals only occur at top level of assignment."
    | Ast.(Expr.Array_access { rcvr = Expr.Var rcvr; idx = Expr.Lit (Lit.Int idx) }) -> (
        match Map.find am rcvr with
        | None -> None
        | Some aaddr ->
            Set.fold aaddr ~init:Interval.bottom ~f:(fun acc addr ->
                let v = apron_var_of_array_cell addr idx in
                if Environment.mem_var (Abstract1.env oct) v then
                  join_intervals acc (Octagon.lookup oct v)
                else acc)
            |> fun v -> Some (Texpr1.Cst (Coeff.Interval v)))
    | _ -> None
  in
  Octagon.texpr_of_expr oct ~fallback:handle_array_or_field_expr expr

let extend_env_by_uses stmt (am, oct) =
  let env = Abstract1.env oct in
  let man = Octagon.get_man () in
  Ast.Stmt.uses stmt
  |> Set.filter ~f:(Var.of_string >> Environment.mem_var env >> not)
  |> Set.to_array |> Array.map ~f:Var.of_string
  |> Environment.add env [||]
  |> fun new_env -> (am, Octagon.change_environment man oct new_env false)

open Ast

let interpret stmt phi =
  let man = Octagon.get_man () in

  (* environment wrangling:
   * first, extend input state [phi]'s env to include all variables used in [stmt];
   * then, forget any used temporary variables after applying the transfer function
      (temporary variables are added by Dai.Frontend.Cfg_parser to extract complicated Java expressions to equivalent sequences of simpler statements;
         e.g. `x[y] = foo(bar(o.f));` parses to `tmp1 = o.f; tmp2 = bar(tmp1); tmp3 = foo(tmp2); x[y] = tmp3;`
         each such temp variable is used exactly once so can be forgotten after that use) *)
  let am, oct = extend_env_by_uses stmt phi in
  forget_used_tmp_vars stmt
  @@
  (* transfer functions for non-procedure-call statements *)
  match stmt with
  | Assign { lhs; rhs = Expr.Var v } when Map.mem am v ->
      (Map.set am ~key:lhs ~data:(Map.find_exn am v), oct)
  | Assign { lhs; rhs = Expr.Array_literal { elts; alloc_site } } -> (
      let am = Addr_map.add am ~key:lhs ~addr:alloc_site in
      let len = List.length elts in
      let oct =
        Octagon.assign oct
          (apron_var_of_array_len alloc_site)
          Texpr1.(Cst (Coeff.Scalar (Scalar.of_int len)))
      in

      ( am,
        List.foldi elts ~init:([], []) ~f:(fun i (acc_vs, acc_octs) elt ->
            match texpr_of_expr (am, oct) elt >>| Octagon.eval_texpr oct with
            | Some elt_oct ->
                let v = apron_var_of_array_cell alloc_site (Int64.of_int i) in
                (v :: acc_vs, elt_oct :: acc_octs)
            | _ -> (acc_vs, acc_octs))
        |> fun (vs, octs) ->
        let vs = Array.of_list vs in
        let octs = Array.of_list octs in

        let new_env = Environment.make [||] vs in
        let old_env = Abstract1.env oct in
        let lce = Environment.lce old_env new_env in
        let oct = Octagon.change_environment man oct lce false in
        try Abstract1.meet man oct @@ Abstract1.of_box man (Abstract1.env oct) vs octs
        with Apron.Manager.Error { exn = _; funid = _; msg } ->
          failwith ("Apron Manager Error: " ^ msg) ))
  | Assign { lhs; rhs } -> (
      let lhs = Var.of_string lhs in
      match texpr_of_expr (am, oct) rhs with
      | Some texpr -> (am, Octagon.assign oct lhs texpr)
      | None ->
          if Environment.mem_var (Abstract1.env oct) lhs then
            (* lhs was constrained, quantify that out *)
            (am, Abstract1.forget_array man oct [| lhs |] false)
          else (* lhs was unconstrained, treat as a `skip`*) (am, oct))
  (* let new_env =
       if Environment.mem_var env lhs then env else Environment.add env [||] [| lhs |]
     in
     let oct_new_env = Abstract1.change_environment man oct new_env true in
     match texpr_of_expr (am, oct) rhs with
     | Some rhs_texpr ->
         (am, Abstract1.assign_texpr man oct_new_env lhs (Texpr1.of_expr new_env rhs_texpr) None)
     | None ->
         if Environment.mem_var env lhs then
           (* lhs was constrained, quantify that out *)
           (am, Abstract1.forget_array man oct [| lhs |] false)
         else (* lhs was unconstrained, treat as a `skip`*) (am, oct)*)
  | Assume e ->
      (am, Octagon.meet_with_constraint ~fallback:(fun oct e -> texpr_of_expr (am, oct) e) oct e)
  | Skip -> (am, oct)
  | Expr _ -> failwith "todo: Oct_array_bounds#interpret Expr"
  | Write { rcvr; field; rhs } ->
      let oct =
        match Map.find am rcvr with
        | None -> oct
        | Some aaddr ->
            Set.fold aaddr ~init:oct ~f:(fun oct addr ->
                let v = apron_var_of_field addr field in
                match texpr_of_expr (am, oct) rhs with
                | Some texpr -> Octagon.weak_assign oct v texpr
                | None ->
                    if Environment.mem_var (Abstract1.env oct) v then
                      (* lhs was constrained, quantify that out *)
                      Abstract1.forget_array man oct [| v |] false
                    else (* lhs was unconstrained, treat as a `skip`*) oct)
      in

      (am, oct)
  | Array_write { rcvr; idx; rhs } ->
      let oct =
        match Map.find am rcvr with
        | None -> oct
        | Some aaddr ->
            let apply_write =
              match texpr_of_expr (am, oct) rhs with
              | Some texpr -> fun oct v -> Octagon.weak_assign oct v texpr
              | None ->
                  fun oct v ->
                    if Environment.mem_var (Abstract1.env oct) v then
                      Abstract1.forget_array man oct [| v |] false
                    else oct
            in
            let idx_interval =
              match texpr_of_expr (am, oct) idx with
              | None -> Interval.top
              | Some texpr -> Octagon.eval_texpr oct texpr
            in
            let affected_vars =
              Environment.vars (Abstract1.env oct)
              |> snd
              |> Array.filter ~f:(fun v ->
                     match array_cell_of_apron_var v with
                     | None -> false
                     | Some (addr, idx) -> (
                         Set.mem aaddr addr
                         &&
                         match Int64.to_int idx with
                         | Some idx -> Interval.(cmp (of_int idx idx) idx_interval) <= 0
                         | None ->
                             failwith
                               "index larger than 32-bit int max seems fishy. sound to return true \
                                here."))
            in
            Array.fold affected_vars ~init:oct ~f:(fun oct v -> apply_write oct v)
      in
      (am, oct)
  | Call _ | Exceptional_call _ -> failwith "unreachable, calls interpreted by [call] instead"

let array_accesses : Stmt.t -> (Expr.t * Expr.t) list =
  let rec expr_derefs = function
    | Expr.Deref { rcvr = _; field = _ } -> failwith "todo"
    | Expr.Lit _ | Expr.Var _ -> []
    | Expr.Binop { l; op = _; r } -> expr_derefs l @ expr_derefs r
    | Expr.Unop { op = _; e } -> expr_derefs e
    | Expr.Array_literal { elts; alloc_site = _ } -> List.bind elts ~f:expr_derefs
    | Expr.Array_access _ | Expr.Array_create _ | Expr.Method_ref _ | Expr.Class_lit _ ->
        failwith "todo"
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
let is_in_bounds addr (idx : Interval.t) oct =
  let env = Abstract1.env oct in
  let arr_len = apron_var_of_array_len addr in
  if not (Environment.mem_var env arr_len) then None
  else
    let len_itv =
      Texpr1.of_expr env (Texpr1.Var arr_len) |> Abstract1.bound_texpr (Octagon.get_man ()) oct
    in
    let min_len, max_len = (len_itv.inf, len_itv.sup) in
    let zero = Scalar.of_int 0 in
    if zero <= idx.inf && idx.sup < min_len then Some true
    else if idx.sup < zero || idx.inf >= max_len then Some false
    else None

(** Lift [is_in_bounds] to take a receiver variable and index expression,
    and return a value of the same form as [is_in_bounds] *)
let is_safe (var : string) (idx : Ast.Expr.t) ((am, oct) : t) =
  if not @@ Map.mem am var then (
    Format.(fprintf std_formatter) "WARNING: No array address information available for %s\n" var;
    None)
  else
    Map.find am var >>= fun aaddr ->
    texpr_of_expr (am, oct) idx >>| Octagon.eval_texpr oct >>= fun idx ->
    assert (not @@ Set.is_empty aaddr);
    Set.to_list aaddr
    |> List.map ~f:(fun a -> is_in_bounds a idx oct)
    |> List.reduce_exn ~f:(fun x y ->
           match (x, y) with Some a, Some b when Bool.equal a b -> Some a | _ -> None)

let arrayify_varargs (callee : Cfg.Fn.t) actuals formals phi : Expr.t list * t =
  let tmp_var = "__DAI_array_for_varargs" in
  let varargs = List.drop actuals (formals - 1) in
  let arrayify =
    Stmt.Assign
      {
        lhs = tmp_var;
        rhs =
          Expr.Array_literal { elts = varargs; alloc_site = Alloc_site.of_varargs callee.method_id };
      }
  in
  let phi' = interpret arrayify phi in
  (List.take actuals (formals - 1) @ [ Expr.Var tmp_var ], phi')

let call ~(callee : Cfg.Fn.t) ~callsite ~caller_state ~fields =
  let caller_am, caller_oct = extend_env_by_uses callsite caller_state in
  match callsite with
  | Ast.Stmt.Call { rcvr; actuals; _ } | Ast.Stmt.Exceptional_call { rcvr; actuals; _ } ->
      let actuals, (caller_am, caller_oct) =
        if List.(length actuals = length callee.formals) then (actuals, (caller_am, caller_oct))
        else arrayify_varargs callee actuals (List.length callee.formals) (caller_am, caller_oct)
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
      let callee_oct =
        let param_fields_oct =
          Map.data callee_am
          |> List.fold ~init:(Set.empty (module Addr)) ~f:Set.union
          |> project_fields caller_oct
        in
        let param_bindings =
          List.(filter_map (zip_exn callee.formals actuals)) ~f:(fun (f, a) ->
              texpr_of_expr caller_state a >>| Octagon.eval_texpr caller_oct
              >>| pair (Var.of_string f))
        in
        let params_oct =
          List.fold param_bindings ~init:param_fields_oct ~f:(fun oct (formal, bound) ->
              Octagon.assign oct formal Texpr1.(Cst (Coeff.Interval bound)))
        in
        let rcvr_field_bindings =
          if String.equal rcvr "this" then
            let ({ package; class_name; _ } : Method_id.t) = callee.method_id in
            Declared_fields.lookup ~package ~class_name fields |> fun { instance; _ } ->
            Set.fold instance ~init:[] ~f:(fun acc var_str ->
                let var = Var.of_string var_str in
                if Environment.mem_var (Abstract1.env caller_oct) var then
                  (var, Octagon.lookup caller_oct var) :: acc
                else acc)
          else
            let rcvr_aaddr = Map.find caller_am rcvr |> Option.value ~default:Addr.Abstract.empty in
            Abstract1.env caller_oct |> Environment.vars |> snd
            |> Array.fold ~init:[] ~f:(fun acc var ->
                   match String.chop_prefix (Var.to_string var) ~prefix:"__dai_alloc_" with
                   | Some var_str
                     when String.take_while var_str ~f:Char.is_digit
                          |> Int.of_string |> Addr.of_int |> Set.mem rcvr_aaddr ->
                       let fld_name =
                         String.(index_exn var_str '.' |> Int.succ |> drop_prefix var_str)
                       in
                       (Var.of_string fld_name, Octagon.lookup caller_oct var) :: acc
                   | _ -> acc)
        in
        List.fold ~init:params_oct rcvr_field_bindings ~f:(fun oct (var, value) ->
            Octagon.assign oct var Texpr1.(Cst (Coeff.Interval value)))
      in
      (callee_am, callee_oct)
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)

let return ~(callee : Cfg.Fn.t) ~(caller : Cfg.Fn.t) ~callsite
    ~caller_state:(caller_amap, caller_oct) ~return_state:(return_amap, return_oct) ~fields =
  forget_used_tmp_vars callsite
  @@
  match callsite with
  | Ast.Stmt.Call { lhs; rcvr; meth; alloc_site; actuals = _ } ->
      let man = Octagon.get_man () in
      (* [rcvr_aaddr] is one of:
         * [`This], indicating an absent or "this" receiver;
         * [`AAddr aaddr], indicating a receiver with abstract address [aaddr]; or
         * [`Static], indicating a static call
      *)
      let rcvr_aaddr =
        if callee.method_id.static then `Static
        else if String.equal rcvr "this" then `This
        else
          match alloc_site with
          | Some a -> `AAddr (Addr.Abstract.of_alloc_site a)
          | None -> (
              match Map.find caller_amap rcvr with Some aaddr -> `AAddr aaddr | None -> `None)
      in
      let callee_instance_fields =
        if callee.method_id.static then String.Set.empty
        else
          Declared_fields.lookup_instance fields ~package:callee.method_id.package
            ~class_name:callee.method_id.class_name
      in
      let retvar = Var.of_string Cfg.retvar in
      let return_val =
        if Environment.mem_var (Abstract1.env return_oct) retvar then
          Abstract1.bound_variable man return_oct retvar
        else Interval.top
      in
      (* (1) transfer constraints on the return value (and its fields) to the lhs of the callsite, then
         (2) transfer constraints on instance fields at the callee return to equivalent constraints on the receiver's fields
         (3) transfer constraints on static fields at the callee return to equivalents on the receiver, if they share a class
      *)
      let oct =
        (* (1) *)
        ((match lhs with
         | Some lhs ->
             Octagon.assign caller_oct (Var.of_string lhs) Texpr1.(Cst (Coeff.Interval return_val))
         | None -> caller_oct)
        |>
        match Map.find return_amap Cfg.retvar with
        | None -> Fn.id
        | Some retval_aaddr ->
            let retval_fields_oct = project_fields return_oct retval_aaddr in
            Octagon.meet retval_fields_oct)
        (* (2) *)
        |> fun oct ->
        Set.fold callee_instance_fields ~init:oct ~f:(fun oct fld ->
            let fld_var = Var.of_string fld in
            if Environment.mem_var (Abstract1.env return_oct) fld_var then
              let fld_val = Octagon.lookup return_oct fld_var in
              match rcvr_aaddr with
              | `This ->
                  Octagon.assign oct (Var.of_string fld) Texpr1.(Cst (Coeff.Interval fld_val))
              | `AAddr aaddr ->
                  Set.fold aaddr ~init:oct ~f:(fun oct addr ->
                      Octagon.assign oct (apron_var_of_field addr fld)
                        Texpr1.(Cst (Coeff.Interval fld_val)))
              | `Static | `None -> oct
            else oct)
        (* (3) *)
        |>
        if Cfg.Fn.is_same_class callee caller then fun oct ->
          let static_fields =
            Declared_fields.lookup_static fields ~package:callee.method_id.package
              ~class_name:callee.method_id.class_name
          in
          Set.fold static_fields ~init:oct ~f:(fun oct fld ->
              let fld_var = Var.of_string fld in
              let fld_val = Octagon.lookup return_oct fld_var in
              if Interval.is_top fld_val then oct
              else Octagon.assign oct fld_var Texpr1.(Cst (Coeff.Interval fld_val)))
        else Fn.id
      in
      (* (1) bind the receiver's abstract address if needed, then
         (2) transfer any return-value address binding to the callsite's lhs *)
      let amap =
        (* (1) *)
        (match rcvr_aaddr with
        | `This | `Static | `None -> caller_amap
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
      (amap, oct)
  | Ast.Stmt.Exceptional_call { rcvr; meth = _; actuals = _ } ->
      (* [rcvr_aaddr] is one of:
         * [`This], indicating an absent or "this" receiver;
         * [`AAddr aaddr], indicating a receiver with abstract address [aaddr]; or
         * [`Static], indicating a static call
      *)
      let rcvr_aaddr =
        if callee.method_id.static then `Static
        else if String.equal rcvr "this" then `This
        else match Map.find caller_amap rcvr with Some aaddr -> `AAddr aaddr | None -> `None
      in
      let callee_instance_fields =
        if callee.method_id.static then String.Set.empty
        else
          Declared_fields.lookup_instance fields ~package:callee.method_id.package
            ~class_name:callee.method_id.class_name
      in
      (* transfer any exceptional-return-value address binding*)
      let amap =
        match Map.find return_amap Cfg.exc_retvar with
        | Some aaddr -> Addr_map.set caller_amap ~key:Cfg.exc_retvar ~aaddr
        | _ -> caller_amap
      in
      let oct =
        Set.fold callee_instance_fields ~init:caller_oct ~f:(fun oct fld ->
            let fld_var = Var.of_string fld in
            if Environment.mem_var (Abstract1.env return_oct) fld_var then
              let fld_val = Octagon.lookup return_oct fld_var in
              match rcvr_aaddr with
              | `This ->
                  Octagon.assign oct (Var.of_string fld) Texpr1.(Cst (Coeff.Interval fld_val))
              | `AAddr aaddr ->
                  Set.fold aaddr ~init:oct ~f:(fun oct addr ->
                      Octagon.assign oct (apron_var_of_field addr fld)
                        Texpr1.(Cst (Coeff.Interval fld_val)))
              | `Static | `None -> oct
            else oct)
        (* (3) *)
        |>
        if Cfg.Fn.is_same_class callee caller then fun oct ->
          let static_fields =
            Declared_fields.lookup_static fields ~package:callee.method_id.package
              ~class_name:callee.method_id.class_name
          in
          Set.fold static_fields ~init:oct ~f:(fun oct fld ->
              let fld_var = Var.of_string fld in
              let fld_val = Octagon.lookup return_oct fld_var in
              if Interval.is_top fld_val then oct
              else Octagon.assign oct fld_var Texpr1.(Cst (Coeff.Interval fld_val)))
        else Fn.id
      in
      (amap, oct)
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)

let noop_library_methods : String.Set.t =
  String.Set.of_list [ "toString"; "hashCode"; "println"; "print"; "printf"; "log"; "debug" ]

let is_getter meth =
  match String.chop_prefix ~prefix:"get" meth with
  | Some fld when String.length fld > 0 && Char.is_uppercase (String.get fld 0) -> true
  | _ -> false

let is_setter meth =
  match String.chop_prefix ~prefix:"set" meth with
  | Some fld when String.length fld > 0 && Char.is_uppercase (String.get fld 0) -> true
  | _ -> false

let approximate_missing_callee ~caller_state ~callsite =
  (* as described in [interpret] above, forget used tmp vars after they are used *)
  forget_used_tmp_vars callsite
  @@
  match callsite with
  (* unknown constructor -- just bind the address and ignore the rest *)
  | Ast.Stmt.Call { lhs = Some lhs; rcvr = _; meth = _; actuals = _; alloc_site = Some a } ->
      let caller_am, caller_oct = caller_state in
      let am = Addr_map.set caller_am ~key:lhs ~aaddr:(Addr.Abstract.singleton a) in
      (am, caller_oct)
  (* unknown constructor, value being discarded -- no-op*)
  | Ast.Stmt.Call { lhs = None; alloc_site = Some _; _ } -> caller_state
  (* call to assumed-no-op library methods -- no-op*)
  | Ast.Stmt.Call { lhs = _; rcvr = _; meth; actuals = _; alloc_site = _ }
    when Set.mem noop_library_methods meth ->
      caller_state
  | Ast.Stmt.Call { lhs = Some lhs; rcvr; meth; actuals = []; alloc_site = None }
    when is_getter meth ->
      (* we (unsoundly) treat a call `x = o.getFoo()` method as equivalent to `x = o.foo` *)
      let field = String.uncapitalize (String.chop_prefix_exn meth ~prefix:"get") in
      interpret Ast.(Stmt.Assign { lhs; rhs = Expr.Deref { rcvr; field } }) caller_state
  | Ast.Stmt.Call { lhs = _; rcvr; meth; actuals = [ rhs ]; alloc_site = None } when is_setter meth
    ->
      (* we (unsoundly) assume that any call `o.setFoo(x)` method is equivalent to `o.foo = x` *)
      let field = String.uncapitalize (String.chop_prefix_exn meth ~prefix:"set") in
      interpret Ast.(Stmt.Write { rcvr; field; rhs }) caller_state
  | Ast.Stmt.Call { lhs; rcvr; meth = _; actuals; alloc_site = None } ->
      (* for a call to an unknown function `x = o.foo(y1,y2,,...)`, we forget any constraints on:
         (1) fields of rcvr o
         (2) fields of actuals y_i
         (3) address or value of lhs
         conservatively assuming that receiver and actual fields are modified by the call
      *)
      let caller_am, caller_oct = caller_state in
      let affected_vars =
        rcvr :: List.filter_map actuals ~f:(function Expr.Var v -> Some v | _ -> None)
      in
      let affected_addrs =
        Addr_map.get_addrs affected_vars caller_am
        |> List.map ~f:(Format.asprintf "__dai_%a" Addr.pp)
      in
      let am = Addr_map.forget (Option.to_list lhs) caller_am in
      let oct =
        let vars_to_forget =
          Abstract1.env caller_oct |> Environment.vars |> snd
          |> Array.filter
               ~f:
                 ( Var.to_string >> fun v ->
                   Option.exists lhs ~f:(String.equal v)
                   || List.exists affected_addrs ~f:(fun prefix -> String.is_prefix v ~prefix) )
        in
        Octagon.forget vars_to_forget caller_oct
      in
      (am, oct)
  | Ast.Stmt.Exceptional_call _ -> caller_state
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)

(** deferred to end of this file to avoid colliding with Dai.Import.(<=) *)
let ( <= ) = implies

let of_alist dims =
  let man = Octagon.get_man () in
  let vars = Array.of_list_map dims ~f:(fst3 >> Var.of_string) in
  let bindings = Array.of_list_map dims ~f:(fun (_, inf, sup) -> Interval.of_float inf sup) in
  let env = Environment.make [||] vars in
  (Addr_map.empty, Abstract1.of_box man env vars bindings)
