open Dai
open Import
open Syntax
open Ast
open Apron
(* open Null_val *)

(* We need an environment map from var names to null_val * (abstract.addr option)
 * We also want a heap as a map from abstract.addr * field name: string to null_val * (abstract.addr option) *)

module Env = struct
  type t = (Null_val.t * Addr.Abstract.t) Map.M(String).t [@@deriving compare, equal, hash, sexp]

  (* why specify module string? *)
  let empty = Map.empty (module String)

  let pp fs (env : t) =
    let pp_binding fs (k, (v, a)) =
      Format.fprintf fs "%s -> (%a, %a)" k Null_val.pp v Addr.Abstract.pp a
    in
    List.pp ~pre:"{" ~suf:"}" ";@ " pp_binding fs (Map.to_alist env)

  let _add ~key ~nullness ~addr env =
    let data =
      match Map.find env key with
      | Some (key_nullness, addrs) ->
          (Null_val.join nullness key_nullness, Addr.Abstract.add addrs addr)
      | None -> (nullness, Addr.Abstract.singleton addr)
    in
    Map.set ~key ~data env

  let set ~key ~nullness ~aaddr env = Map.set env ~key ~data:(nullness, aaddr)

  let get_addrs (vars : string list) (env : t) =
    let vars = String.Set.of_list vars in
    Map.filter_keys env ~f:(Set.mem vars) |> Map.data |> List.map ~f:snd |> List.bind ~f:Set.to_list

  let forget vars env = List.fold vars ~init:env ~f:Map.remove
end

module Heap = struct
  (* type t = (Null_val.t * Addr.Abstract.t) Map.M(Tuple.T2(Domain.Addr.t, string)).t *)
end

type t = Env.t option [@@deriving compare, equal, hash, sexp]

let hash = seeded_hash

let pp fs (env : t) =
  match env with
  | Some env -> Format.fprintf fs "%a" Env.pp env
  | None -> Format.print_string "bottom"

let show (x : t) =
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

let bottom () = None

let top () = Some Env.empty

let init = top

let is_bot = is_some >> not

(* keys s2 subset keys s1 
 * AND s1[k] implies s2[k] for all keys k in keys s2 *)
let implies s1 s2 =
  match (s1, s2) with
  | None, None -> true
  | None, Some _ -> true
  | Some _, None -> false
  | Some s1, Some s2 ->
      Map.for_alli s2 ~f:(fun ~key ~data ->
          Map.mem s1 key
          && Null_val.implies (fst (Map.find_exn s1 key)) (fst data)
          && Addr.Abstract.is_subset (Map.find_exn s1 key |> snd) ~of_:(snd data))

let ( <= ) = implies

let sanitize = Fn.id

let join s1 s2 =
  Option.merge s1 s2
    (Map.merge_skewed ~combine:(fun ~key:_ (v1, addr1) (v2, addr2) ->
         (Null_val.join v1 v2, Addr.Abstract.union addr1 addr2)))

let widen = join (* TODO(archerd): double check this, but I think it still applies *)

let _extend_env_by_uses _stmt phi = phi
(* TODO(archerd): I don't think this does anything for me here/yet. *)

let forget_vars vars phi =
  Option.map phi ~f:(fun env ->
      let vars_to_forget = Map.keys env |> List.filter ~f:(Set.mem vars) in
      List.fold ~init:env ~f:Map.remove vars_to_forget)

let forget_used_tmp_vars stmt state =
  let used_tmps = Ast.Stmt.uses stmt |> Set.filter ~f:(String.is_prefix ~prefix:"__dai_tmp") in
  forget_vars used_tmps state

let interpret = failwith "unimplemented" (* TODO(archerd): implement *)

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

let call ~(callee : Cfg.Fn.t) ~callsite ~caller_state ~fields:_ =
  (* let state = extend_env_by_uses callsite caller_state in *)
  match callsite with
  | Stmt.Call { rcvr = _; actuals; _ } | Stmt.Exceptional_call { rcvr = _; actuals; _ } ->
      let actuals, caller_state =
        if List.(length actuals = length callee.formals) then (actuals, caller_state)
        else arrayify_varargs callee actuals (List.length callee.formals) caller_state
      in
      (* re-scope the address map to include only the formal parameters *)
      let callee_state =
        Some
          (List.(fold (zip_exn callee.formals actuals)) ~init:Env.empty
             ~f:(fun env (formal, actual) ->
               match actual with
               | Ast.Expr.Var v ->
                   Option.fold
                     (Option.bind caller_state ~f:(flip Map.find v))
                     ~init:env
                     ~f:(fun env actual_state -> Map.add_exn env ~key:formal ~data:actual_state)
               | _ -> env))
      in
      callee_state
      (* TODO(archerd): doesn't handle passing receiver or formal fields, or heap(heap isn't modeled yet)
       * (see array_bounds.ml:393-429*)
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Stmt.pp s)

let return ~(callee : Cfg.Fn.t) ~(caller : Cfg.Fn.t) ~callsite ~(caller_state : t)
    ~(return_state : t) ~fields =
  forget_used_tmp_vars callsite
  @@
  (* TODO(archerd): fill out the two cases *)
  (* TODO(archerd): adjust default value or otherwise change this *)
  let caller_env = Option.value caller_state ~default:Env.empty in
  let return_env = Option.value return_state ~default:Env.empty in
  match callsite with
  | Ast.Stmt.Call { lhs; rcvr; meth; alloc_site; actuals = _ } ->
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
              match Map.find caller_env rcvr with Some (_, aaddr) -> `AAddr aaddr | None -> `None)
      in
      let _callee_instance_fields =
        if callee.method_id.static then String.Set.empty
        else
          Declared_fields.lookup_instance fields ~package:callee.method_id.package
            ~class_name:callee.method_id.class_name
      in
      let _retvar = Var.of_string Cfg.retvar in
      (* let return_val = ... (see array_bounds.ml:464-512*)
      (* (1) bind the receiver's abstract address if needed, then
         (2) transfer any return-value address binding to the callsite's lhs *)
      let env =
        (* (1) *)
        (match rcvr_aaddr with
        | `This | `Static | `None -> caller_env
        | `AAddr rcvr_aaddr ->
            if String.equal "<init>" meth && Option.is_some lhs then
              (* TODO(archerd): might be able to tighten the nullness value in one of these cases. *)
              Env.set caller_env ~key:(Option.value_exn lhs) ~nullness:Null_val.top
                ~aaddr:rcvr_aaddr
            else Env.set caller_env ~key:rcvr ~nullness:Null_val.top ~aaddr:rcvr_aaddr)
        (* (2) *)
        |>
        match Map.find return_env Cfg.retvar with
        | Some (retval_nullness, retval_aaddr) when Option.is_some lhs ->
            Env.set ~key:(Option.value_exn lhs) ~nullness:retval_nullness ~aaddr:retval_aaddr
        | _ -> Fn.id
      in
      Some env
  | Ast.Stmt.Exceptional_call { rcvr; meth = _; actuals = _ } ->
      (* [rcvr_aaddr] is one of:
         * [`This], indicating an absent or "this" receiver;
         * [`AAddr aaddr], indicating a receiver with abstract address [aaddr]; or
         * [`Static], indicating a static call
      *)
      let _rcvr_aaddr =
        if callee.method_id.static then `Static
        else if String.equal rcvr "this" then `This
        else match Map.find caller_env rcvr with Some aaddr -> `AAddr aaddr | None -> `None
      in
      let _callee_instance_fields =
        if callee.method_id.static then String.Set.empty
        else
          Declared_fields.lookup_instance fields ~package:callee.method_id.package
            ~class_name:callee.method_id.class_name
      in
      (* transfer any exceptional-return-value address binding*)
      let env =
        match Map.find return_env Cfg.exc_retvar with
        | Some (nullness, aaddr) -> Env.set caller_env ~key:Cfg.exc_retvar ~nullness ~aaddr
        | _ -> caller_env
      in
      (* let itv = *)
      (*   Set.fold _callee_instance_fields ~init:caller_itv ~f:(fun itv fld -> *)
      (*       let fld_var = Var.of_string fld in *)
      (*       if Environment.mem_var (Abstract1.env return_itv) fld_var then *)
      (*         let fld_val = Itv.lookup return_itv fld_var in *)
      (*         match rcvr_aaddr with *)
      (*         | `This -> Itv.assign itv (Var.of_string fld) Texpr1.(Cst (Coeff.Interval fld_val)) *)
      (*         | `AAddr aaddr -> *)
      (*             Set.fold aaddr ~init:itv ~f:(fun itv addr -> *)
      (*                 Itv.assign itv (apron_var_of_field addr fld) *)
      (*                   Texpr1.(Cst (Coeff.Interval fld_val))) *)
      (*         | `Static | `None -> itv *)
      (*       else itv) *)
      (*   (1* (3) *1) *)
      (*   |> *)
      (*   if Cfg.Fn.is_same_class callee caller then fun itv -> *)
      (*     let static_fields = *)
      (*       Declared_fields.lookup_static fields ~package:callee.method_id.package *)
      (*         ~class_name:callee.method_id.class_name *)
      (*     in *)
      (*     Set.fold static_fields ~init:itv ~f:(fun itv fld -> *)
      (*         let fld_var = Var.of_string fld in *)
      (*         let fld_val = Itv.lookup return_itv fld_var in *)
      (*         if Interval.is_top fld_val then itv *)
      (*         else Itv.assign itv fld_var Texpr1.(Cst (Coeff.Interval fld_val))) *)
      (*   else Fn.id *)
      (* in *)
      Some env
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)

let noop_library_methods : String.Set.t =
  String.Set.of_list [ "toString"; "hashCode"; "println"; "print"; "printf"; "log"; "debug" ]

let _is_getter meth =
  match String.chop_prefix ~prefix:"get" meth with
  | Some fld when String.length fld > 0 && Char.is_uppercase (String.get fld 0) -> true
  | _ -> false

let _is_setter meth =
  match String.chop_prefix ~prefix:"set" meth with
  | Some fld when String.length fld > 0 && Char.is_uppercase (String.get fld 0) -> true
  | _ -> false

let approximate_missing_callee ~caller_state ~callsite =
  forget_used_tmp_vars callsite
  @@
  match callsite with
  (* unknown constructor -- just bind the address and ignore the rest *)
  | Ast.Stmt.Call { lhs = Some lhs; rcvr = _; meth = _; actuals = _; alloc_site = Some a } ->
      let caller_env = caller_state in
      let env =
        Env.set
          (Option.value caller_env ~default:Env.empty)
          ~key:lhs ~nullness:Null_val.not_null ~aaddr:(Addr.Abstract.singleton a)
      in
      Some env
  (* unknown constructor, value being discarded -- no-op*)
  | Ast.Stmt.Call { lhs = None; alloc_site = Some _; _ } -> caller_state
  (* call to assumed-no-op library methods -- no-op*)
  | Ast.Stmt.Call { lhs = _; rcvr = _; meth; actuals = _; alloc_site = _ }
    when Set.mem noop_library_methods meth ->
      caller_state
  | Ast.Stmt.Call { lhs; rcvr; meth = _; actuals; alloc_site = None } ->
      (* for a call to an unknown function `x = o.foo(y1,y2,,...)`, we forget any constraints on:
         (1) fields of rcvr o           [not done (no heap)]
         (2) fields of actuals y_i      [not done (no heap)]
         (3) address or value of lhs    [done]
         conservatively assuming that receiver and actual fields are modified by the call
      *)
      Option.map caller_state ~f:(function caller_env ->
          let affected_vars =
            rcvr :: List.filter_map actuals ~f:(function Expr.Var v -> Some v | _ -> None)
          in
          let _affected_addrs =
            Env.get_addrs affected_vars caller_env
            (* |> List.map ~f:(Format.asprintf "__dai_%a" Addr.pp) *)
          in
          let env = Env.forget (Option.to_list lhs) caller_env in
          (* let itv = *)
          (*   let vars_to_forget = *)
          (*     Abstract1.env caller_itv |> Environment.vars |> snd *)
          (*     |> Array.filter *)
          (*          ~f: *)
          (*            ( Var.to_string >> fun v -> *)
          (*              Option.exists lhs ~f:(String.equal v) *)
          (*              || List.exists affected_addrs ~f:(fun prefix -> String.is_prefix v ~prefix) ) *)
          (*   in *)
          (*   Itv.forget vars_to_forget caller_itv *)
          (* in *)
          env)
  (* TODO(archerd): implement more Ast.Stmt.Call cases *)
  | Ast.Stmt.Exceptional_call _ -> caller_state
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)
