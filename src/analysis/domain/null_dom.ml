open Dai
open Import
open Syntax
open Ast

(* open Apron *)
(* open Null_val *)

(* We need an environment map from var names to null_val * (abstract.addr option)
 * We also want a heap as a map from abstract.addr * field name: string to null_val * (abstract.addr option) *)

(* Consider abstracting out null_val * abstract.addr out to a class *)

module Env = struct
  type t = (Null_val.t * Addr.Abstract.t) Map.M(String).t [@@deriving compare, equal, hash, sexp]

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

  let find (env : t) var = Map.find env var
end

module Addr_field = struct
  type t = Addr.t * String.t [@@deriving compare, equal, sexp, hash]

  let pp fs (addr, field) = Format.fprintf fs "(%a, %s)" Addr.pp addr field
end

module Addr_field_with_comparator = struct
  include Addr_field
  include Base.Comparator.Make (Addr_field)
end

(* TODO(archerd): add full heap support *)
module Heap = struct
  (* TODO(archerd): There must be a nicer way to define this type. *)
  type t = (Null_val.t * Addr.Abstract.t) Map.M(Addr_field_with_comparator).t
  [@@deriving compare, equal, hash, sexp]

  let pp fs (heap : t) =
    let pp_binding fs (k, (v, a)) =
      Format.fprintf fs "%a -> (%a, %a)" Addr_field.pp k Null_val.pp v Addr.Abstract.pp a
    in
    List.pp ~pre:"{" ~suf:"}" ";@ " pp_binding fs (Map.to_alist heap)

  let empty = Map.empty (module Addr_field_with_comparator)

  let find (heap : t) addr field = Map.find heap (addr, field)
end

type t = (Env.t * Heap.t) option [@@deriving compare, equal, hash, sexp]
(* type t = Env.t option [@@deriving compare, equal, hash, sexp] *)

let hash = seeded_hash

let pp fs (state : t) =
  match state with
  | Some (env, heap) -> Format.fprintf fs "%a | %a" Env.pp env Heap.pp heap
  | None -> Format.fprintf fs "bottom"

let show (x : t) =
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

let bottom () = None

let top () = Some (Env.empty, Heap.empty)

let init = top

let is_bot = is_some >> not

(* keys s2 subset keys s1 
 * AND s1[k] implies s2[k] for all keys k in keys s2 *)
let implies (s1 : t) (s2 : t) =
  match (s1, s2) with
  | None, None -> true
  | None, Some _ -> true
  | Some _, None -> false
  | Some (env1, heap1), Some (env2, heap2) ->
      Map.for_alli env2 ~f:(fun ~key ~data ->
          Map.mem env1 key
          && Null_val.implies (fst (Map.find_exn env1 key)) (fst data)
          && Addr.Abstract.is_subset (Map.find_exn env1 key |> snd) ~of_:(snd data))
      && Map.for_alli heap2 ~f:(fun ~key ~data ->
             Map.mem heap1 key
             && Null_val.implies (fst (Map.find_exn heap1 key)) (fst data)
             && Addr.Abstract.is_subset (Map.find_exn heap1 key |> snd) ~of_:(snd data))

let ( <= ) = implies

let sanitize = Fn.id

let join (s1 : t) (s2 : t) =
  Option.merge s1 s2 (fun (env1, heap1) (env2, heap2) ->
      ( Map.merge_skewed env1 env2 ~combine:(fun ~key:_ (v1, addr1) (v2, addr2) ->
            (Null_val.join v1 v2, Addr.Abstract.union addr1 addr2)),
        Map.merge_skewed heap1 heap2 ~combine:(fun ~key:_ (v1, addr1) (v2, addr2) ->
            (Null_val.join v1 v2, Addr.Abstract.union addr1 addr2)) ))

let widen = join

let forget_vars vars (phi : t) : t =
  Option.map phi ~f:(fun (env, heap) ->
      (* TODO(archerd): update to reflect heap. *)
      let vars_to_forget = Map.keys env |> List.filter ~f:(Set.mem vars) in
      (Env.forget vars_to_forget env, heap))

let forget_addrs (heap : Heap.t) addrs =
  Map.filter_keys heap ~f:(fun (addr, _) -> List.mem addrs addr ~equal:( = ))

let forget_used_tmp_vars stmt (state : t) : t =
  let used_tmps = Ast.Stmt.uses stmt |> Set.filter ~f:(String.is_prefix ~prefix:"__dai_tmp") in
  forget_vars used_tmps state

(* TODO(archerd): something feels off with the handling of the abstract addrs. *)
let rec eval_expr (state : t) : Ast.Expr.t -> Null_val.t * Addr.Abstract.t =
  let env, heap =
    Option.value state ~default:(Env.empty, Heap.empty)
    (* TODO(archerd): double check this default *)
  in
  let open Ast in
  (* TODO(archerd): update to reflect heap. *)
  function
  | Expr.Var v -> (
      match Env.find env v with
      | Some value -> value
      | None -> (Null_val.of_lit Lit.Null, Addr.Abstract.empty))
  | Expr.Lit l -> (Null_val.of_lit l, Addr.Abstract.empty)
  | Expr.Binop { l; op; r } ->
      let vl, aaddrl = eval_expr state l in
      let vr, aaddrr = eval_expr state r in
      (Null_val.eval_binop vl op vr, Addr.Abstract.union aaddrl aaddrr)
  | Expr.Unop { op; e } ->
      let v, aaddr = eval_expr state e in
      (Null_val.eval_unop op v, aaddr)
  | Expr.Deref { rcvr; field } ->
      let rcvr_state = Env.find env rcvr in
      (* TODO(archerd): check the state of the obj? update it to NotNull (this would require changing the signiture of the function)? *)
      let field_states =
        Option.value_map rcvr_state ~default:[] ~f:(function _, aaddr ->
            aaddr |> Set.to_list
            |> List.map ~f:(fun addr -> Heap.find heap addr field)
            |> List.filter_opt)
      in
      let common_state =
        List.fold field_states ~init:(Null_val.top, Addr.Abstract.empty)
          ~f:(fun (null_acc, aaddr_acc) (null_new, aaddr_new) ->
            (Null_val.join null_acc null_new, Addr.Abstract.union aaddr_acc aaddr_new))
      in
      common_state
  | Expr.Array_access _ | Expr.Array_literal _ | Expr.Array_create _ | Expr.Method_ref _
  | Expr.Class_lit _ ->
      (Null_val.top, Addr.Abstract.empty)
(* failwith "expression not handled by this basic environment functor" *)

let weak_update (heap : Heap.t) aaddr field (new_nullness, new_aaddr) =
  Map.fold
    ~f:(fun ~key:(addr, f) ~data:(data_nullness, data_aaddr) acc ->
      if Addr.Abstract.mem aaddr addr && String.equal field f then
        Map.set acc ~key:(addr, f)
          ~data:(Null_val.join data_nullness new_nullness, Addr.Abstract.union data_aaddr new_aaddr)
      else Map.add_exn acc ~key:(addr, f) ~data:(data_nullness, data_aaddr))
    ~init:Heap.empty heap

let _strong_update (heap : Heap.t) aaddr field (new_nullness, new_aaddr) =
  Map.fold
    ~f:(fun ~key:(addr, f) ~data:(data_nullness, data_aaddr) acc ->
      if Addr.Abstract.mem aaddr addr && String.equal field f then
        Map.set acc ~key:(addr, f) ~data:(new_nullness, new_aaddr)
      else Map.add_exn acc ~key:(addr, f) ~data:(data_nullness, data_aaddr))
    ~init:Heap.empty heap

let interpret stmt (phi : t) : t =
  Option.bind phi ~f:(fun (env, heap) ->
      forget_used_tmp_vars stmt
      @@
      (* TODO(archerd): implement different stmt cases *)
      (* TODO(archerd): update to reflect heap. *)
      match stmt with
      | Assign { lhs; rhs = Expr.Var v } when Map.mem env v ->
          let () = print_string ("assigning var to " ^ lhs ^ "\n") in
          Some (Map.set env ~key:lhs ~data:(Map.find_exn env v), heap)
      | Assign { lhs; rhs } ->
          let () = print_string ("assigning expr to " ^ lhs ^ "\n") in
          let nullness, aaddr = eval_expr phi rhs in
          Some (Env.set env ~key:lhs ~nullness ~aaddr, heap)
      | Assume (Binop { l; op = Binop.NEq; r = Expr.Lit Lit.Null }) ->
          if Null_val.is_null_or_bot (fst (eval_expr phi l)) then None else Some (env, heap)
      | Assume e -> (
          match Null_val.truthiness (fst (eval_expr phi e)) with
          | `T | `Either -> Some (env, heap)
          | `F | `Neither -> None)
      | Skip -> Some (env, heap)
      | Expr _ -> Some (env, heap)
      | Write { rcvr; field; rhs } ->
          let () = print_string ("writing with rcvr " ^ rcvr ^ "\n") in
          let nullness, aaddr = eval_expr phi rhs in
          let rcvr_state = Env.find env rcvr in
          let new_heap =
            match rcvr_state with
            | None ->
                let () = print_string "write with rcvr_state = None\n" in
                heap
            | Some (_, rcvr_aaddr) -> weak_update heap rcvr_aaddr field (nullness, aaddr)
            (* TODO(archerd): check the state of the obj? update it to NotNull? *)
          in
          Some (env, new_heap)
          (* TODO(archerd): revisit, there should be something we can do here (write ensures nonnull) *)
      | Call _ -> failwith "should be handled by the call function?"
      | _ -> failwith "unimplemented")

let arrayify_varargs (callee : Cfg.Fn.t) actuals formals (phi : t) : Expr.t list * t =
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

let call ~(callee : Cfg.Fn.t) ~callsite ~(caller_state : t) ~fields : t =
  Option.bind caller_state ~f:(fun (caller_env, heap) ->
      match callsite with
      | Stmt.Call { rcvr; actuals; _ } | Stmt.Exceptional_call { rcvr; actuals; _ } ->
          let rcvr_nullness =
            Option.value_map (Env.find caller_env rcvr) ~default:Null_val.top ~f:fst
          in
          if Null_val.is_null_or_bot rcvr_nullness then bottom ()
          else
            let actuals, caller_state =
              if List.(length actuals = length callee.formals) then (actuals, caller_state)
              else arrayify_varargs callee actuals (List.length callee.formals) caller_state
            in
            (* re-scope the env to include only the formal parameters and rebind receiver fields to callee-local variables *)
            let rcvr_field_bindings =
              if String.equal rcvr "this" then
                let ({ package; class_name; _ } : Method_id.t) = callee.method_id in
                (* failwith "unimplemented: function call on this" *)
                (* TODO(archerd): handle this case *)
                Declared_fields.lookup ~package ~class_name fields |> fun { instance; _ } ->
                List.filter_map (Set.to_list instance) ~f:(fun field ->
                    Option.map (Env.find caller_env field) ~f:(fun data -> (field, data)))
              else
                let rcvr_aaddr =
                  Map.find caller_env rcvr |> Option.value_map ~default:Addr.Abstract.empty ~f:snd
                in
                Map.filter_keys heap ~f:(fun (addr, _) -> Set.mem rcvr_aaddr addr)
                |> Map.to_alist
                |> List.map ~f:(fun ((_, field), data) -> (field, data))
            in
            let callee_env =
              List.(
                fold (zip_exn callee.formals actuals) ~init:Env.empty
                  ~f:(fun env (formal, actual) ->
                    match actual with
                    | Ast.Expr.Var v ->
                        (* TODO(archerd): Might be able to collapse this case into the second case... *)
                        Option.fold (Map.find caller_env v) ~init:env ~f:(fun env actual_state ->
                            Map.add_exn env ~key:formal ~data:actual_state)
                    | _ ->
                        let nullness, aaddr = eval_expr caller_state actual in
                        Env.set env ~key:formal ~nullness ~aaddr))
              |> fun env ->
              List.fold rcvr_field_bindings ~init:env ~f:(fun acc (field, (nullness, aaddr)) ->
                  Env.set acc ~key:field ~nullness ~aaddr)
            in
            (* project the caller heap down to the fields of formal parameters *)
            let param_fields_addrs =
              Map.data callee_env |> List.map ~f:snd
              |> List.fold ~init:(Set.empty (module Addr)) ~f:Set.union
            in
            let param_fields_heap =
              Map.filter_keys heap ~f:(fun (addr, _) -> Set.mem param_fields_addrs addr)
            in
            Some (callee_env, param_fields_heap)
      | s -> failwith (Format.asprintf "error: %a is not a callsite" Stmt.pp s))

let return ~(callee : Cfg.Fn.t) ~(caller : Cfg.Fn.t) ~callsite ~(caller_state : t)
    ~(return_state : t) ~fields : t =
  forget_used_tmp_vars callsite
  @@ Option.bind caller_state ~f:(fun (caller_env, caller_heap) ->
         Option.map return_state ~f:(fun (return_env, return_heap) ->
             (* TODO(archerd): update to reflect heap. *)
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
                         match Map.find caller_env rcvr with
                         | Some (_, aaddr) -> `AAddr aaddr
                         | None -> `None)
                 in
                 let callee_instance_fields =
                   if callee.method_id.static then String.Set.empty
                   else
                     Declared_fields.lookup_instance fields ~package:callee.method_id.package
                       ~class_name:callee.method_id.class_name
                 in
                 let heap =
                   (* transfer the return value fields *)
                   (match Map.find return_env Cfg.retvar with
                   | None -> caller_heap
                   | Some (_, retval_aaddr) ->
                       let retval_fields_heap =
                         Map.filter_keys return_heap ~f:(fun (addr, _) -> Set.mem retval_aaddr addr)
                       in
                       Map.merge_skewed caller_heap retval_fields_heap
                         ~combine:(fun ~key:_ _ (n2, aaddr2) -> (n2, aaddr2)))
                   (* transfer the instance fields to the receiver's fields *)
                   |> fun heap ->
                   Set.fold callee_instance_fields ~init:heap ~f:(fun heap fld ->
                       let fld_val = Env.find return_env fld in
                       Option.value_map fld_val ~default:heap ~f:(fun (fld_nullness, fld_aaddr) ->
                           match rcvr_aaddr with
                           | `This ->
                               heap
                               (* TOOD(archerd): in this case, I think I need to modify the environment, not the heap *)
                           | `AAddr aaddr ->
                               let () = print_string ("adding fld " ^ fld ^ " to heap\n") in
                               Set.fold aaddr ~init:heap ~f:(fun heap addr ->
                                   Map.set heap ~key:(addr, fld) ~data:(fld_nullness, fld_aaddr))
                           | `Static | `None -> heap))
                 in
                 (* (1) bind the receiver's abstract address if needed
                        and the instance fields for a this receiver, then
                    (2) transfer any return-value address binding to the callsite's lhs *)
                 let env =
                   (* (1) *)
                   (match rcvr_aaddr with
                   | `This ->
                       Set.fold callee_instance_fields ~init:caller_env ~f:(fun env fld ->
                           let fld_val = Env.find return_env fld in
                           Option.value_map fld_val ~default:env
                             ~f:(fun (fld_nullness, fld_aaddr) ->
                               Env.set env ~key:fld ~nullness:fld_nullness ~aaddr:fld_aaddr))
                   | `Static | `None -> caller_env
                   | `AAddr rcvr_aaddr ->
                       if String.equal "<init>" meth && Option.is_some lhs then
                         Env.set caller_env ~key:(Option.value_exn lhs) ~nullness:Null_val.not_null
                           ~aaddr:rcvr_aaddr
                       else
                         Env.set caller_env ~key:rcvr ~nullness:Null_val.not_null ~aaddr:rcvr_aaddr)
                   (* (2) *)
                   |> (match Map.find return_env Cfg.retvar with
                      | Some (retval_nullness, retval_aaddr) when Option.is_some lhs ->
                          Env.set ~key:(Option.value_exn lhs) ~nullness:retval_nullness
                            ~aaddr:retval_aaddr
                      | _ -> Fn.id)
                   (* transfer the static fields to the receiver if they share a class *)
                   |>
                   if Cfg.Fn.is_same_class callee caller then fun env ->
                     let static_fields =
                       Declared_fields.lookup_static fields ~package:callee.method_id.package
                         ~class_name:callee.method_id.class_name
                     in
                     Set.fold static_fields ~init:env ~f:(fun env fld ->
                         let fld_data = Env.find return_env fld in
                         Option.fold fld_data ~init:env ~f:(fun env (nullness, aaddr) ->
                             Env.set env ~key:fld ~nullness ~aaddr))
                   else Fn.id
                 in
                 (env, heap)
             | Ast.Stmt.Exceptional_call { rcvr; meth = _; actuals = _ } ->
                 (* TODO(archerd): UPDATE EXCEPTIONAL CALLS case *)
                 (* [rcvr_aaddr] is one of:
                    * [`This], indicating an absent or "this" receiver;
                    * [`AAddr aaddr], indicating a receiver with abstract address [aaddr]; or
                    * [`Static], indicating a static call
                 *)
                 let _rcvr_aaddr =
                   if callee.method_id.static then `Static
                   else if String.equal rcvr "this" then `This
                   else
                     match Map.find caller_env rcvr with
                     | Some aaddr -> `AAddr aaddr
                     | None -> `None
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
                   | Some (nullness, aaddr) ->
                       Env.set caller_env ~key:Cfg.exc_retvar ~nullness ~aaddr
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
                 (* TODO(archerd): update to reflect heap, this is an (bad) approximation. *)
                 (env, caller_heap)
             | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)))

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

let approximate_missing_callee ~(caller_state : t) ~callsite : t =
  forget_used_tmp_vars callsite
  @@ Option.bind caller_state ~f:(fun (caller_env, caller_heap) ->
         (* TODO(archerd): update to reflect heap. *)
         match callsite with
         (* unknown constructor -- just bind the address and ignore the rest *)
         | Ast.Stmt.Call { lhs = Some lhs; rcvr = _; meth = _; actuals = _; alloc_site = Some a } ->
             let env =
               Env.set caller_env ~key:lhs ~nullness:Null_val.not_null
                 ~aaddr:(Addr.Abstract.singleton a)
             in
             Some (env, caller_heap)
         (* unknown constructor, value being discarded -- no-op*)
         | Ast.Stmt.Call { lhs = None; alloc_site = Some _; _ } -> caller_state
         (* call to assumed-no-op library methods -- no-op*)
         | Ast.Stmt.Call { lhs = _; rcvr = _; meth; actuals = _; alloc_site = _ }
           when Set.mem noop_library_methods meth ->
             caller_state
         | Ast.Stmt.Call { lhs; rcvr; meth; actuals; alloc_site = None } ->
             let () = Printf.printf "approximating method %s\n" meth in
             (* for a call to an unknown function `x = o.foo(y1,y2,,...)`, we forget any constraints on:
                (1) fields of rcvr o [work needed]
                (2) fields of actuals y_i
                (3) address or value of lhs
                conservatively assuming that receiver and actual fields are modified by the call
             *)
             let affected_vars =
               List.filter_map actuals ~f:(function Expr.Var v -> Some v | _ -> None)
               |> if String.equal rcvr "this" then Fn.id else fun l -> rcvr :: l
             in
             let affected_addrs =
               Env.get_addrs affected_vars caller_env
               |> List.append
                    (List.concat
                       (List.filter_map actuals ~f:(function
                         | Expr.Var _ -> None
                         | expr -> Some (Set.to_list @@ snd @@ (eval_expr caller_state) expr))))
             in

             let env =
               Env.forget (Option.to_list lhs) caller_env
               |>
               if String.equal rcvr "this" then Env.forget []
                 (* TODO(archerd): should forget the fields of the current object,
                  * which are stored as local variables,
                  * so the fields need to be retrieved so they can be deleted. *)
               else Fn.id
             in
             let heap = forget_addrs caller_heap affected_addrs in
             Some (env, heap)
         (* TODO(archerd): implement more Ast.Stmt.Call cases *)
         | Ast.Stmt.Exceptional_call _ -> caller_state
         | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s))
