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

  let join_at_key ~key ~nullness ~aaddr env =
    Map.update env key
      ~f:
        (Option.value_map ~default:(nullness, aaddr) ~f:(fun (k_nullness, k_aaddr) ->
             (Null_val.join nullness k_nullness, Addr.Abstract.union aaddr k_aaddr)))

  let get_addrs (vars : string list) (env : t) =
    let vars = String.Set.of_list vars in
    Map.filter_keys env ~f:(Set.mem vars) |> Map.data |> List.map ~f:snd |> List.bind ~f:Set.to_list

  let forget vars env = List.fold vars ~init:env ~f:Map.remove

  let vars_to_top vars env =
    List.fold vars ~init:env ~f:(fun env var ->
        set ~key:var ~nullness:Null_val.Top ~aaddr:Addr.Abstract.empty env)

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

  let join_at_key ~key ~nullness ~aaddr (heap : t) =
    Map.update heap key
      ~f:
        (Option.value_map ~default:(nullness, aaddr) ~f:(fun (k_nullness, k_aaddr) ->
             (Null_val.join nullness k_nullness, Addr.Abstract.union aaddr k_aaddr)))

  let addrs_of_map =
    Map.fold ~init:Addr.Set.empty ~f:(fun ~key:_ ~data:(_, addrs) -> Set.union addrs)

  let rec reachable_addrs roots heap =
    if Map.is_empty heap || Set.is_empty roots then roots
    else
      let rooted, unexplored =
        Map.partition_mapi heap ~f:(fun ~key:(a, _) ~data ->
            if Set.mem roots a then First data else Second data)
      in
      Set.union roots (reachable_addrs (addrs_of_map rooted) unexplored)

  (** project out the subheap reachable from env *)
  let project (heap : t) (env : Env.t) : t =
    let footprint = reachable_addrs (addrs_of_map env) heap in
    Map.filter_keys heap ~f:(fst >> Set.mem footprint)

  let reachable_to_top addrs (heap : t) =
    let footprint = reachable_addrs addrs heap in
    Map.mapi heap ~f:(fun ~key:(addr, _) ~data ->
        if Set.mem footprint addr then (Null_val.Top, Addr.Abstract.empty) else data)
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

let _forget_addrs (heap : Heap.t) addrs =
  Map.filter_keys heap ~f:(fun (addr, _) -> List.mem addrs addr ~equal:( = ))

let forget_used_tmp_vars stmt (state : t) : t =
  let used_tmps = Ast.Stmt.uses stmt |> Set.filter ~f:(String.is_prefix ~prefix:"__dai_tmp") in
  forget_vars used_tmps state

let rec eval_expr (state : t) : Ast.Expr.t -> t * (Null_val.t * Addr.Abstract.t) =
  let env, heap =
    Option.value_exn state
    (* ~default:(Env.empty, Heap.empty) *)
    (* TODO(archerd): double check this default, it might not apply anymore *)
  in
  let open Ast in
  function
  | Expr.Var v -> (
      (* Format.printf "accessing variable %s. " v; *)
      let v_value = Env.find env v in
      match (v_value, Method_id.get_current_method_id ()) with
      | None, Some method_id ->
          (* Format.printf "variable not found in environment.\n"; *)
          let ({ package; class_name; _ } : Method_id.t) = method_id in
          let instance_fields =
            Declared_fields.lookup ~package ~class_name (Declared_fields.get_current_fields ())
            |> fun { instance; _ } -> instance
          in
          (* TODO(archerd): Still not perfect, for example if the field is shadowed but not in the environment yet *)
          if (not method_id.static) && Set.mem instance_fields v then
            eval_expr state (Ast.Expr.Deref { rcvr = "this"; field = v })
          else (state, (Null_val.Top, Addr.Abstract.empty))
      | _, None -> failwith "no method id set in eval_expr"
      | Some value, _ -> (* Format.printf "variable found in environment.\n";*) (state, value))
  | Expr.Lit l -> (state, (Null_val.of_lit l, Addr.Abstract.empty))
  | Expr.Binop { l; op; r } ->
      let state', (vl, aaddrl) = eval_expr state l in
      let state'', (vr, aaddrr) = eval_expr state' r in
      (state'', (Null_val.eval_binop vl op vr, Addr.Abstract.union aaddrl aaddrr))
  | Expr.Unop { op; e } ->
      let state', (v, aaddr) = eval_expr state e in
      (state', (Null_val.eval_unop op v, aaddr))
  | Expr.Deref { rcvr; field } ->
      (* Format.printf "accessing field %s.%s\n" rcvr field; *)
      let rcvr_state = Env.find env rcvr in
      (* TODO(archerd): check the state of the obj? update it to NotNull (this would require changing the signiture of the function)? *)
      (* TODO(archerd): the handling of this receivers assumes that the field is not shadowed by a local variable/argument,
       * which may not always be the case *)
      (* if String.equal rcvr "this" then eval_expr state (Expr.Var field) *)
      (* else *)
      let field_states =
        Option.value_map rcvr_state ~default:[] ~f:(function _, aaddr ->
            aaddr |> Set.to_list
            |> List.map ~f:(fun addr -> Heap.find heap addr field)
            |> List.filter_opt)
      in
      let common_value =
        List.reduce field_states ~f:(fun (null_acc, aaddr_acc) (null_new, aaddr_new) ->
            (Null_val.join null_acc null_new, Addr.Abstract.union aaddr_acc aaddr_new))
      in
      let state' =
        match rcvr_state with
        | None ->
            (* Format.printf "variable lookup for dereference failed: %s.%s\n" rcvr field; *)
            Some (Env.set ~key:rcvr ~nullness:Null_val.NotNull ~aaddr:Addr.Abstract.empty env, heap)
        | Some (Null_val.NotNull, aaddr) | Some (Null_val.Top, aaddr) ->
            Some (Env.set ~key:rcvr ~nullness:Null_val.NotNull ~aaddr env, heap)
        | Some (Null_val.Bot, _) | Some (Null_val.Null, _) -> None
      in
      (state', Option.value common_value ~default:(Null_val.Top, Addr.Abstract.empty))
  | Expr.Array_access _ | Expr.Array_literal _ | Expr.Array_create _ | Expr.Method_ref _
  | Expr.Class_lit _ ->
      (state, (Null_val.Top, Addr.Abstract.empty))
(* failwith "expression not handled by this basic environment functor" *)

let weak_update (heap : Heap.t) update_aaddr field (nullness, aaddr) =
  Set.fold update_aaddr ~init:heap ~f:(fun heap addr ->
      Heap.join_at_key ~key:(addr, field) ~nullness ~aaddr heap)

(* TODO(archerd): this function is probably wrong, do not use *)
let _strong_update_do_not_use (heap : Heap.t) aaddr field (new_nullness, new_aaddr) =
  Map.fold
    ~f:(fun ~key:(addr, f) ~data:(data_nullness, data_aaddr) acc ->
      if Addr.Abstract.mem aaddr addr && String.equal field f then
        Map.set acc ~key:(addr, f) ~data:(new_nullness, new_aaddr)
      else Map.add_exn acc ~key:(addr, f) ~data:(data_nullness, data_aaddr))
    ~init:Heap.empty heap

let rec interpret stmt (phi : t) : t =
  (* Format.printf "interpreting %a\n" Stmt.pp stmt; *)
  Option.bind phi ~f:(fun (env, heap) ->
      forget_used_tmp_vars stmt
      @@
      match stmt with
      (* | Assign { lhs; rhs = Expr.Var v } when Map.mem env v -> *)
      (*     Some (Map.set env ~key:lhs ~data:(Map.find_exn env v), heap) *)
      | Assign { lhs; rhs } -> (
          (* Format.printf "Assigning to %s. " lhs; *)
          let lhs_value = Env.find env lhs in
          match (lhs_value, Method_id.get_current_method_id ()) with
          | None, Some method_id ->
              (* Format.printf "variable not found in method %a (static=%b). " Method_id.pp method_id *)
              (*   method_id.static; *)
              let ({ package; class_name; _ } : Method_id.t) = method_id in
              let instance_fields =
                Declared_fields.lookup ~package ~class_name (Declared_fields.get_current_fields ())
                |> fun { instance; _ } -> instance
              in
              (* TODO(archerd): Still not perfect, for example if the field is shadowed but the corresponding local is not in the environment yet (this check is probably fine for all accesses, but the write needs work so the things get added to the environments) *)
              if (not method_id.static) && Set.mem instance_fields lhs then
                (* Format.printf "treating as field of this.\n"; *)
                interpret (Write { rcvr = "this"; field = lhs; rhs }) phi
              else
                (* Format.printf "treating as new local variable.\n"; *)
                let phi', (nullness, aaddr) = eval_expr phi rhs in
                Option.map phi' ~f:(fun (env', heap') ->
                    (Env.join_at_key env' ~key:lhs ~nullness ~aaddr, heap'))
          | _, None ->
              (* Format.printf "interpret assign %s = %a\n" lhs Expr.pp rhs; *)
              failwith "no method id set in interpret"
          | Some _, _ ->
              (* Format.printf "variable found\n"; *)
              let phi', (nullness, aaddr) = eval_expr phi rhs in
              Option.map phi' ~f:(fun (env', heap') ->
                  (Env.join_at_key env' ~key:lhs ~nullness ~aaddr, heap')))
      | Assume (Binop { l; op = Binop.NEq; r = Expr.Lit Lit.Null }) ->
          (* assume l != null *)
          if Null_val.is_null_or_bot (fst @@ snd (eval_expr phi l)) then None else Some (env, heap)
      | Assume (Unop { op = Unop.Not; e = Binop { l; op = Binop.Eq; r = Expr.Lit Lit.Null } }) ->
          (* assume !(l == null) *)
          if Null_val.is_null_or_bot (fst @@ snd (eval_expr phi l)) then None else Some (env, heap)
      | Assume (Binop { l; op = Binop.Eq; r = Expr.Lit Lit.Null }) ->
          (* assume l == null *)
          if Null_val.is_null_or_top (fst @@ snd (eval_expr phi l)) then Some (env, heap) else None
      | Assume (Unop { op = Unop.Not; e = Binop { l; op = Binop.NEq; r = Expr.Lit Lit.Null } }) ->
          (* assume !(l != null) *)
          if Null_val.is_null_or_top (fst @@ snd (eval_expr phi l)) then Some (env, heap) else None
      | Assume e -> (
          let phi', e_value = eval_expr phi e in
          match Null_val.truthiness (fst e_value) with
          | `T | `Either -> phi'
          | `F | `Neither -> None)
      | Skip -> phi
      | Expr e -> fst (eval_expr phi e)
      | Write { rcvr; field; rhs } ->
          (* if *)
          (*   String.equal rcvr "this" *)
          (*   (1* TODO(archerd): the handling of this receivers assumes that the field is not shadowed by a local variable, *)
          (*    * which may not always be the case *1) *)
          (* then interpret (Stmt.Assign { lhs = field; rhs }) phi *)
          (* else *)
          (* Format.printf "writing to %s.%s\n" rcvr field; *)
          let rcvr_state = Env.find env rcvr in
          let phi' =
            match rcvr_state with
            | Some (Null_val.NotNull, _) | None -> Some (env, heap)
            | Some (Null_val.Top, aaddr) ->
                Some (Env.set env ~key:rcvr ~nullness:Null_val.NotNull ~aaddr, heap)
            | Some (Null_val.Null, _) | Some (Null_val.Bot, _) -> None
          in
          let phi'', (rhs_nullness, rhs_aaddr) = eval_expr phi' rhs in
          Option.map phi'' ~f:(fun (env'', heap'') ->
              let new_heap =
                match rcvr_state with
                | None ->
                    (* TODO(archerd): figure out the correct thing to do in the None case... *)
                    Format.printf "Variable lookup for write failed: %s.%s\n" rcvr field;
                    heap
                | Some (_, rcvr_aaddr) ->
                    weak_update heap'' rcvr_aaddr field (rhs_nullness, rhs_aaddr)
              in
              (env'', new_heap))
      | Call _ -> failwith "should be handled by the call function?"
      | Array_write _ ->
          Some (env, heap)
          (* TODO(archerd): fix this case (and the other array cases in eval_expr *)
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
  Option.bind caller_state ~f:(fun (caller_env, caller_heap) ->
      let constructor_alloc =
        match callsite with Stmt.Call { alloc_site; _ } -> alloc_site | _ -> None
      in
      match callsite with
      | Stmt.Call { rcvr; actuals; meth; _ } | Stmt.Exceptional_call { rcvr; actuals; meth; _ } ->
          let rcvr_nullness, rcvr_aaddr =
            (* TODO(archerd): could be more precise in the top case, but in the return function *)
            Option.value (Env.find caller_env rcvr) ~default:(Null_val.NotNull, Addr.Abstract.empty)
          in
          let rcvr_aaddr =
            Option.value_map constructor_alloc ~default:rcvr_aaddr ~f:Addr.Abstract.singleton
          in
          if Null_val.is_null_or_bot rcvr_nullness then bottom ()
          else
            let actuals, caller_state' =
              if List.(length actuals = length callee.formals) then (actuals, caller_state)
              else (
                Format.printf "Call using varargs to %s.%s (%i formals, %i actuals)\n" rcvr meth
                  (List.length callee.formals) (List.length actuals);
                arrayify_varargs callee actuals (List.length callee.formals) caller_state)
            in
            Option.bind caller_state' ~f:(fun (caller_env', _caller_heap') ->
                (* re-scope the env to include only the formal parameters and rebind receiver fields to callee-local variables *)
                let _rcvr_field_bindings =
                  if String.equal rcvr "this" then
                    let ({ package; class_name; _ } : Method_id.t) = callee.method_id in
                    Declared_fields.lookup ~package ~class_name fields |> fun { instance; _ } ->
                    List.filter_map (Set.to_list instance) ~f:(fun field ->
                        Option.map (Env.find caller_env' field) ~f:(fun data -> (field, data)))
                  else
                    let rcvr_aaddr =
                      Map.find caller_env' rcvr
                      |> Option.value_map ~default:Addr.Abstract.empty ~f:snd
                    in
                    Map.filter_keys caller_heap ~f:(fun (addr, _) -> Set.mem rcvr_aaddr addr)
                    |> Map.to_alist
                    |> List.map ~f:(fun ((_, field), data) -> (field, data))
                in
                (* TODO(archerd): the caller_state'' needs to be propagated somewhere... outside of this function *)
                let caller_state'', callee_env =
                  List.(
                    fold (zip_exn callee.formals actuals) ~init:(caller_state', Env.empty)
                      ~f:(fun (state, env) (formal, actual) ->
                        let state', (nullness, aaddr) = eval_expr state actual in
                        (state', Env.set env ~key:formal ~nullness ~aaddr)))
                  |> fun (state, env) ->
                  (* update the `this' variable *)
                  ( state,
                    if callee.method_id.static then Env.forget [ "this" ] env
                    else Env.set env ~key:"this" ~nullness:rcvr_nullness ~aaddr:rcvr_aaddr
                      (* List.fold rcvr_field_bindings ~init:env *)
                      (*   ~f:(fun acc (field, (nullness, aaddr)) -> *)
                      (*     Env.join_at_key acc ~key:field ~nullness ~aaddr) *) )
                  |> fun (state, env) ->
                  (* add the local variables to the environment *)
                  ( state,
                    List.fold callee.locals ~init:env ~f:(fun env var ->
                        Env.set env ~key:var ~nullness:Null_val.Bot ~aaddr:Addr.Abstract.empty) )
                in
                (* project the caller heap down to the fields of formal parameters *)
                (* TODO(archerd): Possible extension: track everything linked to by these entries, and everything in the heap *)
                let _param_fields_heap =
                  let param_fields_addrs =
                    Map.data callee_env |> List.map ~f:snd
                    |> List.fold ~init:(Set.empty (module Addr)) ~f:Set.union
                  in
                  Map.filter_keys caller_heap ~f:(fun (addr, _) -> Set.mem param_fields_addrs addr)
                in
                Option.map caller_state'' ~f:(fun (_, caller_heap'') ->
                    (* (callee_env, Heap.project caller_heap'' callee_env))) *)
                    (callee_env, caller_heap'')))
      | s -> failwith (Format.asprintf "error: %a is not a callsite" Stmt.pp s))

let return ~(callee : Cfg.Fn.t) ~(caller : Cfg.Fn.t) ~callsite ~(caller_state : t)
    ~(return_state : t) ~fields : t =
  forget_used_tmp_vars callsite
  @@ Option.bind caller_state ~f:(fun (caller_env, caller_heap) ->
         Option.map return_state ~f:(fun (return_env, return_heap) ->
             match callsite with
             | Ast.Stmt.Call { lhs; rcvr; meth; alloc_site; actuals = _ } ->
                 (* [rcvr_aaddr] is one of:
                    * [`This], indicating an absent or "this" receiver; TODO(archerd): is it possible now to merge `This with `AAddr?
                    * [`AAddr aaddr], indicating a receiver with abstract address [aaddr]; or
                    * [`Static], indicating a static call
                    * [`None], TODO(archerd): edge case to deal with
                                                currently sending to `AAddr, which seems to be the right thing most of the time...
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
                         | None ->
                             let ({ package; class_name; _ } : Method_id.t) = caller.method_id in
                             let instance_fields =
                               Declared_fields.lookup ~package ~class_name
                                 (Declared_fields.get_current_fields ())
                               |> fun { instance; _ } -> instance
                             in
                             if Set.mem instance_fields rcvr then (
                               Method_id.set_current_method_id caller.method_id;
                               let _caller_state, (_, aaddr) =
                                 eval_expr caller_state
                                   (Ast.Expr.Deref { rcvr = "this"; field = rcvr })
                               in
                               Method_id.clear_current_method_id ();
                               `AAddr aaddr)
                             else (
                               Format.printf "return from `None: %s.%s to %a\n" rcvr meth
                                 Method_id.pp caller.method_id;
                               (* `None *)
                               `AAddr Addr.Abstract.empty))
                 in
                 let heap =
                   Map.merge_skewed caller_heap return_heap
                     ~combine:(fun ~key:_ (n1, a1) (n2, a2) ->
                       (Null_val.join n1 n2, Addr.Abstract.union a1 a2))
                   (* |> add_instance_fields_to_heap *)
                 in
                 let callee_instance_fields =
                   if callee.method_id.static then String.Set.empty
                   else
                     Declared_fields.lookup_instance fields ~package:callee.method_id.package
                       ~class_name:callee.method_id.class_name
                 in
                 let add_instance_fields_to_heap heap =
                   Set.fold callee_instance_fields ~init:heap ~f:(fun heap fld ->
                       let fld_val = Env.find return_env fld in
                       Option.value_map fld_val ~default:heap ~f:(fun (fld_nullness, fld_aaddr) ->
                           match rcvr_aaddr with
                           | `AAddr aaddr ->
                               weak_update heap aaddr fld (fld_nullness, fld_aaddr)
                               (* Set.fold aaddr ~init:heap ~f:(fun heap addr -> *)
                               (*     Map.set heap ~key:(addr, fld) ~data:(fld_nullness, fld_aaddr)) *)
                           | `This | `Static | `None -> heap))
                 in
                 let _heap_strict =
                   (* transfer the return value fields *)
                   (* TODO(archerd): Possible extension: track everything linked to by these entries, and everything in the heap *)
                   (match Map.find return_env Cfg.retvar with
                   | None -> caller_heap
                   | Some (_, retval_aaddr) ->
                       let retval_fields_heap =
                         Map.filter_keys return_heap ~f:(fun (addr, _) -> Set.mem retval_aaddr addr)
                       in
                       Map.merge_skewed caller_heap retval_fields_heap
                         ~combine:(fun ~key:_ (n1, aaddr1) (n2, aaddr2) ->
                           (Null_val.join n1 n2, Addr.Abstract.union aaddr1 aaddr2)))
                   (* transfer the instance fields to the receiver's fields *)
                   |> add_instance_fields_to_heap
                 in
                 (* (1) bind the receiver's abstract address if needed
                        and the instance fields for a this receiver, then
                    (2) transfer any return-value address binding to the callsite's lhs *)
                 let env =
                   (* (1) *)
                   (match rcvr_aaddr with
                   (* | `This -> *)
                   (*     Set.fold callee_instance_fields ~init:caller_env ~f:(fun env fld -> *)
                   (*         let fld_val = Env.find return_env fld in *)
                   (*         Option.value_map fld_val ~default:env *)
                   (*           ~f:(fun (fld_nullness, fld_aaddr) -> *)
                   (*             Env.join_at_key env ~key:fld ~nullness:fld_nullness ~aaddr:fld_aaddr)) *)
                   | `This | `Static | `None -> caller_env
                   | `AAddr rcvr_aaddr ->
                       if String.equal "<init>" meth && Option.is_some lhs then
                         Env.set caller_env ~key:(Option.value_exn lhs) ~nullness:Null_val.NotNull
                           ~aaddr:rcvr_aaddr
                       else
                         Env.join_at_key caller_env ~key:rcvr ~nullness:Null_val.NotNull
                           ~aaddr:rcvr_aaddr)
                   (* (2) *)
                   |> (match Map.find return_env Cfg.retvar with
                      | Some (retval_nullness, retval_aaddr) when Option.is_some lhs ->
                          Env.join_at_key ~key:(Option.value_exn lhs) ~nullness:retval_nullness
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
                             Env.join_at_key env ~key:fld ~nullness ~aaddr))
                   else Fn.id
                 in
                 (env, heap)
             | Ast.Stmt.Exceptional_call { rcvr; meth = _; actuals = _ } ->
                 (* [rcvr_aaddr] is one of:
                    * [`This], indicating an absent or "this" receiver;
                    * [`AAddr aaddr], indicating a receiver with abstract address [aaddr]; or
                    * [`Static], indicating a static call
                 *)
                 let rcvr_aaddr =
                   if callee.method_id.static then `Static
                   else if String.equal rcvr "this" then `This
                   else
                     match Map.find caller_env rcvr with Some a -> `AAddr (snd a) | None -> `None
                 in
                 let callee_instance_fields =
                   if callee.method_id.static then String.Set.empty
                   else
                     Declared_fields.lookup_instance fields ~package:callee.method_id.package
                       ~class_name:callee.method_id.class_name
                 in
                 let heap =
                   (* transfer the return value fields *)
                   (match Map.find return_env Cfg.exc_retvar with
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
                           | `AAddr aaddr ->
                               Set.fold aaddr ~init:heap ~f:(fun heap addr ->
                                   Map.set heap ~key:(addr, fld) ~data:(fld_nullness, fld_aaddr))
                           | `This | `Static | `None -> heap))
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
                               Env.join_at_key env ~key:fld ~nullness:fld_nullness ~aaddr:fld_aaddr))
                   | `Static | `None -> caller_env
                   | `AAddr rcvr_aaddr ->
                       Env.join_at_key caller_env ~key:rcvr ~nullness:Null_val.NotNull
                         ~aaddr:rcvr_aaddr)
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
                             Env.join_at_key env ~key:fld ~nullness ~aaddr))
                   else Fn.id
                 in
                 (env, heap)
             | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)))

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

(* TODO(archerd): need to set the method_id appropriately for these cases... *)
let approximate_missing_callee ~(caller_state : t) ~callsite : t =
  forget_used_tmp_vars callsite
  @@ Option.bind caller_state ~f:(fun (caller_env, caller_heap) ->
         match callsite with
         (* unknown constructor -- just bind the address and ignore the rest *)
         | Ast.Stmt.Call { lhs = Some lhs; rcvr; meth; actuals = _; alloc_site = Some a } ->
             Format.printf "Approximating missing callee %s.%s\n" rcvr meth;
             let env =
               Env.set caller_env ~key:lhs ~nullness:Null_val.NotNull
                 ~aaddr:(Addr.Abstract.singleton a)
             in
             Some (env, caller_heap)
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
         | Ast.Stmt.Call { lhs = _; rcvr; meth; actuals = [ rhs ]; alloc_site = None }
           when is_setter meth ->
             (* we (unsoundly) assume that any call `o.setFoo(x)` method is equivalent to `o.foo = x` *)
             let field = String.uncapitalize (String.chop_prefix_exn meth ~prefix:"set") in
             interpret Ast.(Stmt.Write { rcvr; field; rhs }) caller_state
         | Ast.Stmt.Call { lhs; rcvr; meth; actuals; alloc_site = None } ->
             let () = Format.printf "approximating method %s.%s\n" rcvr meth in
             (* for a call to an unknown function `x = o.foo(y1,y2,,...)`, we forget any constraints on:
                (1) fields of rcvr o [work needed when o is this]
                (2) fields of actuals y_i
                    (2.5) reachable fields from rcvr and actuals
                (3) address and value of lhs
                conservatively assuming that receiver and actual fields are modified by the call
             *)
             let affected_vars =
               List.filter_map actuals ~f:(function Expr.Var v -> Some v | _ -> None)
               |> if String.equal rcvr "this" then Fn.id else fun l -> rcvr :: l
             in
             let affected_addrs =
               Set.add_list
                 (Env.get_addrs affected_vars caller_env
                 |> List.append
                      (List.concat
                         (List.filter_map actuals ~f:(function
                           | Expr.Var _ -> None
                           | expr -> Some (Set.to_list @@ snd @@ snd @@ eval_expr caller_state expr))))
                 )
                 Addr.Abstract.empty
             in

             let env =
               Env.vars_to_top (Option.to_list lhs) caller_env
               |>
               if String.equal rcvr "this" then Env.vars_to_top []
                 (* TODO(archerd): should forget the fields of the current object,
                  * which are stored as local variables,
                  * so the fields need to be retrieved so they can be deleted. *)
               else Fn.id
             in
             let heap = Heap.reachable_to_top affected_addrs caller_heap in
             Some (env, heap)
         (* TODO(archerd): implement more Ast.Stmt.Call cases *)
         | Ast.Stmt.Exceptional_call _ -> caller_state
         | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s))

let%test "projecting heaps wrt envs" =
  let a0, a1, a2, a3, a4 = Addr.(fresh (), fresh (), fresh (), fresh (), fresh ()) in
  let v_of_addr a = (Null_val.NotNull, Addr.Abstract.of_alloc_site a) in
  let heap : Heap.t =
    Heap.empty
    |> Map.add_exn ~key:(a0, "foo") ~data:(v_of_addr a1)
    |> Map.add_exn ~key:(a1, "foo") ~data:(v_of_addr a2)
    |> Map.add_exn ~key:(a2, "foo") ~data:(v_of_addr a2)
    |> Map.add_exn ~key:(a3, "foo") ~data:(v_of_addr a4)
    |> Map.add_exn ~key:(a4, "foo") ~data:(v_of_addr a3)
  in
  let env1 = String.Map.singleton "x" (v_of_addr a0) in
  let heap1 = Heap.project heap env1 in
  let env2 = String.Map.singleton "x" (v_of_addr a1) in
  let heap2 = Heap.project heap env2 in
  let env3 = String.Map.add_exn env1 ~key:"y" ~data:(v_of_addr a3) in
  let heap3 = Heap.project heap env3 in
  Map.length heap1 = 3 && Map.length heap2 = 2 && Map.length heap3 = 5
