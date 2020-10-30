open Import

module Make (Dom : sig
  include Abstract.DomNoCtx
end) : sig
  include Abstract.DomNoCtx

  val lift : Dom.t -> t
end = struct
  module Art = Adapton.MakeArt.Of (Name) (Dom)
  include Dom

  (* hack to silence typechecker and explicitly construct elements -- probably a nicer way to do this with module/type equalities, but this works.*)
  let lift x = x

  (* Lift a binary [op]eration of the abstract domain to an adapton-memoized equivalent *)
  let mk_memoized_binary_op op nm =
    let mfn = Art.mk_mfn nm (module Adapton.Types.Tuple2 (Dom) (Dom)) (fun _mfn (l, r) -> op l r) in
    fun l r -> mfn.mfn_art (l, r) |> Art.force

  let widen = mk_memoized_binary_op Dom.widen (Name.of_string "Dom#widen")

  let join = mk_memoized_binary_op Dom.join (Name.of_string "Dom#join")

  let interpret =
    let mfn =
      Art.mk_mfn (Name.of_string "Dom#interpret")
        (module Adapton.Types.Tuple2 (Ast.Stmt) (Dom))
        (fun _mfn (stmt, env) -> Dom.interpret stmt env)
    in
    fun l r -> mfn.mfn_art (l, r) |> Art.force
end

module Make_env (Val : Abstract.Val) : sig
  include Abstract.DomNoCtx
end = struct
  module Env = Adapton.Trie.Map.MakeNonInc (Name) (DefaultArtLib) (Adapton.Types.String) (Val)
  include Adapton.Types.Option (Env)
  module Art = Adapton.MakeArt.Of (Name) (Adapton.Types.Option (Env))

  let sexp_of_t =
    let open Sexp in
    function
    | None -> Atom "bottom"
    | Some env -> List (Env.fold (fun a k v -> List [ Atom k; Val.sexp_of_t v ] :: a) [] env)

  let t_of_sexp =
    let open Sexp in
    function
    | Atom "bottom" -> None
    | List tms ->
        let add_sexp_term env = function
          | List [ Atom k; v ] -> Env.add env k (Val.t_of_sexp v)
          | _ -> failwith "malformed environment s-expression"
        in
        Some (List.fold tms ~init:(Env.of_list []) ~f:add_sexp_term)
    | _ -> failwith "malformed environment s-expression"

  let hash_fold_t seed = hash 0 >> Ppx_hash_lib.Std.Hash.fold_int seed

  let init () = Some (Env.of_list [])

  let is_bot = Option.is_none

  (* TODO: adapton-ify? *)
  let rec eval_expr env =
    let open Ast in
    function
    | Expr.Var v -> (
        match Env.find env v with Some value -> value | None -> Val.of_lit Lit.Undefined )
    | Expr.Lit l -> Val.of_lit l
    | Expr.Binop { l; op; r } -> Val.eval_binop (eval_expr env l) op (eval_expr env r)
    | Expr.Unop { op; e } -> Val.eval_unop op (eval_expr env e)
    | Expr.Deref _ | Expr.Array _ ->
        failwith "Arrays and Objects not handled by this basic environment functor"

  let interpret =
    let mfn =
      Art.mk_mfn (Name.of_string "Dom#interpret")
        (module Adapton.Types.Tuple2 (Ast.Stmt) (Env))
        (fun _mfn (stmt, env) ->
          let open Ast.Stmt in
          match stmt with
          | Assign { lhs; rhs } ->
              let rhs = eval_expr env rhs in
              Some (Env.add env lhs rhs)
          | Throw { exn = _ } -> None
          | Assume e -> (
              match Val.truthiness (eval_expr env e) with
              | `T | `Either -> Some env
              | `F | `Neither -> None )
          | Expr _ | Skip | Write _ | Call _ -> Some env)
    in
    fun stmt -> flip ( >>= ) (fun env -> mfn.mfn_art (stmt, env) |> Art.force)

  let implies _x _y = failwith "todo"

  (* let rename =
       let mfn =
         Art.mk_mfn (Name.of_string "Dom#rename")
           (module Adapton.Types.Tuple3 (Adapton.Types.String) (Adapton.Types.String) (Env))
           (fun _mfn (old_var, new_var, env) ->
             if not @@ Env.mem env old_var then Some env
             else
               let add_with_renaming env k v =
                 if String.equal k old_var then Env.add env new_var v else Env.add env k v
               in
               Some (Env.fold add_with_renaming (Env.of_list []) env))
       in
       fun ~old_var ~new_var ->
       flip ( >>= ) (fun env -> mfn.mfn_art (old_var, new_var, env) |> Art.force)

     let project =
       let mfn =
         Art.mk_mfn (Name.of_string "Dom#project")
           (module Adapton.Types.Tuple2 (Adapton.Types.List (Adapton.Types.String)) (Env))
           (fun _mfn (vars, env) ->
             let vars = String.Set.of_list vars in
             let add_if_in_projection env k v = if Set.mem vars k then Env.add env k v else env in
             Some (Env.fold add_if_in_projection (Env.of_list []) env))
       in
       fun ~vars -> flip ( >>= ) (fun env -> mfn.mfn_art (vars, env) |> Art.force) *)

  let handle_return ~caller_state:_ ~return_state:_ ~callsite:_ ~callee_defs:_ = failwith "todo"

  let make_memoized_pointwise_binary_op op nm =
    let mfn =
      Art.mk_mfn nm
        (module Adapton.Types.Tuple2 (Env) (Env))
        (fun _mfn (l, r) ->
          Env.fold
            (fun acc k v ->
              match Env.find acc k with
              | Some acc_v -> Env.add acc k (op v acc_v)
              | None -> Env.add acc k v)
            l r
          |> Option.some)
    in
    fun l r ->
      match (l, r) with
      | Some l, Some r -> mfn.mfn_art (l, r) |> Art.force
      | Some x, _ | _, Some x -> Some x
      | _ -> None

  let join = make_memoized_pointwise_binary_op Val.join (Name.of_string "Dom#join")

  let widen = make_memoized_pointwise_binary_op Val.widen (Name.of_string "Dom#widen")

  let pp fs (env : t) =
    match env with
    | Some env ->
        let pp_binding fs (k, v) = Format.fprintf fs "%s -> %a" k Val.pp v in
        List.pp ~pre:"{" ~suf:"}" ";@ " pp_binding fs (Env.to_list env)
    | None -> Format.print_string "bottom"
end

module Make_env_with_heap (Val : Abstract.Val) : Abstract.DomNoCtx = struct
  module AAddr_or_val = struct
    include Adapton.Types.Sum2 (Addr.Abstract) (Val)

    let apply_pointwise addr_op val_op aov1 aov2 =
      match (aov1, aov2) with
      | InL a1, InL a2 -> Some (InL (addr_op a1 a2))
      | InR v1, InR v2 -> Some (InR (val_op v1 v2))
      | _ -> None

    let addr = function InL a -> Some a | InR _ -> None

    let value = function InL _ -> None | InR v -> Some v
  end

  module Env =
    Adapton.Trie.Map.MakeNonInc (Name) (DefaultArtLib) (Adapton.Types.String) (AAddr_or_val)
  module Heap =
    Adapton.Trie.Map.MakeNonInc (Name) (DefaultArtLib)
      (Adapton.Types.Tuple2 (Addr) (Adapton.Types.Int))
      (AAddr_or_val)
  module AState = Adapton.Types.Tuple2 (Env) (Heap)
  include Adapton.Types.Option (AState)
  module Art = Adapton.MakeArt.Of (Name) (Adapton.Types.Option (AState))

  let hash_fold_t seed = hash 0 >> Ppx_hash_lib.Std.Hash.fold_int seed

  let init () = Some (Env.of_list [], Heap.of_list [])

  let is_bot = Option.is_none

  let rec eval_expr env heap : Ast.Expr.t -> AAddr_or_val.t option =
    let open Ast in
    let some_val v = Some (AAddr_or_val.InR v) in
    function
    | Expr.Var v -> Env.find env v
    | Expr.Lit l -> some_val (Val.of_lit l)
    | Expr.Binop { l; op; r } -> (
        match (eval_expr env heap l, eval_expr env heap r) with
        | Some (AAddr_or_val.InR v_l), Some (AAddr_or_val.InR v_r) ->
            some_val @@ Val.eval_binop v_l op v_r
        | _ -> None )
    | Expr.Unop { op; e } -> (
        match eval_expr env heap e with
        | Some (AAddr_or_val.InR v) -> Some (AAddr_or_val.InR (Val.eval_unop op v))
        | Some (AAddr_or_val.InL _) when Unop.equal op Unop.Typeof ->
            Some (AAddr_or_val.InR (Val.of_lit (Lit.String "object")))
        | _ -> None )
    | Expr.Deref { rcvr; field } -> (
        match (eval_expr env heap rcvr, eval_expr env heap field) with
        | Some (AAddr_or_val.InL addrs), Some (AAddr_or_val.InR field) ->
            Heap.fold
              (fun acc (obj, k) -> function
                | AAddr_or_val.InR v ->
                    if Addr.Abstract.mem addrs obj && Val.models field (Ast.Lit.Int k) then
                      match acc with Some acc -> Some (Val.join acc v) | None -> Some v
                    else acc | _ -> acc)
              None heap
            |> Option.map ~f:(fun v -> AAddr_or_val.InR v)
        | _ -> None )
    | Expr.Array _ ->
        (* NOTE: wouldn't be that tricky to support if it comes up: just thread env/heap through
           eval_expr so we can store elements there and then return the addr
        *)
        failwith "array literals not supported when nested within compound expression"

  let weak_update heap aaddr field new_v =
    Heap.fold
      (fun acc (addr, k) v ->
        if Addr.Abstract.mem aaddr addr && Val.models field (Ast.Lit.Int k) then
          match AAddr_or_val.apply_pointwise Addr.Abstract.union Val.join v new_v with
          | Some v -> Heap.add acc (addr, k) v
          | None -> acc
        else acc)
      (Heap.empty ~min_depth:2) heap

  let interpret =
    let mfn =
      Art.mk_mfn (Name.of_string "Dom#interpret")
        (module Adapton.Types.Tuple2 (Ast.Stmt) (AState))
        (fun _mfn (stmt, (env, heap)) ->
          let open Ast.Stmt in
          match stmt with
          | Assign { lhs; rhs = Ast.Expr.Array { elts; alloc_site } } ->
              let addr = Addr.of_alloc_site alloc_site in
              let abstract_addr = AAddr_or_val.InL (Addr.Abstract.singleton addr) in
              let env = Env.add env lhs abstract_addr in
              let heap =
                List.foldi elts ~init:heap ~f:(fun i acc curr ->
                    match eval_expr env heap curr with
                    | Some v -> Heap.add acc (addr, i) v
                    | None -> acc)
              in
              Some (env, heap)
          | Assign { lhs; rhs } -> eval_expr env heap rhs >>| fun r -> (Env.add env lhs r, heap)
          | Write { rcvr; field; rhs } ->
              Env.find env rcvr >>= AAddr_or_val.addr >>= fun rcvr ->
              eval_expr env heap field >>= AAddr_or_val.value >>= fun field ->
              eval_expr env heap rhs >>| fun rhs -> (env, weak_update heap rcvr field rhs)
          | Throw _ -> None
          | Assume e -> (
              eval_expr env heap e >>= AAddr_or_val.value >>| Val.truthiness >>= function
              | `T | `Either -> Some (env, heap)
              | _ -> None )
          | Skip | Expr _ | Call _ -> Some (env, heap))
    in
    fun stmt -> flip ( >>= ) (fun state -> mfn.mfn_art (stmt, state) |> Art.force)

  let make_memoized_pointwise_binary_op nm op =
    let mfn =
      Art.mk_mfn nm
        (module Adapton.Types.Tuple2 (AState) (AState))
        (fun _mfn ((lenv, lheap), (renv, rheap)) ->
          let combine l r fold find add =
            fold
              (fun acc k v ->
                match find acc k with Some acc_v -> add acc k (op v acc_v) | None -> add acc k v)
              l r
          in
          let env = combine lenv renv Env.fold Env.find Env.add in
          let heap = combine lheap rheap Heap.fold Heap.find Heap.add in
          Some (env, heap))
    in
    fun l r ->
      match (l, r) with
      | Some l, Some r -> mfn.mfn_art (l, r) |> Art.force
      | Some x, _ | _, Some x -> Some x
      | _ -> None

  let widen =
    make_memoized_pointwise_binary_op (Name.of_string "Dom#widen") (fun l r ->
        match (l, r) with
        | AAddr_or_val.InL a1, AAddr_or_val.InL a2 -> AAddr_or_val.InL (Addr.Abstract.union a1 a2)
        | AAddr_or_val.InR v1, AAddr_or_val.InR v2 -> AAddr_or_val.InR (Val.widen v1 v2)
        | _ -> failwith "This domain functor assumes static separation of arrays and scalars")

  let join =
    make_memoized_pointwise_binary_op (Name.of_string "Dom#join") (fun l r ->
        match (l, r) with
        | AAddr_or_val.InL a1, AAddr_or_val.InL a2 -> AAddr_or_val.InL (Addr.Abstract.union a1 a2)
        | AAddr_or_val.InR v1, AAddr_or_val.InR v2 -> AAddr_or_val.InR (Val.join v1 v2)
        | _ -> failwith "This domain functor assumes static separation of arrays and scalars")

  let implies _ _ = failwith "todo"

  let handle_return ~caller_state:_ ~return_state:_ ~callsite:_ ~callee_defs:_ = failwith "todo"

  let sexp_of_t =
    let open Sexp in
    let sexp_of_v = function
      | AAddr_or_val.InL aaddr ->
          let addrs_sexp =
            List (Addr.Abstract.fold ~f:(fun acc addr -> Addr.sexp_of_t addr :: acc) ~init:[] aaddr)
          in
          List [ Atom "AAddr"; addrs_sexp ]
      | AAddr_or_val.InR v -> List [ Atom "Val"; Val.sexp_of_t v ]
    in
    function
    | None -> Atom "bottom"
    | Some (env, heap) ->
        let env_sexp = List (Env.fold (fun a k v -> List [ Atom k; sexp_of_v v ] :: a) [] env) in
        let heap_sexp =
          List
            (Heap.fold
               (fun a (obj, idx) v ->
                 List [ Addr.sexp_of_t obj; Int.sexp_of_t idx; sexp_of_v v ] :: a)
               [] heap)
        in
        List [ env_sexp; heap_sexp ]

  let t_of_sexp =
    let open Sexp in
    let v_of_sexp = function
      | List [ Atom "AAddr"; List addrs ] ->
          AAddr_or_val.InL
            (List.fold addrs ~init:Addr.Abstract.empty ~f:(fun acc addr ->
                 Addr.Abstract.add acc (Addr.t_of_sexp addr)))
      | List [ Atom "Val"; v ] -> AAddr_or_val.InR (Val.t_of_sexp v)
      | _ -> failwith "malformed environment s-expression"
    in
    function
    | Atom "bottom" -> None
    | List [ List env_sexp; List heap_sexp ] ->
        let env =
          List.fold env_sexp ~init:(Env.of_list []) ~f:(fun env ->
            function
            | List [ Atom k; v ] -> Env.add env k (v_of_sexp v)
            | _ -> failwith "malformed environment s-expression")
        in
        let heap =
          List.fold heap_sexp ~init:(Heap.of_list []) ~f:(fun heap ->
            function
            | List [ obj; fld; v ] ->
                Heap.add heap (Addr.t_of_sexp obj, Int.t_of_sexp fld) (v_of_sexp v)
            | _ -> failwith "malformed environment s-expression")
        in
        Some (env, heap)
    | _ -> failwith "malformed environment s-expression"
end
