open Import

module Make (Val : Abstract.Val) : Abstract.Dom = struct
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
  let rec eval_expr env = function
    | Ast.Expr.Var v -> (
        match Env.find env v with Some value -> value | None -> Val.of_lit Ast.Lit.Undefined )
    | Ast.Expr.Lit l -> Val.of_lit l
    | Ast.Expr.Binop { l; op; r } -> Val.eval_binop (eval_expr env l) op (eval_expr env r)
    | Ast.Expr.Unop { op; e } -> Val.eval_unop op (eval_expr env e)

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
          | Expr _ | Skip -> Some env)
    in
    fun stmt -> flip ( >>= ) (fun env -> mfn.Art.mfn_data (stmt, env))

  let implies _x _y = failwith "todo"

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
      | Some l, Some r -> mfn.mfn_data (l, r)
      | Some x, _ | _, Some x -> Some x
      | _ -> None

  let join = make_memoized_pointwise_binary_op Val.join (Name.of_string "Dom#join")

  let widen = make_memoized_pointwise_binary_op Val.widen (Name.of_string "Dom#widen")

  let pp fs (env : t) =
    match env with
    | Some env ->
        let pp_string_color color str = Format.fprintf fs "%s%s%s" color str Colors.reset in
        pp_string_color Colors.cyan "{";
        Format.open_hovbox 0;
        Format.print_space ();
        List.iter (Env.to_list env) ~f:(fun (k, v) ->
            Format.fprintf fs "%s%s%s -> %a %s;%s@ " Colors.cyan k Colors.reset Val.pp v Colors.red
              Colors.reset);
        Format.close_box ();
        pp_string_color Colors.cyan "}"
    | None -> Format.print_string "bottom"
end
