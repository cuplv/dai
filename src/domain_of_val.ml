open Import
open Adapton

module type Abstract_value = sig
  include Adapton.Data.S

  val pp : t pp

  val join : t -> t -> t

  val widen : t -> t -> t

  val implies : t -> t -> bool

  val eval_binop : t -> Ast.Binop.t -> t -> t

  val eval_unop : Ast.Unop.t -> t -> t

  val of_lit : Ast.Lit.t -> t

  val truthiness : t -> [ `Neither | `T | `F | `Either ]
end

module Make (Val : Abstract_value) : Domain_intf.Dom = struct
  module Env = Trie.Map.Make (Name) (DefaultArtLib) (Types.String) (Val)
  include Env

  let hash_fold_t seed env =
    Ppx_hash_lib.Std.Hash.fold_int seed (Env.hash 0 env)

  let init = Env.of_list (Name.gensym ()) []

  let rec eval_expr env = function
    | Ast.Expr.Var v -> (
        match find env v with
        | Some value -> value
        | None -> Val.of_lit Ast.Lit.Undefined )
    | Ast.Expr.Lit l -> Val.of_lit l
    | Ast.Expr.Binop { l; op; r } ->
        Val.eval_binop (eval_expr env l) op (eval_expr env r)
    | Ast.Expr.Unop { op; e } -> Val.eval_unop op (eval_expr env e)

  let interpret =
    let mfn =
      Art.mk_mfn
        (Name.of_string "Dom#interpret")
        (module Types.Tuple2 (Ast.Stmt) (Env))
        (fun _mfn (stmt, env) ->
          let open Ast.Stmt in
          match stmt with
          | Assign { lhs = _; rhs = _ } ->
              failwith "todo"
              (*let rhs = eval_expr env rhs in add (Name.gensym ()) env lhs rhs;*)
          | Throw { exn = _ } -> failwith "todo"
          | Assume _e -> failwith "todo"
          | Expr _ | Skip -> env)
    in
    fun stmt env -> mfn.mfn_data (stmt, env)

  let implies _x _y = failwith "todo"

  let make_memoized_pointwise_binary_op op nm =
    let mfn =
      Art.mk_mfn nm
        (module Types.Tuple2 (Env) (Env))
        (fun _mfn (l, r) ->
          fold
            (fun acc k v ->
              match find acc k with
              | Some acc_v -> add nm acc k (op v acc_v)
              | None -> add nm acc k v)
            l r)
    in
    fun l r -> mfn.mfn_data (l, r)

  let join =
    make_memoized_pointwise_binary_op Val.join (Name.of_string "Dom#join")

  let widen =
    make_memoized_pointwise_binary_op Val.widen (Name.of_string "Dom#widen")

  let pp fs (env : t) =
    let pp_string_color color str =
      Format.fprintf fs "%s%s%s" color str Colors.reset
    in
    pp_string_color Colors.cyan "{";
    Format.open_hovbox 0;
    Format.print_space ();
    List.iter (to_list env) ~f:(fun (k, v) ->
        Format.fprintf fs "%s%s%s -> %a %s;%s@ " Colors.cyan k Colors.reset
          Val.pp v Colors.red Colors.reset);
    Format.close_box ();
    pp_string_color Colors.cyan "}"
end
