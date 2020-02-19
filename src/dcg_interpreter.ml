open Adapton
open Import
open Interpreter
module Dcg = Dcg.With_default_articulation
open Dcg

module Make (Val : Abstract_value) : sig
  include Articulated.S

  module ArtOpt : Adapton.Art.S with type data := t option

  val init : t

  val join : t -> t -> Art.t

  val exec_stmt : t -> AStmt.t -> t option
end = struct
  module Env = EnvNonInc (Val)
  include Env
  module Art = MakeArt.Of (Name) (Env)
  module ArtOpt = MakeArt.Of (Name) (Types.Option (Env))

  type name = Name.t

  let init = Env.init

  let join =
    let mfn =
      Art.mk_mfn
        (Name.of_string "Dcg_interpreter#join")
        (module Types.Tuple2 (Env) (Env))
        (fun _mfn (l, r) -> Env.join l r)
    in
    fun l r -> mfn.mfn_art (l, r)

  let exec_stmt =
    let mfn =
      ArtOpt.mk_mfn
        (Name.of_string "Dcg_interpreter#exec_stmt")
        (module Types.Tuple2 (Env) (AStmt))
        (fun mfn (env, stmt) ->
          let exec_stmt env stmt =
            let env_hash = Env.hash 0 env in
            let stmt_hash = AStmt.hash 0 stmt in
            Format.fprintf Format.std_formatter
              "memo fn input hashes: (%d; %d)\n" env_hash stmt_hash;
            ArtOpt.force @@ mfn.ArtOpt.mfn_art (env, stmt)
          in
          match stmt with
          | Stmt.Seq (l, r) -> exec_stmt env l >>= flip exec_stmt r
          | If { cond; then_body; else_body } -> (
              match Val.truthiness @@ eval_expr env cond with
              | `T -> exec_stmt env then_body
              | `F -> exec_stmt env else_body
              | `Either -> (
                  match (exec_stmt env then_body, exec_stmt env else_body) with
                  | Some then_res, Some else_res ->
                      Some (Art.force (join then_res else_res))
                  | Some res, _ | _, Some res -> Some res
                  | _ -> None )
              | `Neither -> None )
          | Assign { lhs; rhs } -> Some (add env lhs (eval_expr env rhs))
          | Throw { exn = _ } -> None
          | Expr _ | Skip -> Some env
          | Art a -> exec_stmt env (AStmt.Art.force a))
    in
    fun env stmt ->
      let env_hash = Env.hash 0 env in
      let stmt_hash = AStmt.hash 0 stmt in
      Format.fprintf Format.std_formatter "input hashes: (%d;%d)\n" env_hash
        stmt_hash;
      ArtOpt.force @@ mfn.ArtOpt.mfn_art (env, stmt)
end

open Make (Set_of_concrete)

let ast = Dcg_parser.arith0

let%test "dcg_interpret ast" =
  ( match exec_stmt init ast with
  | Some x -> Format.fprintf Format.std_formatter "dcg_interpret ast: %a\n" pp x
  | None ->
      Format.fprintf Format.std_formatter
        "dcg_interpret0: no control flow to exit\n" );
  true

let%test _ =
  Format.print_string "\n\nModifying AST...\n\n\n";
  ( match ast with
  | Seq (Seq ((Art og_stmt as a), _), _) ->
      (*  | Seq (Seq (Name {name=_; stmt= Art og_stmt as a}, _), _) ->*)
      let new_stmt =
        Stmt.Assign { lhs = "x"; rhs = Expr.Lit (Lit.Float 100.) }
      in
      let og_stmt = AStmt.Art.force og_stmt in
      AStmt.set a (AStmt.seq new_stmt og_stmt)
  | _ -> failwith "failed to modify ast" );
  true

let%test "dcg_interpret modified_ast" =
  ( match exec_stmt init ast with
  | Some x ->
      Format.fprintf Format.std_formatter "dcg_interpret modified: %a\n" pp x
  | None ->
      Format.fprintf Format.std_formatter
        "dcg_interpret1: no control flow to exit\n" );
  true
