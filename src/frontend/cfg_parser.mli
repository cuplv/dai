open Tree_sitter_java
open Syntax

type edge = Cfg.Loc.t * Cfg.Loc.t * Ast.Stmt.t

val expr : ?exit_loc:Cfg.Loc.t -> Cfg.Loc.t -> CST.expression -> Ast.Expr.t * (Cfg.Loc.t * edge list)
(** Convert an expression concrete syntax tree to an expression in our IR, along with potentially some preceding statements for any function invocations and assignments therein, and a shifted current program location to accomodate those intermediate statements.
    That is, 
      * if `cst` represents a simple expression with no function invocations or assignments, return value is (<that expression in our IR>, (curr_loc,[]))
      * if `cst` contains function invocations f_1 ... f_k and assignments x_1=e_1 ... x_n=e_n, return value is 
          (
             <that expression in our IR>[ tmp_var_i / f_i][x_i / x_i=e_i],
             Some (fresh_loc_k+n, [curr_loc -[tmp_var_1 := f_1]-> fresh_loc_1, ... , fresh_loc_(k-1) -[tmp_var_k := f_k]-> fresh_loc_k] ++ [fresh_loc_k -[x_1=e_1]-> fresh_loc_k+1, ... , fresh_loc_(k+n-1) -[x_n=e_n]-> fresh_loc_(k+n)])
    Optional [exit_loc] param is used to special-case the common statement syntax of [Exp_stmt (`Assign_exp _)] and avoid generating extraneous locations and [Skip] edges

*)

val of_java_cst : CST.program -> Loc_map.t * Cfg.t
(** Construct a CFG in our IR from a (java) tree-sitter concrete syntax tree *)

val of_method_decl :
  Loc_map.t ->
  class_prefix:string ->
  CST.method_declaration ->
  (Loc_map.t * edge list * Cfg.Fn.t) option
(** construct a procedure's CFG from its declaration's concrete syntax tree *)

val edge_list_of_stmt_list :
  string ->
  Loc_map.t ->
  entry:Cfg.Loc.t ->
  exit:Cfg.Loc.t ->
  ret:Cfg.Loc.t ->
  CST.program ->
  Loc_map.t * edge list

val for_loop_header :
  string ->
  body_entry:Cfg.Loc.t ->
  body_exit:Cfg.Loc.t ->
  entry:Cfg.Loc.t ->
  exit:Cfg.Loc.t ->
  ret:Cfg.Loc.t ->
  Loc_map.t ->
  CST.for_statement ->
  Loc_map.t * edge list
