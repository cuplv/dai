open Tree_sitter_java
open Syntax

type edge = Cfg.Loc.t * Cfg.Loc.t * Ast.Stmt.t

type prgm_parse_result = {
  loc_map : Loc_map.t;
  cfgs : Cfg.t Cfg.Fn.Map.t;
  fields : Declared_fields.t;
  cha : Class_hierarchy.t;
}

val of_java_cst : ?acc:prgm_parse_result -> CST.program -> prgm_parse_result
(** Parse each method in a (java) tree-sitter concrete syntax tree to a CFG, adding to an [acc]umulator parse result if provided *)

val of_file_exn : ?acc:prgm_parse_result -> string -> prgm_parse_result
(** Parse each method in a (java) source file to a CFG, adding to an [acc]umulator parse result if provided *)

val of_files : files:string list -> prgm_parse_result
(** Parse each method in some (java) source [files] to a CFG *)

val expr :
  ?exit_loc:Cfg.Loc.t ->
  curr_loc:Cfg.Loc.t ->
  exc:Cfg.Loc.t ->
  CST.expression ->
  Ast.Expr.t * (Cfg.Loc.t * edge list)
(** Convert an expression concrete syntax tree to an expression in our IR, along with potentially some preceding statements for any function invocations and assignments therein, and a shifted current program location to accomodate those intermediate statements.
    That is, 
      * if `cst` represents a simple expression with no function invocations or assignments, return value is (<that expression in our IR>, (curr_loc,[]))
      * if `cst` contains function invocations f_1 ... f_k and assignments x_1=e_1 ... x_n=e_n, return value is 
          (
             <that expression in our IR>[ tmp_var_i / f_i][x_i / x_i=e_i],
             Some (fresh_loc_k+n, [curr_loc -[tmp_var_1 := f_1]-> fresh_loc_1, ... , fresh_loc_(k-1) -[tmp_var_k := f_k]-> fresh_loc_k] ++ [fresh_loc_k -[x_1=e_1]-> fresh_loc_k+1, ... , fresh_loc_(k+n-1) -[x_n=e_n]-> fresh_loc_(k+n)])
    Optional [exit_loc] param is used to special-case the common statement syntax of [Exp_stmt (`Assign_exp _)] and avoid generating extraneous locations and [Skip] edges

*)

val of_method_decl :
  Loc_map.t ->
  ?package:string list ->
  class_name:string ->
  CST.method_declaration ->
  (Loc_map.t * edge list * Cfg.Fn.t) option
(** construct a procedure's CFG from its declaration's concrete syntax tree *)

val of_constructor_decl :
  Loc_map.t ->
  ?package:string list ->
  class_name:string ->
  CST.constructor_declarator ->
  CST.constructor_body ->
  (Loc_map.t * edge list * Cfg.Fn.t) option
(** construct a constructor's CFG from its declaration's concrete syntax tree *)

val types_of_formals : CST.formal_parameters -> string list
(** simpler representation of a formal parameter list, for distinguishing overloading *)

val edge_list_of_stmt_list :
  Method_id.t ->
  Loc_map.t ->
  entry:Cfg.Loc.t ->
  exit:Cfg.Loc.t ->
  ret:Cfg.Loc.t ->
  exc:Cfg.Loc.t ->
  CST.program ->
  Loc_map.t * edge list

val for_loop_header :
  Method_id.t ->
  body_entry:Cfg.Loc.t ->
  body_exit:Cfg.Loc.t ->
  entry:Cfg.Loc.t ->
  exit:Cfg.Loc.t ->
  ret:Cfg.Loc.t ->
  exc:Cfg.Loc.t ->
  Loc_map.t ->
  CST.for_statement ->
  Loc_map.t * edge list * edge
(** Return value is composed of:
   (1) updated loc_map,
   (2) all CFG edges for the for-loop excluding its body,
   (3) the back edge from the update back up to the condition.

   This is distinguished to support updates to the loop-header without updating the full loop body.

   Return value (3) is contained in (2), but is useful when constructing the updated DAIG region for an edited loop.

   Analogs to this function are not needed for conditional/while-loop headers because those can easily be identified from the CFG structure.
*)
