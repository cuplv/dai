open Apron
open Syntax

include Abstract.Dom with type t = Oct.t Abstract1.t

val get_man : unit -> Oct.t Manager.t

val texpr_of_expr :
  ?fallback:(t -> Ast.Expr.t -> Texpr1.expr option) -> t -> Ast.Expr.t -> Texpr1.expr option

val eval_texpr : t -> Texpr1.expr -> Interval.t

val meet_with_constraint : ?fallback:(t -> Ast.Expr.t -> Texpr1.expr option) -> t -> Ast.Expr.t -> t

val filter_env : t -> f:(string -> bool) -> t

val assign : t -> Var.t -> Texpr1.expr -> t

val weak_assign : t -> Var.t -> Texpr1.expr -> t

val lookup : t -> Var.t -> Interval.t

val forget : Var.t array -> t -> t

val meet : t -> t -> t
