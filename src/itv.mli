open Apron

include Abstract.Dom with type t = Box.t Abstract1.t

val get_man : unit -> Box.t Manager.t

val texpr_of_expr :
  ?fallback:(t -> Ast.Expr.t -> Texpr1.expr option) -> t -> Ast.Expr.t -> Texpr1.expr option

val eval_texpr : t -> Texpr1.expr -> Interval.t

val meet_with_constraint : ?fallback:(t -> Ast.Expr.t -> Texpr1.expr option) -> t -> Ast.Expr.t -> t
