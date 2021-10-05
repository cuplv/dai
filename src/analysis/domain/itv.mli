open Dai.Import
open Apron
open Syntax

include Abstract.Dom with type t = Box.t Abstract1.t

val get_man : unit -> Box.t Manager.t

val pp_interval : Interval.t pp

val texpr_of_expr :
  ?fallback:(t -> Ast.Expr.t -> Texpr1.expr option) -> t -> Ast.Expr.t -> Texpr1.expr option

val eval_texpr : t -> Texpr1.expr -> Interval.t

val meet_with_constraint : ?fallback:(t -> Ast.Expr.t -> Texpr1.expr option) -> t -> Ast.Expr.t -> t

val filter_env : t -> f:(string -> bool) -> t

val assign : t -> Var.t -> Texpr1.expr -> t

val lookup : t -> Var.t -> Interval.t

val forget : Var.t array -> t -> t

val meet : t -> t -> t
