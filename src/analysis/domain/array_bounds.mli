open Syntax

include Abstract.DomNoCtx

val is_safe : string -> Ast.Expr.t -> t -> bool option

val array_accesses : Ast.Stmt.t -> (Ast.Expr.t * Ast.Expr.t) list
