include Abstract.Dom

val is_safe : string -> Ast.Expr.t -> t -> bool option

val array_accesses : Stmt.t -> (Ast.Expr.t * Ast.Expr.t) list
