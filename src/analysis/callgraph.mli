open Syntax

type t

val deserialize : fns:Cfg.Fn.t list -> Frontend.Src_file.t -> t
(** construct an internal representation of a serialized callgraph generated using github.com/bennostein/WALA-callgraph;
    (use the provided pool of [fns] to resolve callee [Method_id]'s to corresponding [Cfg.Fn]'s at deserialization-time rather
    than keeping a Method_id -> Method_id callgraph and resolving method_id's to fn's at analysis-time
 *)

val resolve_with_callgraph :
  callsite:Ast.Stmt.t -> caller_method:Method_id.t -> callgraph:t -> Cfg.Fn.Set.t
