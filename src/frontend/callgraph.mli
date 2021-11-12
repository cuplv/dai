open Syntax

type t
(** regular callgraphs, mapping callers to callees *)

type reverse_t
(** reverse callgraphs, mapping callees to callers -- this can be done with a regular callgraph, but
    it is expensive so we hide the operation behind this opaque type to ensure that the callgraph is
    reversed once-and-for-all rather than each time callers are needed *)

type bidirectional = { forward : t; reverse : reverse_t }

val deserialize : fns:Cfg.Fn.t list -> Src_file.t -> t
(** construct an internal representation of a serialized callgraph generated using github.com/bennostein/WALA-callgraph;
    (use the provided pool of [fns] to resolve callee [Method_id]'s to corresponding [Cfg.Fn]'s at deserialization-time rather
    than keeping a Method_id -> Method_id callgraph and resolving method_id's to fn's at analysis-time
 *)

val resolve_with_callgraph :
  callsite:Ast.Stmt.t -> caller_method:Method_id.t -> callgraph:t -> Cfg.Fn.t list

val is_syntactically_compatible : Ast.Stmt.t -> Cfg.Fn.t -> bool

val reverse : fns:Cfg.Fn.t list -> t -> reverse_t

val callers : callee_method:Method_id.t -> reverse_cg:reverse_t -> Cfg.Fn.t list

val dump_dot : filename:string -> t -> unit
