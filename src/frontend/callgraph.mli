open Syntax

type forward_t
(** regular callgraphs, mapping callers to callees *)

type reverse_t
(** reverse callgraphs, mapping callees to callers -- this can be done with a regular callgraph, but
    it is expensive so we hide the operation behind this opaque type to ensure that the callgraph is
    reversed once-and-for-all rather than each time callers are needed *)

type scc
(** strongly-connected components of a callgraph, to detect mutual recursion *)

type t = { forward : forward_t; reverse : reverse_t; scc : scc option }

val empty : t

val add : caller:Cfg.Fn.t -> callee:Cfg.Fn.t -> t -> t

val deserialize : fns:Cfg.Fn.t list -> Src_file.t -> t
(** deserialize a forward callgraph as in [deserialize_forward], and construct the reversed version and strongly-connected components partition*)

val deserialize_forward : fns:Cfg.Fn.t list -> Src_file.t -> forward_t
(** construct an internal representation of a serialized callgraph generated using github.com/bennostein/WALA-callgraph;
    (use the provided pool of [fns] to resolve callee [Method_id]'s to corresponding [Cfg.Fn]'s at deserialization-time rather
    than keeping a Method_id -> Method_id callgraph and resolving method_id's to fn's at analysis-time
*)

val filter : fns:Cfg.Fn.t list -> Src_file.t -> unit
(** write to stdout the provided serialized callgraph, stripped of any edges for which there is no corresponding function in [fns] *)

val callees : callsite:Ast.Stmt.t -> caller_method:Method_id.t -> cg:forward_t -> Cfg.Fn.t list

val is_syntactically_compatible : Ast.Stmt.t -> Cfg.Fn.t -> bool

val is_mutually_recursive : scc option -> Cfg.Fn.t -> Cfg.Fn.t -> bool

val methods_mutually_recursive : scc option -> Method_id.t -> Method_id.t -> bool

val reverse : fns:Cfg.Fn.t list -> forward_t -> reverse_t

val callers : callee_method:Method_id.t -> reverse_cg:reverse_t -> Cfg.Fn.t list

val dump_dot : filename:string -> forward_t -> unit

val print_scc_stats : scc -> unit
