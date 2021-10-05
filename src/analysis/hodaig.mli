open Domain
open Frontend
open Syntax

(** Higher-order DAIG: per-procedure DAIGs interoperating to perform summary-based interprocedural analysis *)
module Make (Dom : Abstract.Dom) : sig
  module D : Daig.Sig with type absstate := Dom.t
  (** underlying intra-procedural DAIGs *)

  module Q : Query.Sig with type state := Dom.t
  (** summary queries *)

  (*module R : Relation.Sig with type state := Dom.t*)
  (** relations over abstract states, representing summaries *)

  type t

  val apply_edit : diff:Tree_diff.t -> Loc_map.t -> t -> Loc_map.t * t
  (** apply a syntactic edit to a HODAIG, updating a Loc_map in the process *)

  val init : cfgs:Cfg.t Cfg.Fn.Map.t -> t
  (** construct the initial HODAIG for some collection of procedure CFGs *)

  val dump_dot : filename:string -> t -> unit
  (** dump a DOT representation of a HODAIG to [filename] *)

  val query :
    method_id:Method_id.t ->
    entry_state:Dom.t ->
    loc:Cfg.Loc.t ->
    callgraph:Callgraph.t ->
    fields:Declared_fields.t ->
    t ->
    Dom.t * t
end
