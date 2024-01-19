open Dai.Import
open Domain
open Frontend
open Syntax

(** Demanded Summarization Graph : per-procedure DAIGs interoperating to perform summary-based interprocedural analysis *)
module Make (Dom : Abstract.Dom) : sig
  module Dom : module type of Abstract.DomWithDataStructures (Dom)

  module D : Daig.Sig with type absstate := Dom.t
  (** underlying intra-procedural DAIGs *)

  module Q : Query.Sig with type state := Dom.t
  (** summary queries *)

  (*module R : Relation.Sig with type state := Dom.t*)
  (** relations over abstract states, representing summaries *)

  type t = (Cfg.t * D.t Dom.Map.t) Cfg.Fn.Map.t

  val print_summaries : ?num_summaries:int -> t pp

  val print_stats : t pp

  val materialize_daig :
    fn:Cfg.Fn.t ->
    entry_state:Dom.t ->
    t ->
    D.t * t

  val apply_edit : cha:Class_hierarchy.t -> diff:Tree_diff.t -> Loc_map.t -> t -> Loc_map.t * t
  (** apply a syntactic edit to a DSG, updating a Loc_map in the process *)

  val init : cfgs:Cfg.t Cfg.Fn.Map.t -> t
  (** construct the initial DSG for some collection of procedure CFGs *)

  val add_exn : cfgs:Cfg.t Cfg.Fn.Map.t -> t -> t
  (** add some new cfgs to this DSG *)

  val fns : t -> Cfg.Fn.t list

  val dump_dot : filename:string -> ?num_daigs:int -> t -> unit
  (** dump a DOT representation of a DSG to [filename] *)

  val query :
    fn:Cfg.Fn.t ->
    entry_state:Dom.t ->
    loc:Cfg.Loc.t ->
    cg:Callgraph.t ->
    fields:Declared_fields.t ->
    t ->
    Dom.t * t
  (** query for the abstract state at some [loc] under some [entry_state] precondition *)

  val loc_only_query :
    fn:Cfg.Fn.t ->
    loc:Cfg.Loc.t ->
    cg:Callgraph.t ->
    fields:Declared_fields.t ->
    entrypoints:Cfg.Fn.t list ->
    t ->
    Dom.t list * t
  (** query for the abstract state at some [loc] under _any_ reachable [entry_state] precondition,
        exploring back to the specified [entrypoints]
  *)

  val drop_daigs : t -> t

  val dirty_interproc_deps : ?ctx:Dom.t -> Cfg.Fn.t -> t -> t

  val f0_daigs : t -> D.t list

  val check_deps : t -> unit
end
