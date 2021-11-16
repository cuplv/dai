open Dai.Import
open Domain
open Frontend
open Syntax

(** Demanded Summarization Graph : per-procedure DAIGs interoperating to perform summary-based interprocedural analysis *)
module Make (Dom : Abstract.Dom) (Ctx : Context.Sig) : sig
  module D : Daig.Sig with type absstate := Dom.t
  (** underlying intra-procedural DAIGs *)

  type t

  val print_stats : t pp

  val apply_edit : cha:Class_hierarchy.t -> diff:Tree_diff.t -> Loc_map.t -> t -> Loc_map.t * t
  (** apply a syntactic edit to a DSG, updating a Loc_map in the process *)

  val init : cfgs:Cfg.t Cfg.Fn.Map.t -> t
  (** construct the initial DSG for some collection of procedure CFGs *)

  val add_exn : cfgs:Cfg.t Cfg.Fn.Map.t -> t -> t
  (** add some new cfgs to this DSG *)

  val fns : t -> Cfg.Fn.t list

  val dump_dot : filename:string -> t -> unit
  (** dump a DOT representation of a DSG to [filename] *)

  val query :
    fn:Cfg.Fn.t ->
    ctx:Ctx.t ->
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
    (Dom.t * Ctx.t) list * t
  (** query for the abstract state at some [loc] under _any_ reachable [entry_state] precondition,
        exploring back to the specified [entrypoints]
    *)
end
