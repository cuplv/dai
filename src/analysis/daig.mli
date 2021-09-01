open Domain
open Frontend
open Syntax

module Make (Dom : Abstract.Dom) : sig
  type t

  val of_cfg : init_state:Dom.t -> cfg:Cfg.t -> fn:Cfg.Fn.t -> t
  (** Construct a DAIG for a procedure with body [cfg] and metadata [fn], with [init_state] at the procedure entry *)

  val apply_edit :
    daig:t -> cfg_edit:Tree_diff.cfg_edit_result -> fn:Cfg.Fn.t -> Tree_diff.edit -> t
  (** apply the specified [Tree_diff.edit] to the input [daig]; [cfg_edit] and [fn] are passed as additional information needed for certain types of edit *)

  val dump_dot : filename:string -> t -> unit
  (** dump a DOT representation of a DAIG to [filename] *)

  type 'a or_summary_query =
    | Result of 'a
    | Summ_qry of { callsite : Ast.Stmt.t; caller_state : Dom.t }
        (** sum type representing the possible cases when a query is issued to a DAIG:
      (case 1: Result) the result is available or can be computed with no new method summaries
      (case 2: Summ_qry) additional method summaries are needed to evaluate some [callsite] in [caller_state]
  *)

  val get_by_loc : Cfg.Loc.t -> t -> Dom.t or_summary_query * t
  (** Get the fixed-point abstract state at some program location in some DAIG (or query for requisite new method summaries)*)
end
