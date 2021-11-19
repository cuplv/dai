open Dai.Import
open Domain
open Frontend
open Syntax

module type Sig = sig
  type absstate

  type t

  (** names are opaque from outside a DAIG, except for comparison, hashing, and sexp utilities (for use in hashsets/maps) *)
  module Name : sig
    type t [@@deriving compare, hash, sexp_of]

    val pp : t pp
  end

  val of_cfg : entry_state:absstate -> cfg:Cfg.t -> fn:Cfg.Fn.t -> t
  (** Construct a DAIG for a procedure with body [cfg] and metadata [fn], with [init_state] at the procedure entry *)

  val apply_edit :
    daig:t -> cfg_edit:Tree_diff.cfg_edit_result -> fn:Cfg.Fn.t -> Tree_diff.edit -> t
  (** apply the specified [Tree_diff.edit] to the input [daig]; [cfg_edit] and [fn] are passed as additional information needed for certain types of edit *)

  val dirty : Name.t -> t -> t
  (** dirty all dependencies of some name (including that name itself) *)

  val dump_dot : filename:string -> ?loc_labeller:(Cfg.Loc.t -> string option) -> t -> unit
  (** dump a DOT representation of a DAIG to [filename], decorating abstract-state cells according to [loc_labeller] if provided *)

  val is_solved : Cfg.Loc.t -> t -> bool
  (** true iff an abstract state is available at the given location *)

  type 'a or_summary_query =
    | Result of 'a
    | Summ_qry of { callsite : Ast.Stmt.t; returnsite : Name.t; caller_state : absstate }
        (** sum type representing the possible cases when a query is issued to a DAIG:
        (case 1: Result) the result is available or can be computed with no new method summaries
        (case 2: Summ_qry) additional method summaries are needed to evaluate some [callsite] in [caller_state]
        *)

  type summarizer = callsite:Ast.Stmt.t * Name.t -> absstate -> absstate option

  exception Ref_not_found of [ `By_loc of Cfg.Loc.t | `By_name of Name.t ]

  val get_by_loc : ?summarizer:summarizer -> Cfg.Loc.t -> t -> absstate or_summary_query * t

  val get_by_name : ?summarizer:summarizer -> Name.t -> t -> absstate or_summary_query * t
  (** GET functions attempt to compute the requested value, analyzing its backward dependencies *)

  val read_by_loc : Cfg.Loc.t -> t -> absstate option

  val read_by_name : Name.t -> t -> absstate option
  (** READ functions return the current contents of the requested cell, performing no analysis computation*)

  val write_by_name : Name.t -> absstate -> t -> t

  val write_by_loc : Cfg.Loc.t -> absstate -> t -> t
  (** WRITE functions write the given [absstate] to the cell named by the given [Name.t], dirtying any forward dependencies *)

  val pred_state_exn : Name.t -> t -> absstate
  (** returns the predecessor absstate of the cell named by the given [Name.t], if there is exactly one *)

  val pred_stmt : Name.t -> t -> Ast.Stmt.t option

  val assert_wf : t -> unit

  val total_astate_refs : t -> int

  val nonempty_astate_refs : t -> int

  val dirty_by_loc : Cfg.Loc.t -> t -> t

  val add_assumefalse_edge : to_:Cfg.Loc.t -> from:Cfg.Loc.t -> t -> t

  val reachable_callsites : Cfg.Loc.t -> t -> Ast.Stmt.t list

  val recursive_call_return_sites :
    t -> cg:Frontend.Callgraph.t -> self:Cfg.Fn.t -> (Ast.Stmt.t * Name.t) list
end

module Make (Dom : Abstract.Dom) : Sig with type absstate := Dom.t
