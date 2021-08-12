open Dai
open Import
open Syntax

module type Val = sig
  type t [@@deriving compare, equal, hash, sexp]

  include Adapton.Data.S with type t := t

  val pp : t pp

  val join : t -> t -> t

  val widen : t -> t -> t

  val implies : t -> t -> bool

  val eval_binop : t -> Ast.Binop.t -> t -> t

  val eval_unop : Ast.Unop.t -> t -> t

  val of_lit : Ast.Lit.t -> t

  val truthiness : t -> [ `Neither | `T | `F | `Either ]

  val models : t -> Ast.Lit.t -> bool
end

module type DomNoCtx = sig
  type t [@@deriving compare, equal, hash, sexp]

  include Adapton.Data.S with type t := t

  val pp : t pp

  (* [unit -> t] type allows for lazy apron manager allocation, unlike [t] *)
  val init : unit -> t

  val interpret : Ast.Stmt.t -> t -> t

  val implies : t -> t -> bool

  val join : t -> t -> t

  val widen : t -> t -> t

  val is_bot : t -> bool

  (*  val forget : var:string -> t -> t*)
  (* TODO: rename [project] to [project_locals]? For handling heap side effects in interprocedural shape analysis, might be an important distinction.*)
  (* val project : vars:string list -> t -> t *)
  val handle_return :
    caller_state:t -> return_state:t -> callsite:Ast.Stmt.t -> callee_defs:string list -> t
end

module type Dom = sig
  include DomNoCtx

  module Ctx : sig
    type dom = t

    type t [@@deriving compare, equal, hash, sexp_of]

    include Adapton.Data.S with type t := t

    val pp : t pp

    val init : t

    val callee_ctx : caller_state:dom -> callsite:Ast.Stmt.t -> ctx:t -> t

    (* Given a context and a callchain, returns true if the context MAY be reached via the callchain.  This signature allows syntactic context sensitivity policies (e.g. kCFA) to filter out infeasible chains. (e.g. in 1CFA with context f, those not ending in f)
     *)
    val is_feasible_callchain : t -> Ast.Stmt.t list -> bool
  end
end
