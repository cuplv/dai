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

  val ( <= ) : t -> t -> bool

  (* infix alias of [implies] *)

  val eval_binop : t -> Ast.Binop.t -> t -> t

  val eval_unop : Ast.Unop.t -> t -> t

  val of_lit : Ast.Lit.t -> t

  val truthiness : t -> [ `Neither | `T | `F | `Either ]

  val models : t -> Ast.Lit.t -> bool
end

module type Dom = sig
  type t [@@deriving compare, equal, hash, sexp]

  include Adapton.Data.S with type t := t

  val pp : t pp

  (* [unit -> t] type allows for lazy apron manager allocation, unlike [t] *)
  val init : unit -> t

  val bottom : unit -> t

  val interpret : Ast.Stmt.t -> t -> t

  val implies : t -> t -> bool

  val ( <= ) : t -> t -> bool

  (* infix alias of [implies] *)

  val join : t -> t -> t

  val widen : t -> t -> t

  val is_bot : t -> bool

  val call :
    callee:Cfg.Fn.t -> callsite:Ast.Stmt.t -> caller_state:t -> fields:Declared_fields.t -> t

  val return :
    callee:Cfg.Fn.t ->
    callsite:Ast.Stmt.t ->
    caller_state:t ->
    return_state:t ->
    fields:Declared_fields.t ->
    t

  val approximate_missing_callee : caller_state:t -> callsite:Ast.Stmt.t -> t
end

module type CtxSensitiveDom = sig
  include Dom

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

module DomWithDataStructures (T : sig
  include Dom

  val compare : t -> t -> int

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end) : sig
  include Dom with type t = T.t

  include Comparator.S with type t := T.t

  module Set : sig
    type absstate := T.t

    type t = (absstate, comparator_witness) Set.t

    val empty : t
  end

  module Map : sig
    type absstate := T.t

    type 'v t = (absstate, 'v, comparator_witness) Map.t

    val empty : 'v t
  end
end = struct
  module T_comparator = struct
    include Comparator.Make (T)
    include T
  end

  include T_comparator

  module Set = struct
    include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

    type t = Set.M(T_comparator).t [@@deriving compare]

    let empty = Set.empty (module T_comparator)
  end

  module Map = struct
    include (Map : module type of Map with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) Map.t)

    type 'v t = 'v Map.M(T_comparator).t

    let empty = Map.empty (module T_comparator)
  end
end
