open Import

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

module type Dom = sig
  type t [@@deriving compare, equal, hash, sexp]

  include Adapton.Data.S with type t := t

  module Stmt : sig
    type t [@@deriving compare, equal, sexp_of]

    (*    val pp : t pp*)
    val to_string : t -> string

    val skip : t

    include Adapton.Data.S with type t := t
  end

  val pp : t pp

  (* [unit -> t] type allows for lazy apron manager allocation, unlike [t] *)
  val init : unit -> t

  val interpret : Stmt.t -> t -> t

  val implies : t -> t -> bool

  val join : t -> t -> t

  val widen : t -> t -> t

  val is_bot : t -> bool
end
