module type Dom = sig
  type t [@@deriving compare, hash]

  include Adapton.Data.S with type t := t

  val init : t

  val interpret : Ast.Stmt.t -> t -> t

  val implies : t -> t -> bool

  val join : t -> t -> t

  val widen : t -> t -> t
end
