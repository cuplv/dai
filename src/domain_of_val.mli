open Import

module type Abstract_value = sig
  include Adapton.Data.S
  val pp : t pp
  val join : t -> t -> t
  val widen : t -> t -> t
  val implies : t -> t -> bool
  val eval_binop : t -> Ast.Binop.t -> t -> t
  val eval_unop : Ast.Unop.t -> t -> t
  val of_lit : Ast.Lit.t -> t
  val truthiness : t -> [`Neither | `T | `F | `Either]
end


module Make (Val : Abstract_value) : Domain_intf.Dom
