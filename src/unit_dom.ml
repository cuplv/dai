open Import

module T = struct
  type t = unit

  type stmt = Ast.Stmt.t [@@deriving compare, equal, sexp_of]

  module Stmt = Ast.Stmt

  let is_bot () = true

  let widen () () = ()

  let join () () = ()

  let implies () () = true

  let interpret _ () = ()

  let init () = ()

  let pp fs () = Format.fprintf fs "()"

  let sanitize () = ()

  let show () = "()"

  let hash seed () = seed

  let compare () () = 0

  let equal () () = true

  let sexp_of_t () = Sexp.Atom "()"

  let t_of_sexp _ = ()

  let hash_fold_t seed () = seed

  let handle_return ~caller_state:_ ~return_state:_ ~callsite:_ ~callee_defs:_ = failwith "todo"
end

include Context.MakeInsensitive (T)
