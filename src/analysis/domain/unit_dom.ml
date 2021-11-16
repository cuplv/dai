open Dai
open Import
open Syntax

type t = unit

type stmt = Ast.Stmt.t [@@deriving compare, equal, sexp_of]

let is_bot () = true

let widen () () = ()

let join () () = ()

let implies () () = true

let ( <= ) = implies

let interpret _ () = ()

let init () = ()

let bottom () = ()

let top () = ()

let pp fs () = Format.fprintf fs "()"

let sanitize () = ()

let show () = "()"

let hash seed () = seed

let compare () () = 0

let equal () () = true

let sexp_of_t () = Sexp.Atom "()"

let t_of_sexp _ = ()

let hash_fold_t seed () = seed

let call ~callee:_ ~caller:_ ~callsite:_ ~caller_state:_ ~fields:_ = ()

let return ~callee:_ ~caller:_ ~callsite:_ ~caller_state:_ ~return_state:_ ~fields:_ = ()

let approximate_missing_callee ~caller_state:_ ~callsite:_ = ()
