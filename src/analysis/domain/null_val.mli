(* open Syntax *)

include Abstract.Val

val not_null : t

val top : t

val is_null_or_bot : t -> bool

val is_null_or_top : t -> bool
