(* open Syntax *)

type nullness = Top | Null | NotNull | Bot

include Abstract.Val with type t = nullness

val is_null_or_bot : t -> bool

val is_null_or_top : t -> bool
