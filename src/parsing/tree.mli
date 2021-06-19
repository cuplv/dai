type t

val parse : old_tree:t option -> file:Src_file.t -> (t, string) result
(** parse the contents of [file], doing so incrementally if an [old_tree] is provided *)

val apply : Text_diff.t list -> offsets:int list -> t -> t
(** Update the in-memory representation of input tree with new offsets according to this (textual, not tree!) diff.
    This operation MUST be performed before [Parse.parse] can be invoked incrementally.
*)
