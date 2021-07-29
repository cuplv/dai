type t

type java_cst = Tree_sitter_java.CST.program

val parse : old_tree:t option -> file:Src_file.t -> (t, 'a) result
(** parse the contents of [file], doing so incrementally if an [old_tree] is provided *)

val apply : Text_diff.t list -> offsets:int list -> t -> t
(** Update the in-memory representation of input tree with new offsets according to this (textual, not tree!) diff.
    This operation MUST be performed before [Parse.parse] can be invoked incrementally.
*)

val as_java_cst : Src_file.t -> t -> (java_cst, Tree_sitter_run.Tree_sitter_error.t list) result
(** Convert a raw tree-sitter tree (opaque wrapper around C struct) to a typed OCaml-native concrete syntax tree *)
