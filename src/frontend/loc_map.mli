open Dai.Import

type loc = Dai.Cfg.Loc.t
(** a single CFG location *)

type loc_ctx = { entry : loc; exit : loc; ret : loc }
(** the necessary context to build a CFG region for some code: an entry location, exit location, and containing-method return location *)

type t
(** a cache containing the CFG-location context for each statement of a parse tree *)

val empty : t
(** an empty cache *)

val add : string -> Tree_sitter_java.CST.statement -> loc_ctx -> t -> t
(** add [loc_ctx] for a [statement] in a method named by the [string] to some cache.  Side-effect-free. *)

val get : string -> Tree_sitter_java.CST.statement -> t -> loc_ctx
(** get the [loc_ctx] for some [statement] in a method named by the [string] *)

val remove : string -> Tree_sitter_java.CST.statement -> t -> t
(** remove the [loc_ctx] for some [statement] in a method named by the [string] *)

val pp : t pp
