open Dai.Import
open Syntax.Cfg

type loc_ctx = { entry : Loc.t; exit : Loc.t; ret : Loc.t}
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

val remove_fn : t -> string -> t
(** remove the [loc_ctx] for an entire method named by the [string] arg *)

val remove_region : string -> Syntax.Cfg.Loc.Set.t -> t -> t
(** remove all cached [loc_ctx]'s within the given CFG region (for a method named by the [string] arg) *)

val rebase_edges : string -> old_src:Loc.t -> new_src:Loc.t -> t -> t
(** for any [loc_ctx] where [entry]==[old_src], change [entry] to [new_src] (for handling statement insertion edits) *)

val union : t -> t -> t

val pp : t pp
