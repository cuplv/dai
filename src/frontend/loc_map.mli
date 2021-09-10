open Dai.Import
open Syntax
open Cfg

type loc_ctx = { entry : Loc.t; exit : Loc.t; ret : Loc.t; exc : Loc.t }
(** the necessary context to build a CFG region for some code: an entry location, exit location, and containing-method return location and exceptional-exit location *)

type t
(** a cache containing the CFG-location context for each statement of a parse tree *)

val empty : t
(** an empty cache *)

type 'a or_collision = [ `Ok of 'a | `Collision ]

val add : Method_id.t -> Tree_sitter_java.CST.statement -> loc_ctx -> t -> t or_collision
(** add [loc_ctx] for a [statement] in a method identified by the [Method_id.t].
    If that statement is syntactically identical to another statement in the method, return [`Collision].
 *)

val get : Method_id.t -> Tree_sitter_java.CST.statement -> t -> loc_ctx
(** get the [loc_ctx] for some [statement] in a method identified by the [Method_id.t] *)

val remove : Method_id.t -> Tree_sitter_java.CST.statement -> t -> t
(** remove the [loc_ctx] for some [statement] in a method identified by the [Method_id.t] *)

val remove_fn : t -> Method_id.t -> t
(** remove the [loc_ctx] for an entire method identified by the [Method_id.t] *)

val remove_region : Method_id.t -> Syntax.Cfg.Loc.Set.t -> t -> t
(** remove all cached [loc_ctx]'s within the given CFG region (for a method identified by the [Method_id.t]) *)

val rebase_edges : Method_id.t -> old_src:Loc.t -> new_src:Loc.t -> t -> t
(** for any [loc_ctx] where [entry]==[old_src], change [entry] to [new_src] (for handling statement insertion edits) *)

val union : t -> t -> t

val pp : t pp
