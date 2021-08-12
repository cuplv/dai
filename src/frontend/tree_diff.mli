open Dai
open Syntax

type t

val pp : t Import.pp

val btwn : Loc_map.t -> prev:Tree.java_cst -> next:Tree.java_cst -> t

val apply : t -> Loc_map.t -> Cfg.t -> Loc_map.t * Cfg.t

