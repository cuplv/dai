open Dai
open Syntax
open Tree_sitter_java

type edit =
  | Add_function of { method_id : string; method_decl : CST.method_declaration }
  | Delete_function of { method_id : string }
  | Modify_function of { method_id : string; new_header : CST.method_header }
  | Add_statements of { method_id : string; at_loc : Cfg.Loc.t; stmts : CST.statement list }
  | Modify_statements of {
      method_id : string;
      from_loc : Cfg.Loc.t;
      to_loc : Cfg.Loc.t;
      new_stmts : CST.statement list;
    }
  | Modify_header of {
      method_id : string;
      at_loc : Cfg.Loc.t;
      stmt : CST.statement;
      loop_body_exit : Cfg.Loc.t option;
    }
  | Delete_statements of { method_id : string; from_loc : Cfg.Loc.t; to_loc : Cfg.Loc.t }

type t = edit list

val pp : t Import.pp

val btwn : Loc_map.t -> prev:Tree.java_cst -> next:Tree.java_cst -> t

val apply : t -> Loc_map.t -> Cfg.t Cfg.Fn.Map.t -> Loc_map.t * Cfg.t Cfg.Fn.Map.t

type cfg_edit_result = {
  cfg : Cfg.t;
  new_loc_map : Loc_map.t;
  added_edges : Cfg_parser.edge list;
  deleted_edges : Cfg.G.edge list;
  added_loc : Cfg.Loc.t option;
  added_for_loop_backedge : Cfg_parser.edge option;
}

val apply_edit : edit -> Loc_map.t -> Cfg.t -> ret:Cfg.Loc.t -> cfg_edit_result
