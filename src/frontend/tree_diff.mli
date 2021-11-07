open Dai
open Syntax
open Tree_sitter_java

type edit =
  | Add_function of {
      method_id : Method_id.t;
      decl : CST.class_body_declaration;
      instance_init : CST.program option;
    }
  | Delete_function of { method_id : Method_id.t }
  | Modify_function of { method_id : Method_id.t; new_header : CST.method_header }
  | Add_statements of { method_id : Method_id.t; at_loc : Cfg.Loc.t; stmts : CST.statement list }
  | Modify_statements of {
      method_id : Method_id.t;
      from_loc : Cfg.Loc.t;
      to_loc : Cfg.Loc.t;
      new_stmts : CST.statement list;
    }
  | Modify_header of {
      method_id : Method_id.t;
      prev_loc_ctx : Loc_map.loc_ctx;
      next_stmt : CST.statement;
      loop_body_exit : Cfg.Loc.t option;
    }
  | Delete_statements of { method_id : Method_id.t; from_loc : Cfg.Loc.t; to_loc : Cfg.Loc.t }

val method_id_of_edit : edit -> Method_id.t

type t = edit list

val pp : t Import.pp

val btwn : Loc_map.t -> prev:Tree.java_cst -> next:Tree.java_cst -> t

val apply :
  Class_hierarchy.t -> t -> Loc_map.t -> Cfg.t Cfg.Fn.Map.t -> Loc_map.t * Cfg.t Cfg.Fn.Map.t

type cfg_edit_result = {
  cfg : Cfg.t;
  new_loc_map : Loc_map.t;
  added_edges : Cfg_parser.edge list;
  deleted_edges : Cfg.G.edge list;
  added_loc : Cfg.Loc.t option;
  added_for_loop_backedge : Cfg_parser.edge option;
}

val apply_edit : edit -> Loc_map.t -> Cfg.t -> ret:Cfg.Loc.t -> exc:Cfg.Loc.t -> cfg_edit_result
