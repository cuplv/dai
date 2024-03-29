open Dai.Import
open Syntax
open Tree_sitter_java

type edit =
  | Add_function of {
      method_id : Method_id.t;
      decl : CST.class_body_declaration;
      init_info : init_info;
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

and init_info = { instance_init : CST.program option; field_decls : CST.field_declaration list }

let empty_init_info = { instance_init = None; field_decls = [] }

let method_id_of_edit = function
  | Add_function { method_id; _ }
  | Delete_function { method_id; _ }
  | Modify_function { method_id; _ }
  | Add_statements { method_id; _ }
  | Modify_statements { method_id; _ }
  | Modify_header { method_id; _ }
  | Delete_statements { method_id; _ } ->
      method_id

type t = edit list

let pp_edit fs = function
  | Add_function { method_id; decl = _; init_info = _ } ->
      Format.fprintf fs "(Add function %a)" Method_id.pp method_id
  | Delete_function { method_id } -> Format.fprintf fs "(Delete function %a)" Method_id.pp method_id
  | Modify_function { method_id; new_header = _ } ->
      Format.fprintf fs "(Modify function %a's header)" Method_id.pp method_id
  | Add_statements { method_id; at_loc; stmts } ->
      Format.fprintf fs "(Add %i statements at %a in %a)" (List.length stmts) Cfg.Loc.pp at_loc
        Method_id.pp method_id
  | Modify_statements { method_id; from_loc; to_loc; new_stmts } ->
      Format.fprintf fs "(Overwrite range %a->%a with %i new stmts in %a)" Cfg.Loc.pp from_loc
        Cfg.Loc.pp to_loc (List.length new_stmts) Method_id.pp method_id
  | Modify_header { method_id; prev_loc_ctx; next_stmt; _ } ->
      let control_flow_type =
        match next_stmt with
        | `If_stmt _ -> "conditional"
        | `While_stmt _ -> "while-loop"
        | `For_stmt _ -> "for-loop"
        | _ -> failwith "unrecognized control-flow construct"
      in
      Format.fprintf fs "(Modify %s header at %a in %a)" control_flow_type Cfg.Loc.pp
        prev_loc_ctx.entry Method_id.pp method_id
  | Delete_statements { method_id; from_loc; to_loc } ->
      Format.fprintf fs "(Delete statements in range %a->%a in %a)" Cfg.Loc.pp from_loc Cfg.Loc.pp
        to_loc Method_id.pp method_id

let pp = List.pp ~pre:"DIFF:[@[<hv 2>" ~suf:"@]]" ";@," pp_edit

module Stmt_patdiff = Patience_diff_lib.Patience_diff.Make (struct
  type t = CST.statement [@@deriving sexp_of]

  let t_of_sexp _ = failwith "unimplemented"

  let compare s1 s2 = Sexp.compare (sexp_of_t s1) (sexp_of_t s2)

  let hash = sexp_of_t >> Sexp.hash
end)

module Hunk = Patience_diff_lib.Patience_diff.Hunk
module Range = Patience_diff_lib.Patience_diff.Range

let loc_range method_id loc_map stmts =
  assert (Array.length stmts > 0);
  Array.fold stmts ~init:None ~f:(fun removed_range curr_stmt ->
      let ({ entry = curr_entry; exit = curr_exit; ret = _; exc = _ } : Loc_map.loc_ctx) =
        Loc_map.get method_id curr_stmt loc_map
      in
      match removed_range with
      | None -> Some (curr_entry, curr_exit)
      | Some (acc_range_start, acc_range_end) ->
          assert (Cfg.Loc.equal curr_entry acc_range_end);
          Some (acc_range_start, curr_exit))
  |> fun range_opt -> Option.value_exn range_opt

let rec diff_of_stmt_list method_id loc_map ~(prev : CST.statement list)
    ~(next : CST.statement list) : t =
  let open Array in
  let prev = of_list prev in
  let next = of_list next in
  let hunk =
    match
      Stmt_patdiff.get_hunks ~transform:(fun x -> x) ~context:(-1) ~big_enough:3 ~prev ~next
    with
    | [ hunk ] -> hunk
    | _ -> failwith "unreachable; context<0 guarantees singleton hunk list per patdiff docs"
  in
  let ranges = Hunk.ranges hunk in
  let loc_by_idx idx =
    let pred_opt =
      match List.nth ranges (idx - 1) with
      | Some (Same preds) -> Some (last preds |> fst)
      | Some (Replace (preds, _)) | Some (Prev preds) -> Some (last preds)
      | _ -> None
    in
    match pred_opt with
    | Some pred ->
        Loc_map.get method_id pred loc_map |> fun { entry = _; exit; ret = _; exc = _ } -> exit
    | None ->
        let succ =
          match List.nth ranges (idx + 1) with
          | Some (Same succs) -> get succs 0 |> fst
          | Some (Replace (succs, _)) | Some (Prev succs) -> get succs 0
          | _ -> failwith "unable to find CFG location for \"Next\" range from patdiff"
        in
        Loc_map.get method_id succ loc_map |> fun { entry; exit = _; ret = _; exc = _ } -> entry
  in
  List.foldi ranges ~init:[] ~f:(fun idx acc_edits -> function
    | Prev stmts ->
        let from_loc, to_loc = loc_range method_id loc_map stmts in
        Delete_statements { method_id; from_loc; to_loc } :: acc_edits
    | Next stmts ->
        Add_statements { method_id; at_loc = loc_by_idx idx; stmts = to_list stmts } :: acc_edits
    | Replace (prev_stmts, next_stmts) ->
        flip ( @ ) acc_edits
          (match (prev_stmts, next_stmts) with
          | [||], [||] -> failwith "malformed diff: empty replacement"
          | ( [| `If_stmt (_, (_, prev_cond, _), prev_t_branch, prev_f_branch) |],
              [| `If_stmt (_, (_, next_cond, _), next_t_branch, next_f_branch) |] )
            when Sexp.equal (CST.sexp_of_expression prev_cond) (CST.sexp_of_expression next_cond) ->
              let prev_t_branch =
                match prev_t_branch with `Blk (_, b, _) -> b | stmt -> [ stmt ]
              in
              let next_t_branch =
                match next_t_branch with `Blk (_, b, _) -> b | stmt -> [ stmt ]
              in
              let t_branch_diff =
                diff_of_stmt_list method_id loc_map ~prev:prev_t_branch ~next:next_t_branch
              in
              let prev_f_branch =
                match prev_f_branch with
                | None -> []
                | Some (_else, `Blk (_, b, _)) -> b
                | Some (_else, stmt) -> [ stmt ]
              in
              let next_f_branch =
                match next_f_branch with
                | None -> []
                | Some (_else, `Blk (_, b, _)) -> b
                | Some (_else, stmt) -> [ stmt ]
              in
              let f_branch_diff =
                diff_of_stmt_list method_id loc_map ~prev:prev_f_branch ~next:next_f_branch
              in
              t_branch_diff @ f_branch_diff
          | ( [| `While_stmt (_, (_, prev_cond, _), prev_body) |],
              [| `While_stmt (_, (_, next_cond, _), next_body) |] )
            when Sexp.equal (CST.sexp_of_expression prev_cond) (CST.sexp_of_expression next_cond) ->
              let prev = match prev_body with `Blk (_, b, _) -> b | stmt -> [ stmt ] in
              let next = match next_body with `Blk (_, b, _) -> b | stmt -> [ stmt ] in
              diff_of_stmt_list method_id loc_map ~prev ~next
          | ( [| `For_stmt (_, _, prev_init, prev_cond, _, prev_iter, _, prev_body) |],
              [| `For_stmt (_, _, next_init, next_cond, _, next_iter, _, next_body) |] )
            when (match (prev_init, next_init) with
                 | `Local_var_decl p, `Local_var_decl n ->
                     Sexp.equal
                       (CST.sexp_of_local_variable_declaration p)
                       (CST.sexp_of_local_variable_declaration n)
                 | `Opt_exp_rep_COMMA_exp_SEMI (None, _), `Opt_exp_rep_COMMA_exp_SEMI (None, _) ->
                     true
                 | `Opt_exp_rep_COMMA_exp_SEMI (Some p, _), `Opt_exp_rep_COMMA_exp_SEMI (Some n, _)
                   ->
                     Sexp.equal
                       (CST.sexp_of_anon_exp_rep_COMMA_exp_0bb260c p)
                       (CST.sexp_of_anon_exp_rep_COMMA_exp_0bb260c n)
                 | _ -> false)
                 && (match (prev_cond, next_cond) with
                    | None, None -> true
                    | Some p, Some n ->
                        Sexp.equal (CST.sexp_of_expression p) (CST.sexp_of_expression n)
                    | _ -> false)
                 &&
                 match (prev_iter, next_iter) with
                 | None, None -> true
                 | Some p, Some n ->
                     Sexp.equal
                       (CST.sexp_of_anon_exp_rep_COMMA_exp_0bb260c p)
                       (CST.sexp_of_anon_exp_rep_COMMA_exp_0bb260c n)
                 | _ -> false ->
              let prev = match prev_body with `Blk (_, b, _) -> b | stmt -> [ stmt ] in
              let next = match next_body with `Blk (_, b, _) -> b | stmt -> [ stmt ] in
              diff_of_stmt_list method_id loc_map ~prev ~next
          | prevs, nexts ->
              let from_loc, to_loc = loc_range method_id loc_map prevs in
              [ Modify_statements { method_id; from_loc; to_loc; new_stmts = to_list nexts } ])
    | Same _ -> acc_edits
    | Unified _ -> failwith "Unrecognized diff type: \"Unified\"")

let rec decls_by_id ?(package = []) ?(parent_class = None) :
    CST.statement -> (Method_id.t * CST.class_body_declaration) list =
  let parent_class_prefix = match parent_class with Some n -> n ^ "#" | None -> "" in
  function
  | `Decl (`Class_decl (_, _, (_, class_name), _, _, _, (_, body_decls, _))) ->
      List.fold body_decls ~init:[] ~f:(fun acc -> function
        | `Meth_decl (modifiers, (_, _, (`Id (_, method_name), formals, _), _), _) as md ->
            let arg_types = Cfg_parser.types_of_formals formals in
            let static =
              Option.exists modifiers
                ~f:(List.exists ~f:(function `Static _ -> true | _ -> false))
            in
            let method_id : Method_id.t =
              {
                package;
                class_name = parent_class_prefix ^ class_name;
                method_name;
                static;
                arg_types;
              }
            in
            (method_id, md) :: acc
        | `Cons_decl (_, (_, _, formals), _, _) as cd ->
            let arg_types = Cfg_parser.types_of_formals formals in
            let method_id : Method_id.t =
              {
                package;
                class_name = parent_class_prefix ^ class_name;
                method_name = "<init>";
                static = false;
                arg_types;
              }
            in
            (method_id, cd) :: acc
        | `Class_decl _ as cd ->
            let nested_method_decls =
              decls_by_id ~package
                ~parent_class:(Some (parent_class_prefix ^ class_name))
                (`Decl cd)
            in
            nested_method_decls @ acc
        | `Static_init _ as si ->
            let method_id : Method_id.t =
              {
                package;
                class_name = parent_class_prefix ^ class_name;
                method_name = "<staticinit>";
                static = true;
                arg_types = [];
              }
            in
            (method_id, si) :: acc
        | _ -> acc)
  | `Decl (`Anno_type_decl _)
  | `SEMI _
  | `Decl (`Enum_decl _)
  | `Decl (`Import_decl _)
  | `Decl (`Inte_decl _)
  | `Decl (`Pack_decl _) ->
      []
  | _ -> failwith "unrecognized top-level definition"

let init_info class_name (cst : Tree.java_cst) =
  (* recursively traverse nested class decls until finding one with [class_name]; at that point, return its instance init block if it exists *)
  let rec impl containing_class = function
    | `Decl (`Class_decl (_, _, (_, name), _, _, _, (_, body, _))) :: rest -> (
        let curr_class_name =
          match containing_class with Some n -> n ^ "$" ^ name | None -> name
        in
        if String.equal class_name curr_class_name then
          let instance_init =
            List.find_map body ~f:(function `Blk (_, b, _) -> Some b | _ -> None)
          in
          let field_decls = Cfg_parser.instance_field_decls body in
          Some { instance_init; field_decls }
        else
          match
            List.find_map body ~f:(function
              | `Class_decl _ as cd -> impl (Some curr_class_name) [ `Decl cd ]
              | _ -> None)
          with
          | None -> impl containing_class rest
          | Some _ as nested_instance_init -> nested_instance_init)
    | _ :: rest -> impl containing_class rest
    | [] -> None
  in
  impl None cst |> Option.value ~default:empty_init_info

let btwn loc_map ~(prev : Tree.java_cst) ~(next : Tree.java_cst) =
  let package =
    let prev_package = Cfg_parser.package_of_cst prev in
    let next_package = Cfg_parser.package_of_cst next in
    if (List.equal String.equal) prev_package next_package then prev_package
    else failwith "todo: deal with package-renaming edit"
  in
  let open List.Monad_infix in
  let prev_decls_by_id = prev >>= decls_by_id ~package |> Map.of_alist_exn (module Method_id) in
  let next_decls_by_id = next >>= decls_by_id ~package |> Map.of_alist_exn (module Method_id) in
  let prev_ids = Map.keys prev_decls_by_id |> Set.of_list (module Method_id) in
  let next_ids = Map.keys next_decls_by_id |> Set.of_list (module Method_id) in
  let shared_ids = Method_id.Set.inter prev_ids next_ids in
  let deleted_ids, added_ids =
    Method_id.Set.symmetric_diff prev_ids next_ids
    |> Sequence.fold ~init:([], []) ~f:(fun (deleted, added) -> function
         | First id -> (id :: deleted, added) | Second id -> (deleted, id :: added))
  in
  let function_additions =
    List.map added_ids ~f:(fun method_id ->
        let decl = Method_id.Map.find_exn next_decls_by_id method_id in
        let init_info =
          match decl with
          | `Cons_decl _ -> init_info method_id.class_name next
          | _ -> empty_init_info
        in
        Add_function { method_id; decl; init_info })
  in
  let function_deletions =
    List.map deleted_ids ~f:(fun method_id -> Delete_function { method_id })
  in
  let function_header_modifications =
    Set.fold shared_ids ~init:[] ~f:(fun acc method_id ->
        let prev_decl = Method_id.Map.find_exn prev_decls_by_id method_id in
        let next_decl = Method_id.Map.find_exn next_decls_by_id method_id in
        match (prev_decl, next_decl) with
        | `Meth_decl (_, prev_header, _), `Meth_decl (_, next_header, _) ->
            if
              Sexp.equal
                (CST.sexp_of_method_header prev_header)
                (CST.sexp_of_method_header next_header)
            then acc
            else Modify_function { method_id; new_header = next_header } :: acc
        | `Static_init _, `Static_init _ -> acc
        | `Cons_decl _, `Cons_decl _ -> acc
        | _ -> failwith "mismatched or unrecognized declarations with same method_id")
  in
  let stmt_edits =
    let stmts_of_decl = function
      | `Meth_decl (_, _, `Blk (_, stmts, _)) -> stmts
      | `Meth_decl (_, _, `SEMI _) -> []
      | `Cons_decl (_, _, _, (_, _, stmts, _)) -> stmts
      | `Static_init (_, (_, stmts, _)) -> stmts
      | _ ->
          failwith
            "unexpected class body declaration in stmt-diff: not a method, static init block, or \
             constructor"
    in
    Set.fold shared_ids ~init:[] ~f:(fun acc method_id ->
        let prev = Method_id.Map.find_exn prev_decls_by_id method_id |> stmts_of_decl in
        let next = Method_id.Map.find_exn next_decls_by_id method_id |> stmts_of_decl in
        diff_of_stmt_list method_id loc_map ~prev ~next @ acc)
  in
  stmt_edits @ function_additions @ function_deletions @ function_header_modifications

type cfg_edit_result = {
  cfg : Cfg.t;
  new_loc_map : Loc_map.t;
  added_edges : Cfg_parser.edge list;
  deleted_edges : Cfg.G.edge list;
  added_loc : Cfg.Loc.t option;
  added_for_loop_backedge : Cfg_parser.edge option;
}

let apply_edit edit loc_map cfg ~ret ~exc : cfg_edit_result =
  let edit_result ?added_loc ?(added_for_loop_backedge = None) ?(added_edges = [])
      ?(deleted_edges = []) cfg new_loc_map : unit -> cfg_edit_result =
   fun () -> { cfg; new_loc_map; added_edges; deleted_edges; added_loc; added_for_loop_backedge }
  in
  match edit with
  | Add_function _ | Delete_function _ | Modify_function _ ->
      failwith "Can't apply interprocedural edit to intraprocedural CFG"
  | Add_statements { method_id; at_loc; stmts } ->
      (* for all CFG edges FROM [at_loc], replace their source by the [fresh_loc] (in both CFG and loc-map) *)
      (* generate CFG edges for [stmts] with entry=at_loc, exit=fresh_loc, ret=(method_id.exit) *)
      let fresh_loc = Cfg.Loc.fresh () in
      let old_succ_edges = Cfg.G.Node.outputs at_loc cfg in
      let new_succ_edges =
        Sequence.fold old_succ_edges ~init:[] ~f:(fun acc edge ->
            (fresh_loc, Cfg.dst edge, Cfg.G.Edge.label edge) :: acc)
      in
      let loc_map = Loc_map.rebase_edges method_id ~old_src:at_loc ~new_src:fresh_loc loc_map in
      let loc_map, added_edges =
        Cfg_parser.edge_list_of_stmt_list method_id loc_map ~entry:at_loc ~exit:fresh_loc ~ret ~exc
          stmts
      in
      let cfg =
        Sequence.fold old_succ_edges ~init:cfg ~f:(flip Cfg.G.Edge.remove) |> fun init ->
        List.fold new_succ_edges ~init ~f:(fun cfg edge ->
            Cfg.G.Edge.(insert (uncurry3 create edge) cfg))
        |> fun init ->
        List.fold added_edges ~init ~f:(fun cfg e -> Cfg.G.Edge.(insert (uncurry3 create e) cfg))
      in
      edit_result cfg loc_map ~added_edges ~added_loc:fresh_loc ()
  | Modify_statements { method_id; from_loc; to_loc; new_stmts } ->
      (* grab the region of edges reachable between from_loc / to_loc*)
      let deleted_edges = Cfg.edges_btwn cfg ~src:from_loc ~dst:to_loc |> Set.to_list in
      let deleted_locs =
        List.fold deleted_edges ~init:Cfg.Loc.Set.empty ~f:(fun locs edge ->
            Cfg.Loc.Set.(add (add locs (Cfg.src edge)) (Cfg.dst edge)))
      in
      let loc_map = Loc_map.remove_region method_id deleted_locs loc_map in
      (* generate CFG edges for [new_stmts] with entry=from_loc, exit=to_loc, ret=(method_id.exit) *)
      let loc_map, added_edges =
        Cfg_parser.edge_list_of_stmt_list method_id loc_map ~entry:from_loc ~exit:to_loc ~ret ~exc
          new_stmts
      in
      let cfg =
        List.fold deleted_edges ~init:cfg ~f:(flip Cfg.G.Edge.remove) |> fun init ->
        List.fold added_edges ~init ~f:(fun cfg e -> Cfg.G.Edge.(insert (uncurry3 create e) cfg))
      in
      edit_result cfg loc_map ~added_edges ~deleted_edges ()
  | Modify_header { method_id; prev_loc_ctx; next_stmt; loop_body_exit } ->
      (* Traverse from [at_loc] until reaching a control-flow fork (also grabbing the `update` expression of a for-loop by traversing from [loop_body_exit]);
          delete everything in range, then construct the replacement from [stmt]
      *)
      let rec count_negations expr =
        let open Ast in
        match expr with Expr.Unop { op = Unop.Not; e } -> 1 + count_negations e | _ -> 0
      in
      let rec extract_cond_header at_loc acc =
        match
          Cfg.G.Node.outputs at_loc cfg |> Sequence.to_list
          |> List.filter ~f:(Cfg.G.Edge.label >> Ast.Stmt.is_exc >> not)
        with
        | [ e ] -> extract_cond_header (Cfg.dst e) (e :: acc)
        | [ e1; e2 ] ->
            let e1_cond =
              match Cfg.G.Edge.label e1 with
              | Ast.Stmt.Assume cond -> cond
              | _ -> failwith "malformed conditional"
            in
            let e2_cond =
              match Cfg.G.Edge.label e2 with
              | Ast.Stmt.Assume cond -> cond
              | _ -> failwith "malformed conditional"
            in
            if count_negations e1_cond < count_negations e2_cond then (acc, e1, e2)
            else (acc, e2, e1)
        | _ -> failwith "malformed conditional"
      in
      let loc_map, deleted_edges, added_edges, added_for_loop_backedge =
        match next_stmt with
        | `If_stmt (_, (_, cond, _), _, _) | `While_stmt (_, (_, cond, _), _) ->
            (* traverse from at_loc until reaching a out-degree-2 node; delete all traversed edges and those 2 outgoing ones, then build the new edges for the new header *)
            let old_cond_intermediates, old_assume_pos, old_assume_neg =
              extract_cond_header prev_loc_ctx.entry []
            in
            let deleted_edges = old_assume_pos :: old_assume_neg :: old_cond_intermediates in
            let cond, (branch_loc, cond_intermediates) =
              Cfg_parser.expr ~curr_loc:prev_loc_ctx.entry ~exc:prev_loc_ctx.exc cond
            in
            let new_assume_pos =
              (branch_loc, Cfg.G.Edge.dst old_assume_pos, Ast.Stmt.Assume cond)
            in
            let new_assume_neg =
              ( branch_loc,
                Cfg.G.Edge.dst old_assume_neg,
                Ast.(Stmt.Assume (Expr.Unop { op = Unop.Not; e = cond })) )
            in
            let added_edges = new_assume_pos :: new_assume_neg :: cond_intermediates in
            (loc_map, deleted_edges, added_edges, None)
        | `For_stmt f ->
            let body_exit =
              match loop_body_exit with
              | Some l -> l
              | None -> failwith "no cached loop-body exit for for-loop header modification"
            in
            let init_edges, cond, cond_neg = extract_cond_header prev_loc_ctx.entry [] in
            let init_locs =
              List.fold init_edges ~init:Cfg.Loc.Set.empty ~f:(fun locs edge ->
                  Cfg.Loc.Set.(add (add locs (Cfg.src edge)) (Cfg.dst edge)))
            in
            let update_edges =
              let is_back_edge = Cfg.G.Edge.dst >> Cfg.Loc.Set.mem init_locs in
              let rec gather_update_edges acc curr =
                match Cfg.G.Node.outputs curr cfg |> Sequence.to_list with
                | [ e ] when Ast.Stmt.(equal Skip) (Cfg.G.Edge.label e) && is_back_edge e ->
                    e :: acc
                | [ e ] -> gather_update_edges (e :: acc) (Cfg.G.Edge.dst e)
                | _ -> failwith "unexpected conditional control flow in for-loop update"
              in
              gather_update_edges [] body_exit
            in
            let all_edges = cond :: cond_neg :: (update_edges @ init_edges) in
            let entry = prev_loc_ctx.entry in
            let exit = Cfg.G.Edge.dst cond_neg in
            let body_entry = Cfg.G.Edge.dst cond in
            let loc_map, new_header, back_edge =
              Cfg_parser.for_loop_header method_id ~entry ~exit ~ret ~body_entry ~body_exit ~exc
                loc_map f
            in
            (loc_map, all_edges, new_header, Some back_edge)
        | _ -> failwith "unrecognized statement type in Modify_header tree-diff"
      in
      let deleted_locs =
        List.fold deleted_edges ~init:Cfg.Loc.Set.empty ~f:(fun locs edge ->
            Cfg.Loc.Set.(add (add locs (Cfg.src edge)) (Cfg.dst edge)))
      in
      let loc_map = Loc_map.remove_region method_id deleted_locs loc_map in
      let cfg =
        List.fold deleted_edges ~init:cfg ~f:(flip Cfg.G.Edge.remove) |> fun cfg ->
        List.fold added_edges ~init:cfg ~f:(fun cfg e ->
            Cfg.G.Edge.(insert (uncurry3 create e) cfg))
      in
      edit_result cfg loc_map ~added_edges ~deleted_edges ~added_for_loop_backedge ()
  | Delete_statements { method_id; from_loc; to_loc } ->
      let collapse_locs = not (Cfg.Loc.equal to_loc ret) in
      let if_collapsing f = if collapse_locs then f else fun x -> x in
      let deleted_edges = Cfg.edges_btwn cfg ~src:from_loc ~dst:to_loc |> Set.to_list in
      let deleted_locs =
        List.fold deleted_edges ~init:Cfg.Loc.Set.empty ~f:(fun locs edge ->
            Cfg.Loc.Set.(add (add locs (Cfg.src edge)) (Cfg.dst edge)))
        |> flip Cfg.Loc.Set.remove from_loc
        |> if_collapsing @@ flip Cfg.Loc.Set.remove to_loc
      in
      let added_edges = if collapse_locs then [] else [ (from_loc, to_loc, Ast.Stmt.Skip) ] in
      let loc_map =
        Loc_map.remove_region method_id deleted_locs loc_map
        |> if_collapsing @@ Loc_map.rebase_edges method_id ~old_src:to_loc ~new_src:from_loc
      in
      let cfg =
        List.fold deleted_edges ~init:cfg ~f:(flip Cfg.G.Edge.remove)
        |> if_collapsing @@ fun cfg ->
           let dangling_edges = Cfg.G.Node.outputs to_loc cfg in
           Sequence.fold dangling_edges ~init:cfg ~f:(fun cfg edge ->
               Cfg.G.Edge.(insert (create from_loc (dst edge) (label edge)) (remove edge cfg)))
      in
      edit_result cfg loc_map ~deleted_edges ~added_edges ()

let apply cha diff loc_map cfgs =
  List.fold diff ~init:(loc_map, cfgs) ~f:(fun (lm, cfgs) edit ->
      match edit with
      | Add_function { method_id = { package; class_name; _ }; decl; init_info } -> (
          match decl with
          | `Meth_decl md -> (
              match Cfg_parser.of_method_decl loc_map ~package ~class_name md with
              | Some (loc_map, edges, fn) -> (loc_map, Cfg.add_fn fn ~edges cfgs)
              | None -> (loc_map, cfgs))
          | `Cons_decl (_, cd, _, body) ->
              let { instance_init; field_decls } = init_info in
              let loc_map, edges, fn =
                Cfg_parser.of_constructor_decl loc_map ~package ~class_name ~cha ~instance_init
                  ~field_decls cd body
              in
              (loc_map, Cfg.add_fn fn ~edges cfgs)
          | `Static_init (_, blk) ->
              let loc_map, edges, fn = Cfg_parser.of_static_init loc_map ~package ~class_name blk in
              (loc_map, Cfg.add_fn fn ~edges cfgs)
          | _ -> failwith "todo: Add_function with unrecognized class body decl")
      | Delete_function { method_id } ->
          let loc_map = Loc_map.remove_fn loc_map method_id in
          let cfgs = Cfg.remove_fn method_id cfgs in
          (loc_map, cfgs)
      | Modify_function { method_id; new_header } -> (
          let new_method_id, formals =
            match new_header with _ -> failwith "todo: modify function header"
          in
          match Cfg.Fn.Map.fn_by_method_id method_id cfgs with
          | Some ({ method_id = _; formals = _; locals; entry; exit; exc_exit } as old_fn) ->
              let old_fn_cfg = Cfg.Fn.Map.find_exn cfgs old_fn in
              let fn : Cfg.Fn.t =
                { method_id = new_method_id; formals; locals; entry; exit; exc_exit }
              in
              let cfgs = Cfg.remove_fn method_id cfgs in
              (loc_map, Cfg.set_fn_cfg fn ~cfg:old_fn_cfg cfgs)
          | None ->
              failwith (Format.asprintf "can't modify unknown function %a" Method_id.pp method_id))
      | Add_statements { method_id; _ }
      | Modify_statements { method_id; _ }
      | Modify_header { method_id; _ }
      | Delete_statements { method_id; _ } -> (
          match Cfg.Fn.Map.fn_by_method_id method_id cfgs with
          | None ->
              failwith (Format.asprintf "can't modify unknown function %a" Method_id.pp method_id)
          | Some fn ->
              let old_fn_cfg = Cfg.Fn.Map.find_exn cfgs fn in
              let { cfg; new_loc_map; _ } =
                apply_edit ~ret:fn.exit ~exc:fn.exc_exit edit lm old_fn_cfg
              in
              (new_loc_map, Cfg.Fn.Map.set cfgs ~key:fn ~data:cfg)))

open Result
open Option.Monad_infix

let%test "statement additions" =
  let prev_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/HelloWorld.java" in
  let next_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/HelloWorlds2.java" in
  let prev_tree = Tree.parse ~old_tree:None ~file:prev_file in
  let prev_cst =
    bind ~f:(Tree.as_java_cst prev_file) prev_tree |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let updated_prev_tree =
    prev_tree |> ok
    >>| Tree.apply
          (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
          ~offsets:(Src_file.line_offsets prev_file)
  in
  let next_tree = Tree.parse ~old_tree:updated_prev_tree ~file:next_file in
  assert (is_ok next_tree);
  let next_cst =
    bind ~f:(Tree.as_java_cst next_file) next_tree |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let ({ loc_map; _ } : Cfg_parser.prgm_parse_result) = Cfg_parser.of_java_cst prev_cst in
  let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
  match diff with
  | [ Add_statements { stmts = [ _ ]; _ }; Add_statements { stmts = [ _ ]; _ } ] -> true
  | _ -> false

let%test "statement deletions" =
  let prev_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/HelloWorlds2.java" in
  let next_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/HelloWorld.java" in
  let prev_tree = Tree.parse ~old_tree:None ~file:prev_file in
  let prev_cst =
    bind ~f:(Tree.as_java_cst prev_file) prev_tree |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let updated_prev_tree =
    prev_tree |> ok
    >>| Tree.apply
          (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
          ~offsets:(Src_file.line_offsets prev_file)
  in
  let next_cst =
    Tree.parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(Tree.as_java_cst next_file)
    |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let ({ loc_map; _ } : Cfg_parser.prgm_parse_result) = Cfg_parser.of_java_cst prev_cst in
  let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
  match diff with
  | [
   Delete_statements { from_loc = _; to_loc = _; method_id = _ };
   Delete_statements { from_loc = _; to_loc = _; method_id = _ };
  ] ->
      true
  | _ -> false

let%test "statement modification in conditional" =
  let prev_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Conditional.java" in
  let next_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Conditional2.java" in
  let prev_tree = Tree.parse ~old_tree:None ~file:prev_file in
  let prev_cst =
    bind ~f:(Tree.as_java_cst prev_file) prev_tree |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let updated_prev_tree =
    prev_tree |> ok
    >>| Tree.apply
          (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
          ~offsets:(Src_file.line_offsets prev_file)
  in
  let next_cst =
    Tree.parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(Tree.as_java_cst next_file)
    |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let ({ loc_map; _ } : Cfg_parser.prgm_parse_result) = Cfg_parser.of_java_cst prev_cst in
  let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
  match diff with
  | [ Modify_statements { method_id = _; from_loc = _; to_loc = _; new_stmts = _ } ] -> true
  | _ -> false

let%test "conditional branch deletion" =
  let prev_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Conditional.java" in
  let next_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Conditional3.java" in
  let prev_tree = Tree.parse ~old_tree:None ~file:prev_file in
  let prev_cst =
    bind ~f:(Tree.as_java_cst prev_file) prev_tree |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let updated_prev_tree =
    prev_tree |> ok
    >>| Tree.apply
          (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
          ~offsets:(Src_file.line_offsets prev_file)
  in
  let next_cst =
    Tree.parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(Tree.as_java_cst next_file)
    |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let ({ loc_map; _ } : Cfg_parser.prgm_parse_result) = Cfg_parser.of_java_cst prev_cst in
  let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
  match diff with
  | [ Delete_statements { method_id = _; from_loc = _; to_loc = _ } ] -> true
  | _ -> false

(* NOTE(benno): punting on modify-header diffs -- if a loop/conditional header is modified, just treat the entire thing as edited.
   let%test "modify condition of conditional" =
     let prev_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Conditional.java" in
     let next_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Conditional4.java" in
     let prev_tree = Tree.parse ~old_tree:None ~file:prev_file in
     let prev_cst =
       bind ~f:(Tree.as_java_cst prev_file) prev_tree |> function
       | Ok cst -> cst
       | Error _ -> failwith "parse error"
     in
     let updated_prev_tree =
       prev_tree |> ok
       >>| Tree.apply
             (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
             ~offsets:(Src_file.line_offsets prev_file)
     in
     let next_cst =
       Tree.parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(Tree.as_java_cst next_file)
       |> function
       | Ok cst -> cst
       | Error _ -> failwith "parse error"
     in
     let ({ loc_map; cfgs = prev_cfg; cha; _ } : Cfg_parser.prgm_parse_result) =
       Cfg_parser.of_java_cst prev_cst
     in
     let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
     let _next_cfg = apply cha diff loc_map prev_cfg in
     match diff with
     | [ Modify_header { method_id = _; prev_loc_ctx = _; next_stmt = _; loop_body_exit = None } ] ->
         true
     | _ -> false

   let%test "modify header of for-loop" =
     let prev_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/NestedLoops.java" in
     let next_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/NestedLoops2.java" in
     let prev_tree = Tree.parse ~old_tree:None ~file:prev_file in
     let prev_cst =
       bind ~f:(Tree.as_java_cst prev_file) prev_tree |> function
       | Ok cst -> cst
       | Error _ -> failwith "parse error"
     in
     let updated_prev_tree =
       prev_tree |> ok
       >>| Tree.apply
             (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
             ~offsets:(Src_file.line_offsets prev_file)
     in
     let next_cst =
       Tree.parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(Tree.as_java_cst next_file)
       |> function
       | Ok cst -> cst
       | Error _ -> failwith "parse error"
     in
     let ({ loc_map; cfgs = prev_cfg; cha; _ } : Cfg_parser.prgm_parse_result) =
       Cfg_parser.of_java_cst prev_cst
     in
     let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
     let _next_cfg = apply cha diff loc_map prev_cfg in
     match diff with
     | [ Modify_header { method_id = _; prev_loc_ctx = _; next_stmt = _; loop_body_exit = Some _ } ] ->
         true
     | _ -> false

   let%test "modify header of while-loop" =
     let prev_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/While.java" in
     let next_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/While3.java" in
     let prev_tree = Tree.parse ~old_tree:None ~file:prev_file in
     let prev_cst =
       bind ~f:(Tree.as_java_cst prev_file) prev_tree |> function
       | Ok cst -> cst
       | Error _ -> failwith "parse error"
     in
     let updated_prev_tree =
       prev_tree |> ok
       >>| Tree.apply
             (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
             ~offsets:(Src_file.line_offsets prev_file)
     in
     let next_cst =
       Tree.parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(Tree.as_java_cst next_file)
       |> function
       | Ok cst -> cst
       | Error _ -> failwith "parse error"
     in
     let ({ loc_map; cfgs = prev_cfg; cha; _ } : Cfg_parser.prgm_parse_result) =
       Cfg_parser.of_java_cst prev_cst
     in
     let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
     let _next_cfg = apply cha diff loc_map prev_cfg in
     match diff with
     | [ Modify_header { method_id = _; prev_loc_ctx = _; next_stmt = _; loop_body_exit = None } ] ->
         true
     | _ -> false
*)
let%test "modify body of while-loop" =
  let prev_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/While.java" in
  let next_file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/While2.java" in
  let prev_tree = Tree.parse ~old_tree:None ~file:prev_file in
  let prev_cst =
    bind ~f:(Tree.as_java_cst prev_file) prev_tree |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let updated_prev_tree =
    prev_tree |> ok
    >>| Tree.apply
          (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
          ~offsets:(Src_file.line_offsets prev_file)
  in
  let next_cst =
    Tree.parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(Tree.as_java_cst next_file)
    |> function
    | Ok cst -> cst
    | Error _ -> failwith "parse error"
  in
  let ({ loc_map; cfgs = prev_cfg; cha; _ } : Cfg_parser.prgm_parse_result) =
    Cfg_parser.of_java_cst prev_cst
  in
  let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
  let _next_cfg = apply cha diff loc_map prev_cfg in
  match diff with
  | [ Delete_statements { method_id = _; _ }; Add_statements { method_id = _; _ } ] -> true
  | _ -> false

(*
  (match Cfg.Fn.Map.data prev_cfg with
  | [cfg] ->  Dai.Cfg.G.edges cfg
              |> Sequence.to_list
              |> List.map ~f:(fun e -> Format.asprintf "%a--[%a]-->%a" Loc.pp (Dai.Cfg.src e) Dai.Ast.Stmt.pp (Dai.Cfg.G.Edge.label e) Loc.pp (Dai.Cfg.dst e))
              |> (List.pp ~pre:"\nCFG EDGES[\n\t" ~suf:"\n]" ";\n\t" String.pp) Format.std_formatter
  | _ -> failwith "should only be one function (main)");
  (match Dai.Cfg.Fn.Map.keys prev_cfg with
   | [fn] -> Format.fprintf Format.std_formatter "\nMain fn entry: %a; exit: %a\n" Loc.pp (Dai.Cfg.Fn.entry fn) Loc.pp (Dai.Cfg.Fn.exit fn)
   | _ ->  failwith "should only be one function (main)");
  *)
