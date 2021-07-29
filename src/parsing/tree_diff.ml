open Dai.Import
module Loc = Dai.Cfg.Loc
module CST = Tree_sitter_java.CST

type edit =
  | Add_function of { name : string; method_decl : CST.method_declaration }
  | Delete_function of { name : string }
  | Modify_function of { name : string; new_header : CST.method_header }
  | Add_statements of { at_loc : Loc.t; stmts : CST.statement list }
  | Modify_statements of { from_loc : Loc.t; to_loc : Loc.t; new_stmts : CST.statement list }
  | Modify_header of { at_loc : Loc.t; stmt : CST.statement }
  | Delete_statements of { from_loc : Loc.t; to_loc : Loc.t }

type t = edit list

let pp_edit fs = function
  | Add_function { name; method_decl = _ } -> Format.fprintf fs "(Add function %s)" name
  | Delete_function { name } -> Format.fprintf fs "(Delete function %s)" name
  | Modify_function { name; new_header = _ } ->
      Format.fprintf fs "(Modify function %s's header)" name
  | Add_statements { at_loc; stmts } ->
      Format.fprintf fs "(Add %i statements at %a)" (List.length stmts) Loc.pp at_loc
  | Modify_statements { from_loc; to_loc; new_stmts } ->
      Format.fprintf fs "(Overwrite range %a->%a with %i new stmts)" Loc.pp from_loc Loc.pp to_loc
        (List.length new_stmts)
  | Modify_header { at_loc; stmt } ->
      let control_flow_type =
        match stmt with
        | `If_stmt _ -> "conditional"
        | `While_stmt _ -> "while-loop"
        | `For_stmt _ -> "for-loop"
        | _ -> failwith "unrecognized control-flow construct"
      in
      Format.fprintf fs "(Modify %s header at %a)" control_flow_type Loc.pp at_loc
  | Delete_statements { from_loc; to_loc } ->
      Format.fprintf fs "(Delete statements in range %a->%a)" Loc.pp from_loc Loc.pp to_loc

let pp = List.pp ~pre:"DIFF:[@[<hv 2>" ~suf:"@]" ";@," pp_edit

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
      let ({ entry = curr_entry; exit = curr_exit; ret = _ } : Loc_map.loc_ctx) =
        Loc_map.get method_id curr_stmt loc_map
      in
      match removed_range with
      | None -> Some (curr_entry, curr_exit)
      | Some (acc_range_start, acc_range_end) ->
          assert (Loc.equal curr_entry acc_range_end);
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
    | Some pred -> Loc_map.get method_id pred loc_map |> fun { entry = _; exit; ret = _ } -> exit
    | None ->
        let succ =
          match List.nth ranges (idx + 1) with
          | Some (Same succs) -> get succs 0 |> fst
          | Some (Replace (succs, _)) | Some (Prev succs) -> get succs 0
          | _ -> failwith "unable to find CFG location for \"Next\" range from patdiff"
        in
        Loc_map.get method_id succ loc_map |> fun { entry; exit = _; ret = _ } -> entry
  in
  List.foldi ranges ~init:[] ~f:(fun idx acc_edits -> function
    | Prev stmts ->
        let from_loc, to_loc = loc_range method_id loc_map stmts in
        Delete_statements { from_loc; to_loc } :: acc_edits
    | Next stmts -> Add_statements { at_loc = loc_by_idx idx; stmts = to_list stmts } :: acc_edits
    | Replace (prev_stmts, next_stmts) ->
        flip ( @ ) acc_edits
          ( match (prev_stmts, next_stmts) with
          | [||], [||] -> failwith "malformed diff: empty replacement"
          | ( [| `If_stmt (_, (_, prev_cond, _), prev_t_branch, prev_f_branch) as prev_stmt |],
              [| `If_stmt (_, (_, next_cond, _), next_t_branch, next_f_branch) as next_stmt |] ) ->
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
              if Sexp.equal (CST.sexp_of_expression prev_cond) (CST.sexp_of_expression next_cond)
              then t_branch_diff @ f_branch_diff
              else
                let at_loc = Loc_map.get method_id prev_stmt loc_map |> fun { entry; _ } -> entry in
                Modify_header { at_loc; stmt = next_stmt } :: (t_branch_diff @ f_branch_diff)
          | ( [| `While_stmt (_, (_, prev_cond, _), prev_body) as prev_stmt |],
              [| `While_stmt (_, (_, next_cond, _), next_body) as next_stmt |] ) ->
              let prev = match prev_body with `Blk (_, b, _) -> b | stmt -> [ stmt ] in
              let next = match next_body with `Blk (_, b, _) -> b | stmt -> [ stmt ] in
              let body_diff = diff_of_stmt_list method_id loc_map ~prev ~next in
              if Sexp.equal (CST.sexp_of_expression prev_cond) (CST.sexp_of_expression next_cond)
              then body_diff
              else
                let at_loc = Loc_map.get method_id prev_stmt loc_map |> fun { entry; _ } -> entry in
                Modify_header { at_loc; stmt = next_stmt } :: body_diff
          | ( [| `For_stmt (_, _, prev_init, prev_cond, _, prev_iter, _, prev_body) as prev_stmt |],
              [| `For_stmt (_, _, next_init, next_cond, _, next_iter, _, next_body) as next_stmt |]
            ) ->
              let is_header_unchanged =
                ( match (prev_init, next_init) with
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
                | _ -> false )
                && ( match (prev_cond, next_cond) with
                   | None, None -> true
                   | Some p, Some n ->
                       Sexp.equal (CST.sexp_of_expression p) (CST.sexp_of_expression n)
                   | _ -> false )
                &&
                match (prev_iter, next_iter) with
                | None, None -> true
                | Some p, Some n ->
                    Sexp.equal
                      (CST.sexp_of_anon_exp_rep_COMMA_exp_0bb260c p)
                      (CST.sexp_of_anon_exp_rep_COMMA_exp_0bb260c n)
                | _ -> false
              in
              let prev = match prev_body with `Blk (_, b, _) -> b | stmt -> [ stmt ] in
              let next = match next_body with `Blk (_, b, _) -> b | stmt -> [ stmt ] in
              let body_diff = diff_of_stmt_list method_id loc_map ~prev ~next in
              if is_header_unchanged then body_diff
              else
                let at_loc = Loc_map.get method_id prev_stmt loc_map |> fun { entry; _ } -> entry in
                Modify_header { at_loc; stmt = next_stmt } :: body_diff
          | prevs, nexts ->
              let from_loc, to_loc = loc_range method_id loc_map prevs in
              [ Modify_statements { from_loc; to_loc; new_stmts = to_list nexts } ] )
    | Same _ -> acc_edits
    | Unified _ -> failwith "Unrecognized diff type: \"Unified\"")

let btwn loc_map ~(prev : Tree.java_cst) ~(next : Tree.java_cst) =
  let open List.Monad_infix in
  let method_decls_by_name : CST.statement -> (string * CST.method_declaration) list = function
    | `Decl (`Class_decl (_, _, (_, class_name), _, _, _, (_, body_decls, _))) ->
        List.fold body_decls ~init:[] ~f:(fun acc -> function
          | `Meth_decl ((_, (_, _, (`Id (_, method_name), _, _), _), _) as md) ->
              (class_name ^ "#" ^ method_name, md) :: acc
          | _ -> failwith "unrecognized class body declaration")
    | `Decl (`Import_decl _) -> []
    | _ -> failwith "unrecognized top-level definition"
  in
  let prev_decls_by_name = prev >>= method_decls_by_name |> String.Map.of_alist_exn in
  let next_decls_by_name = next >>= method_decls_by_name |> String.Map.of_alist_exn in
  let prev_names = String.Map.key_set prev_decls_by_name in
  let next_names = String.Map.key_set next_decls_by_name in
  let shared_names = String.Set.inter prev_names next_names in
  let deleted_names, added_names =
    String.Set.symmetric_diff prev_names next_names
    |> Sequence.fold ~init:([], []) ~f:(fun (deleted, added) -> function
         | First nm -> (nm :: deleted, added) | Second nm -> (deleted, nm :: added))
  in
  let function_additions =
    List.map added_names ~f:(fun name ->
        Add_function { name; method_decl = String.Map.find_exn next_decls_by_name name })
  in
  let function_deletions = List.map deleted_names ~f:(fun name -> Delete_function { name }) in
  let function_header_modifications =
    Set.fold shared_names ~init:[] ~f:(fun acc name ->
        let prev_header = String.Map.find_exn prev_decls_by_name name |> snd3 in
        let next_header = String.Map.find_exn next_decls_by_name name |> snd3 in
        if
          Sexp.equal (CST.sexp_of_method_header prev_header) (CST.sexp_of_method_header next_header)
        then acc
        else Modify_function { name; new_header = next_header } :: acc)
  in
  let stmt_edits =
    Set.fold shared_names ~init:[] ~f:(fun acc name ->
        let prev =
          String.Map.find_exn prev_decls_by_name name |> trd3 |> function
          | `Blk (_, b, _) -> b
          | `SEMI _ -> []
        in
        let next =
          String.Map.find_exn next_decls_by_name name |> trd3 |> function
          | `Blk (_, b, _) -> b
          | `SEMI _ -> []
        in
        diff_of_stmt_list name loc_map ~prev ~next @ acc)
  in
  stmt_edits @ function_additions @ function_deletions @ function_header_modifications

open Result
open Option.Monad_infix

let%test "statement additions" =
  let prev_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorld.java" in
  let next_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorlds2.java" in
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
  let loc_map, _prev_cfg = Cfg.of_java_cst prev_cst in
  let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
  match diff with
  | [ Add_statements { at_loc = _; stmts = [ _ ] }; Add_statements { at_loc = _; stmts = [ _ ] } ]
    ->
      true
  | _ -> false

let%test "statement deletions" =
  let prev_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorlds2.java" in
  let next_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorld.java" in
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
  let loc_map, _prev_cfg = Cfg.of_java_cst prev_cst in
  let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
  match diff with
  | [
   Delete_statements { from_loc = _; to_loc = _ }; Delete_statements { from_loc = _; to_loc = _ };
  ] ->
      true
  | _ -> false

let%test "statement modification in conditional" =
  let prev_file = Src_file.of_file ~abspath:false "test_cases/java/Conditional.java" in
  let next_file = Src_file.of_file ~abspath:false "test_cases/java/Conditional2.java" in
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
  let loc_map, _prev_cfg = Cfg.of_java_cst prev_cst in
  let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
  match diff with
  | [ Modify_statements { from_loc = _; to_loc = _; new_stmts = _ } ] -> true
  | _ -> false

let%test "conditional branch deletion" =
  let prev_file = Src_file.of_file ~abspath:false "test_cases/java/Conditional.java" in
  let next_file = Src_file.of_file ~abspath:false "test_cases/java/Conditional3.java" in
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
  let loc_map, _prev_cfg = Cfg.of_java_cst prev_cst in
  let diff = btwn loc_map ~prev:prev_cst ~next:next_cst in
  match diff with [ Delete_statements { from_loc = _; to_loc = _ } ] -> true | _ -> false

(*
  (match Dai.Cfg.Fn.Map.data prev_cfg with
  | [cfg] ->  Dai.Cfg.G.edges cfg
              |> Sequence.to_list
              |> List.map ~f:(fun e -> Format.asprintf "%a--[%a]-->%a" Loc.pp (Dai.Cfg.src e) Dai.Ast.Stmt.pp (Dai.Cfg.G.Edge.label e) Loc.pp (Dai.Cfg.dst e))
              |> (List.pp ~pre:"\nCFG EDGES[\n\t" ~suf:"\n]" ";\n\t" String.pp) Format.std_formatter
  | _ -> failwith "should only be one function (main)");
  (match Dai.Cfg.Fn.Map.keys prev_cfg with
   | [fn] -> Format.fprintf Format.std_formatter "\nMain fn entry: %a; exit: %a\n" Loc.pp (Dai.Cfg.Fn.entry fn) Loc.pp (Dai.Cfg.Fn.exit fn)
   | _ ->  failwith "should only be one function (main)");
  
*)
