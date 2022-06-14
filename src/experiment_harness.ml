open Dai.Import
open Frontend
open Analysis
open Domain
open Syntax

let ( / ) pre post = pre ^ Stdlib.Filename.dir_sep ^ post

let base_exclusions =
  [ "test"; "package-info.java"; "module-info.java"; "annotations"; "annotation"; ".m2" ]

let experiment_exclusions = []
(* files containing "@interface" annotations that crash tree sitter *)

let exclusions = experiment_exclusions @ base_exclusions

let rec java_srcs dir =
  let open Sys in
  let open Stdlib.Filename in
  match is_directory dir with
  | `No | `Unknown ->
      failwith (Format.asprintf "can't get java sources from non-directory file %s" dir)
  | `Yes ->
      List.bind (ls_dir dir) ~f:(fun f ->
          if List.mem exclusions f ~equal:String.equal then []
          else
            let file = dir / f in
            if is_directory_exn file then java_srcs file
            else if is_file_exn file && String.equal ".java" (extension f) then [ file ]
            else [])

let relative_java_srcs dir = List.map (java_srcs dir) ~f:(String.chop_prefix_exn ~prefix:(dir / ""))

module type S = sig
  type t

  val cg : t -> Callgraph.forward_t

  val init : ?cg:string -> string -> t

  val fns : t -> Cfg.Fn.t list

  val update : ?cg:string -> next_dir:string -> t -> t

  val entrypoints : string option -> t -> Cfg.Fn.t list

  val issue_exit_queries : Cfg.Fn.t list -> t -> t

  val issue_demand_query : qry_loc:string -> Cfg.Fn.t list -> t -> t

  val dump_dot : t -> filename:string -> unit
end

module DSG_wrapper (Dom : Abstract.Dom) : S = struct
  module Dom = Abstract.DomWithDataStructures (Dom)
  module G = Dsg.Make (Dom)
  module D = G.D

  type parse_info = {
    src_dir : string;
    trees : Tree.t String.Map.t;
    loc_map : Loc_map.t;
    fields : Declared_fields.t;
    cha : Class_hierarchy.t;
  }

  type t = { dsg : G.t; cg : Callgraph.t; parse : parse_info }

  let dump_dot { dsg; _ } = G.dump_dot dsg

  let cg x = x.cg.forward

  let fns x = G.fns x.dsg

  (* Initialize a DSG over src_dir/**/*.java, with the callgraph serialized at [cg] *)
  let init ?cg src_dir =
    let cg = match cg with Some cg -> cg | None -> "/dev/null" in
    let trees =
      List.fold (java_srcs src_dir) ~init:String.Map.empty ~f:(fun trees src ->
          let file = Src_file.of_file src in
          match Tree.parse ~old_tree:None ~file with
          | Ok tree ->
              let key = String.chop_prefix_exn src ~prefix:(src_dir / "") in
              Map.set trees ~key ~data:tree
          | Error _ -> failwith ("parse error in " ^ src))
    in
    let ({ cfgs; loc_map; fields; cha } : Cfg_parser.prgm_parse_result) =
      let trees = List.map (Map.to_alist trees) ~f:(fun (file, tree) -> (src_dir / file, tree)) in
      Cfg_parser.parse_trees_exn ~trees
    in
    let dsg = G.init ~cfgs in
    let fns = G.fns dsg in
    let cg = Callgraph.deserialize ~fns (Src_file.of_file cg) in
    let () = Declared_fields.set_current_fields fields in
    let parse = { src_dir; trees; loc_map; fields; cha } in
    { dsg; cg; parse }

  (* For the bugswarm experiments, we point the analysis at two side-by-side program versions
   * rather than editing the program in place.
   * This function applies the "edit" between the two program versions to an analysis [state]
   *)
  let update ?(cg = "/dev/null") ~(next_dir : string) (g : t) : t =
    let prev_src_dir = g.parse.src_dir in
    let prev_files = String.Map.keys g.parse.trees |> String.Set.of_list in
    let next_files = relative_java_srcs next_dir |> String.Set.of_list in
    let shared_files, new_files = Set.(partition_tf next_files ~f:(mem prev_files)) in
    let changed_files =
      Set.filter shared_files ~f:(fun file ->
          let file = String.substr_replace_all ~pattern:"$" ~with_:"\\$" file in
          Sys.command
            (Format.asprintf "cmp %s %s >/dev/null" (prev_src_dir / file) (next_dir / file))
          |> (Int.equal 0 >> not))
    in
    Format.printf
      "prev_files:    %i\n\
       next_files:    %i\n\
       shared_files:  %i\n\
       new_files:     %i\n\
       changed_files: %a\n"
      (Set.length prev_files) (Set.length next_files) (Set.length shared_files)
      (Set.length new_files) (Set.pp String.pp) changed_files;
    let init = (g.parse.trees, g.dsg, g.parse.loc_map) in
    let trees, dsg, loc_map =
      Set.fold changed_files ~init ~f:(fun (trees, dsg, lm) filename ->
          let prev_tree = String.Map.find_exn g.parse.trees filename in
          let prev_file = Src_file.of_file (prev_src_dir / filename) in
          let next_file = Src_file.of_file (next_dir / filename) in
          Result.Let_syntax.(
            let%bind prev = Tree.as_java_cst prev_file prev_tree in
            let%bind tree = Tree.parse ~old_tree:(Some prev_tree) ~file:next_file in
            let%map next = Tree.as_java_cst next_file tree in
            let diff = Tree_diff.btwn lm ~prev ~next in
            Format.printf "tree diff for %s:\n%a\n" filename Tree_diff.pp diff;
            let lm, dsg = G.apply_edit ~cha:g.parse.cha ~diff lm dsg in
            (Map.set trees ~key:filename ~data:tree, dsg, lm))
          |> function
          | Ok res -> res
          | _ -> failwith ("failed to update file: " ^ filename))
    in
    let open Cfg_parser in
    let open Result.Monad_infix in
    let ({ cfgs; loc_map; cha; fields } : prgm_parse_result) =
      let init =
        set_parse_result ~loc_map ~cha:g.parse.cha ~fields:g.parse.fields empty_parse_result
      in
      Set.fold new_files ~init ~f:(fun acc filename ->
          let file = Src_file.of_file (next_dir / filename) in
          Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| Cfg_parser.of_java_cst ~acc
          |> function
          | Ok res -> res
          | _ -> failwith ("error parsing file: " ^ filename))
    in
    let dsg = G.add_exn ~cfgs dsg in
    let fns = G.fns dsg in
    let cg = Callgraph.deserialize ~fns (Src_file.of_file cg) in
    (* TODO: handle added fields and CHA edges in edited files; add corresponding Tree_diff.edit's
       and expose functions there to use here to apply diffs to our fields/cha structures *)
    let () = Declared_fields.set_current_fields fields in
    let parse = { src_dir = next_dir; trees; loc_map; fields; cha } in
    Format.printf "\n[EXPERIMENT][INFO] dirtying\n";
    G.print_stats Format.std_formatter dsg;
    { dsg; cg; parse }

  let entrypoints entry_class g =
    let f =
      match entry_class with
      | Some cls ->
          let package = deserialize_package cls in
          let class_name = deserialize_class cls in
          fun (f : Cfg.Fn.t) ->
            String.equal class_name f.method_id.class_name
            && (List.equal String.equal) package f.method_id.package
            && String.equal "main" f.method_id.method_name
      | None -> fun (_f : Cfg.Fn.t) -> true
      (*String.equal "main" f.method_id.method_name*)
    in
    List.filter (G.fns g.dsg) ~f

  let issue_exit_queries entrypoints (g : t) =
    let st = systime () in
    let dsg =
      List.fold entrypoints ~init:g.dsg ~f:(fun dsg (fn : Cfg.Fn.t) ->
          G.query dsg ~fn ~entry_state:(Dom.init ()) ~loc:fn.exit ~cg:g.cg ~fields:g.parse.fields
          |> snd)
    in
    Format.(fprintf std_formatter)
      "\n[EXPERIMENT] exhaustive analysis took: %.3f\n"
      (1000. *. (systime () -. st));
    G.print_stats Format.std_formatter dsg;
    if Option.is_some g.cg.scc then Callgraph.print_scc_stats (Option.value_exn g.cg.scc)
    else Format.print_string "[EXPERIMENT] no scc\n";
    (* G.print_summaries Format.std_formatter dsg ~num_summaries:100; *)
    (* G.dump_dot dsg ~filename:(abs_of_rel_path "solved_experiement.dsg.dot") ~num_daigs:100; *)
    { dsg; cg = g.cg; parse = g.parse }

  let issue_demand_query ~qry_loc entrypoints (g : t) : t =
    let method_id = Method_id.deserialize qry_loc in
    match List.find (G.fns g.dsg) ~f:(fun fn -> Method_id.equal method_id fn.method_id) with
    | None -> failwith ("no procedure found matching demand query " ^ qry_loc)
    | Some fn ->
        let st = systime () in
        let _res, dsg =
          G.loc_only_query g.dsg ~fn ~loc:fn.exit ~cg:g.cg ~fields:g.parse.fields ~entrypoints
        in
        Format.(fprintf std_formatter)
          "\n[EXPERIMENT] demand query took: %.3f\n"
          (1000. *. (systime () -. st));
        G.print_stats Format.std_formatter dsg;
        { dsg; cg = g.cg; parse = g.parse }
end
