open Dai
open Import
open Command
open Command.Let_syntax

let analyze =
  basic ~summary:"Analyze a program edit using the DAI framework"
    [%map_open
      let src_dir = anon ("src_dir" %: string)
      and incr =
        flag "incremental" no_arg
          ~doc:"incremental analysis: reuse results across the program edit where possible"
      and dd =
        flag "demand" (optional string)
          ~doc:
            "<com.example.Baz#foo(argtypes)> demand-driven analysis: analyze only as needed to \
             analyze the specified method (in the same format as output by WALA-callgraph)"
      and diagnostic =
        flag "diagnostic" no_arg
          ~doc:
            "Run in diagnostic mode, just printing information about frontend compatibility with \
             the given sources"
      and next_dir =
        flag "edit-dir" (optional string)
          ~doc:
            "<dir> run on two versions of a program for experiments, considering the source in \
             [src_dir] as the pre-edit and [edit-dir] as the post-edit version"
      and prev_cg =
        flag "prev-callgraph" (optional string)
          ~doc:"<cg> path toserialized callgraph for src_dir program version"
      and next_cg =
        flag "next-callgraph" (optional string)
          ~doc:"<cg> path to serialized callgraph for next_dir program version"
      and entry_class =
        flag "entry-class" (optional string)
          ~doc:
            "<com.example.Baz> fully-qualified Java class name to treat as entrypoint. Defaults to \
             all \"main\" methods if not provided"
      and cg_to_dot =
        flag "cg-to-dot" (optional string)
          ~doc:"<cg> deserialize callgraph for <src_dir>, dump to ./cg.dot, and exit"
      and unit_dom =
        flag "unit-dom" no_arg
          ~doc:"use the unit domain (defaults to array-bounds-checking interval domain otherwise)"
      and null_dom =
        flag "null-dom" no_arg
          ~doc:
            "use the nullable domain (defaults to array-bounds-checking interval domain otherwise)"
      and filter_cg =
        flag "filter-cg" (optional string)
          ~doc:
            "<serialized-callgraph> filter the serialized callgraph to include only edges for \
             which source is available in SRC_DIR, writing to stdout"
      in
      fun () ->
        Format.(fprintf err_formatter) "[EXEC] Initializing DAI; java src_dir: %s\n" src_dir;
        let open Frontend in
        let (module Harness : Experiment_harness.S) =
          if unit_dom then
            (module Experiment_harness.DSG_wrapper (Domain.Unit_dom) : Experiment_harness.S)
          else if null_dom then
            (module Experiment_harness.DSG_wrapper (Domain.Incr.Make_env_with_heap (Domain.Null_val))
            : Experiment_harness.S)
          else (module Experiment_harness.DSG_wrapper (Domain.Array_bounds) : Experiment_harness.S)
        in
        if Option.is_some filter_cg then
          let fns = Harness.(init src_dir |> fns) in
          Callgraph.filter ~fns (Option.value_exn filter_cg |> Src_file.of_file)
        else if Option.is_some cg_to_dot then
          let cg = Option.value_exn cg_to_dot in
          let _ =
            Format.(fprintf err_formatter)
              "[EXEC] Dumping DOT representation of %s at ./cg.dot\n" cg
          in
          let state = Harness.init ~cg src_dir in
          Callgraph.dump_dot ~filename:(abs_of_rel_path "cg.dot") (Harness.cg state)
        else if diagnostic then (
          Format.(fprintf err_formatter) "[EXEC] RUNNING DIAGNOSTIC MODE: PARSING\n";
          Cfg_parser.set_diagnostic true;
          let state =
            match prev_cg with Some cg -> Harness.init ~cg src_dir | _ -> Harness.init src_dir
          in
          (match next_dir with
          | Some next_dir -> (
              Format.(fprintf err_formatter) "[EXEC] RUNNING DIAGNOSTIC MODE: EDITING\n";
              ignore
              @@
              match next_cg with
              | Some cg -> Harness.update ~cg ~next_dir state
              | _ -> Harness.update ~next_dir state)
          | None -> ());
          Cfg_parser.print_diagnostic_results ())
        else
          match (next_dir, prev_cg, next_cg) with
          | Some next_dir, Some prev_cg, Some next_cg -> (
              let open Harness in
              let mode =
                match (incr, dd) with
                | true, Some qry_loc -> `Demand_and_incr qry_loc
                | false, Some qry_loc -> `Demand_only qry_loc
                | true, None -> `Incr_only
                | false, None -> `Batch
              in
              match mode with
              | `Batch ->
                  Format.(fprintf err_formatter) "[EXEC] Running analysis in BATCH mode\n";
                  Format.(fprintf err_formatter) "[EXEC] Constructing initial state...\n";
                  let initial_state = init ~cg:prev_cg src_dir in
                  let init_entrypoints = entrypoints entry_class initial_state in
                  Format.(fprintf err_formatter) "[EXEC] Querying at program exit locations...\n";
                  let _queried_state = issue_exit_queries init_entrypoints initial_state in
                  Format.(fprintf err_formatter) "[EXEC] Constructing edited state...\n";
                  let edited_state = init ~cg:next_cg next_dir in
                  let edit_entrypoints = entrypoints entry_class edited_state in
                  Format.(fprintf err_formatter) "[EXEC] Querying at program exit locations...\n";
                  let _queried_edited_state = issue_exit_queries edit_entrypoints edited_state in
                  ()
              | `Demand_only qry_loc ->
                  Format.(fprintf err_formatter) "[EXEC] Running analysis in DEMAND-DRIVEN mode\n";
                  Format.(fprintf err_formatter) "[EXEC] Constructing initial state...\n";
                  let initial_state = init ~cg:prev_cg src_dir in
                  let init_entrypoints = entrypoints entry_class initial_state in
                  Format.(fprintf err_formatter) "[EXEC] Querying at demand query location...\n";
                  let _queried_state = issue_demand_query ~qry_loc init_entrypoints initial_state in
                  Format.(fprintf err_formatter) "[EXEC] Constructing edited state...\n";
                  let edited_state = init ~cg:next_cg next_dir in
                  let edit_entrypoints = entrypoints entry_class edited_state in
                  Format.(fprintf err_formatter) "[EXEC] Querying at demand query locations...\n";
                  let _queried_edited_state =
                    issue_demand_query ~qry_loc edit_entrypoints edited_state
                  in
                  ()
              | `Incr_only ->
                  Format.(fprintf err_formatter) "[EXEC] Running analysis in INCREMENTAL mode\n";
                  Format.(fprintf err_formatter) "[EXEC] Constructing initial state...\n";
                  let initial_state = init ~cg:prev_cg src_dir in
                  let init_entrypoints = entrypoints entry_class initial_state in
                  Format.(fprintf err_formatter) "[EXEC] Querying at program exit locations...\n";
                  let queried_state = issue_exit_queries init_entrypoints initial_state in
                  Format.(fprintf err_formatter) "[EXEC] Applying incremental edit...\n";
                  let edited_state = update ~next_dir ~cg:next_cg queried_state in
                  let edit_entrypoints = entrypoints entry_class edited_state in
                  Format.(fprintf err_formatter) "[EXEC] Querying at program exit locations...\n";
                  let _queried_edited_state = issue_exit_queries edit_entrypoints edited_state in
                  ()
              | `Demand_and_incr qry_loc ->
                  Format.(fprintf err_formatter) "[EXEC] Running analysis in DEMANDED mode\n";
                  Format.(fprintf err_formatter) "[EXEC] Constructing initial state...\n";
                  let initial_state = init ~cg:prev_cg src_dir in
                  let init_entrypoints = entrypoints entry_class initial_state in
                  Format.(fprintf err_formatter) "[EXEC] Querying at demand query location...\n";
                  let queried_state = issue_demand_query ~qry_loc init_entrypoints initial_state in
                  Format.(fprintf err_formatter) "[EXEC] Applying incremental edit...\n";
                  let edited_state = update ~next_dir ~cg:next_cg queried_state in
                  let edit_entrypoints = entrypoints entry_class edited_state in
                  Format.(fprintf err_formatter) "[EXEC] Querying at demand query locations...\n";
                  let _queried_edited_state =
                    issue_demand_query ~qry_loc edit_entrypoints edited_state
                  in
                  ())
          | _ ->
              Format.printf
                "Please either specify diagnostic-mode or supply two program versions and callgraphs\n"]

let () = Command.run ~version:"0.1" analyze
