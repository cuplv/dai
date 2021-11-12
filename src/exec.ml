open Dai
open Import
open Command
open Command.Let_syntax
module Analysis = Experiment_harness.DSG_wrapper (Domain.Array_bounds)

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
      in
      fun () ->
        let srcs = Experiment_harness.java_srcs src_dir in
        Format.printf "Initializing DAI; java src_dir: %s\n" src_dir;
        let open Frontend in
        let open Result.Monad_infix in
        if diagnostic && Option.is_some next_dir then
          failwith "diagnostic mode only applies to a single program version"
        else if diagnostic then (
          List.iter srcs ~f:(fun src ->
              let file = Src_file.of_file src in
              Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file
              >>| Cfg_parser.of_java_cst ~diagnostic
              |> ignore);
          Cfg_parser.print_diagnostic_results ())
        else
          match (next_dir, prev_cg, next_cg) with
          | Some next_dir, Some prev_cg, Some next_cg -> (
              let open Analysis in
              let mode =
                match (incr, dd) with
                | true, Some qry_loc -> `Demand_and_incr qry_loc
                | false, Some qry_loc -> `Demand_only qry_loc
                | true, None -> `Incr_only
                | false, None -> `Batch
              in
              match mode with
              | `Batch ->
                  Format.printf "Running analysis in BATCH mode\n";
                  Format.printf "Constructing initial state...\n";
                  let initial_state = init src_dir prev_cg in
                  let entrypoints = entrypoints entry_class initial_state in
                  Format.printf "Querying at program exit locations...\n";
                  let _queried_state = issue_exit_queries entrypoints initial_state in
                  Format.printf "Constructing edited state...\n";
                  let edited_state = init next_dir next_cg in
                  Format.printf "Querying at program exit locations...\n";
                  let _queried_edited_state = issue_exit_queries entrypoints edited_state in
                  ()
              | `Demand_only qry_loc ->
                  Format.printf "Running analysis in DEMAND-DRIVEN mode\n";
                  Format.printf "Constructing initial state...\n";
                  let initial_state = init src_dir prev_cg in
                  let entrypoints = entrypoints entry_class initial_state in
                  Format.printf "Querying at demand query location...\n";
                  let _queried_state = issue_demand_query qry_loc entrypoints initial_state in
                  Format.printf "Constructing edited state...\n";
                  let edited_state = init next_dir next_cg in
                  Format.printf "Querying at demand query locations...\n";
                  let _queried_edited_state = issue_demand_query qry_loc entrypoints edited_state in
                  ()
              | `Incr_only ->
                  Format.printf "Running analysis in INCREMENTAL mode\n";
                  Format.printf "Constructing initial state...\n";
                  let initial_state = init src_dir prev_cg in
                  let entrypoints = entrypoints entry_class initial_state in
                  Format.printf "Querying at program exit locations...\n";
                  let queried_state = issue_exit_queries entrypoints initial_state in
                  Format.printf "Applying incremental edit...\n";
                  let edited_state = update next_dir next_cg queried_state in
                  Format.printf "Querying at program exit locations...\n";
                  let _queried_edited_state = issue_exit_queries entrypoints edited_state in
                  ()
              | `Demand_and_incr qry_loc ->
                  Format.printf "Running analysis in DEMANDED mode\n";
                  Format.printf "Constructing initial state...\n";
                  let initial_state = init src_dir prev_cg in
                  let entrypoints = entrypoints entry_class initial_state in
                  Format.printf "Querying at demand query location...\n";
                  let queried_state = issue_demand_query qry_loc entrypoints initial_state in
                  Format.printf "Applying incremental edit...\n";
                  let edited_state = update next_dir next_cg queried_state in
                  Format.printf "Querying at demand query locations...\n";
                  let _queried_edited_state = issue_demand_query qry_loc entrypoints edited_state in
                  ())
          | _ ->
              Format.printf
                "Please either specify diagnostic-mode or supply two program versions and callgraphs\n"]

let () = Command.run ~version:"0.1" analyze
