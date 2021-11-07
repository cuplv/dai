open Dai
open Import
open Command
open Command.Let_syntax
module Analysis = Experiment_harness.DSG_wrapper (Domain.Array_bounds)

let analyze =
  basic ~summary:"Interactively analyze a Java program using the DAI framework."
    [%map_open
      let src_dir = anon ("src_dir" %: string)
      and _batch = flag "batch" no_arg ~doc:"Run in batch mode, disabling interactivity"
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
          | Some next_dir, Some prev_cg, Some next_cg ->
              Format.printf
                "Running basic debugging/testing setup: \n\
                 \t(1) parse [src_dir] (initial_dsg.dot)\n\
                 \t(2) issue query at program exit (queried_initial_dsg.dot)\n\
                 \t(3) apply \"edit\" btwn src_dir/next_dir (edited_dsg.dot)\n\
                 \t(4) issue query at program exit (queried_edited_dsg.dot)\n";
              let open Analysis in
              let initial_state = init src_dir prev_cg in
              dump_dot initial_state.dsg ~filename:(abs_of_rel_path "initial_dsg.dot");
              let queried_state = issue_exit_queries initial_state in
              dump_dot queried_state.dsg ~filename:(abs_of_rel_path "queried_initial_dsg.dot");
              let edited_state = update next_dir next_cg queried_state in
              dump_dot edited_state.dsg ~filename:(abs_of_rel_path "edited_dsg.dot");
              let queried_edited_state = issue_exit_queries edited_state in
              dump_dot queried_edited_state.dsg ~filename:(abs_of_rel_path "queried_edited_dsg.dot")
          | _ ->
              Format.printf
                "Please either specify diagnostic-mode or supply two program versions and callgraphs\n"]

let () = Command.run ~version:"0.1" analyze
