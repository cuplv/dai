open D1a
open Import
open Experiments
open Command
open Command.Let_syntax

let do_n_edits_and_queries =
  basic
    ~summary:
      "Starting from an empty program, issue a random edit followed by a random query (or queries) \
       N times"
    [%map_open
      let n = anon ("n" %: int)
      and dd = flag "dd" no_arg ~doc:"Enable demand-driven analysis"
      and incr = flag "incr" no_arg ~doc:"Enable incremental analysis"
      and qpe =
        flag "qpe" (optional_with_default 1 int)
          ~doc:"<X> If \"-dd\" flag is enabled, issue X queries between each edit.  Default 1."
      and seed =
        flag "seed" (optional_with_default 0 int) ~doc:"<int> Random number generator seed"
      in
      fun () ->
        (* unique output stream for this run, named by parameters*)
        let filename =
          (if dd then "dd_" ^ Int.to_string qpe ^ "qpe_" else "")
          ^ (if incr then "incr_" else "")
          ^ ("n" ^ Int.to_string n ^ "_")
          ^ "seed" ^ Int.to_string seed
        in
        let fs_out =
          Unix.openfile ~mode:[ Unix.O_WRONLY; Unix.O_CREAT ] (Util.exp_output filename)
          |> Unix.out_channel_of_descr |> Format.formatter_of_out_channel
        in

        let gc_settings = Gc.get () in
        (* disable heap compaction entirely *)
        gc_settings.max_overhead <- 1000000;
        (* per docs suggestion, switch to "best-fit" allocation when heap compaction is off *)
        gc_settings.allocation_policy <- 2;
        (* massively increase interval between GC cycles *)
        gc_settings.space_overhead <- 8000;
        Gc.set gc_settings;

        Cfg.Loc.reset ();
        if dd && incr then (
          let module RE = Random_edits.Make (Incr.Make (Itv)) in
          Random.init seed;
          let init = RE.D.of_cfg @@ Cfg.empty () in
          let issue_queries init =
            apply_n_times ~n:qpe ~init ~f:(fun x -> time fs_out "" ~f:RE.issue_random_query ~x)
          in
          let f = RE.random_edit >> issue_queries in
          let daig = apply_n_times ~n ~init ~f in
          RE.D.dump_dot ~filename:(Util.daig_output filename) daig )
        else if dd then (
          let module RE = Random_edits.Make (Itv) in
          Random.init seed;
          let init = RE.D.of_cfg @@ Cfg.empty () in
          let issue_queries init =
            apply_n_times ~n:qpe ~init
              ~f:(RE.D.drop_cache >> fun x -> time fs_out "" ~f:RE.issue_random_query ~x)
          in
          let f = RE.random_edit >> issue_queries in
          let daig = apply_n_times ~n ~init ~f in
          RE.D.dump_dot ~filename:(Util.daig_output filename) daig )
        else if incr then (
          let module RE = Random_edits.Make (Incr.Make (Itv)) in
          Random.init seed;
          let init = RE.D.of_cfg @@ Cfg.empty () in
          let f = RE.random_edit >> fun x -> time fs_out "" ~f:RE.issue_exit_query ~x in
          let daig = apply_n_times ~n ~init ~f in
          RE.D.dump_dot ~filename:(Util.daig_output filename) daig )
        else
          let module RE = Random_edits.Make (Itv) in
          Random.init seed;
          let init = RE.D.of_cfg @@ Cfg.empty () in
          let f =
            RE.random_edit >> RE.D.drop_cache >> fun x -> time fs_out "" ~f:RE.issue_exit_query ~x
          in
          let daig = apply_n_times ~n ~init ~f in
          RE.D.dump_dot ~filename:(Util.daig_output filename) daig]

let () = Command.run ~version:"0.1" do_n_edits_and_queries
