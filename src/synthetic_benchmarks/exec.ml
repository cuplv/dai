open Dai
open Import
open Command
open Command.Let_syntax

module Mode = struct
  type t = Incr_and_dd | Incr_only | Demand_only | Batch

  let pp fs a =
    Format.fprintf fs
      (match a with
      | Incr_and_dd -> "INCREMENTAL & DEMAND-DRIVEN"
      | Incr_only -> "INCREMENTAL"
      | Demand_only -> "DEMAND-DRIVEN"
      | Batch -> "BATCH")

  let pp_short fs a =
    Format.fprintf fs
      (match a with
      | Incr_and_dd -> "incr_dd"
      | Incr_only -> "incr"
      | Demand_only -> "dd"
      | Batch -> "batch")
end

module RE = Random_edits

let rec apply_n_times ~n ~init ~f =
  if n <= 0 then init else (apply_n_times [@tailcall]) ~n:(pred n) ~init:(f init n) ~f

let time ~f ~x fs =
  let st = systime () in
  let res = f x in
  let st' = systime () in
  Format.fprintf fs "%.3f\n" (1000. *. (st' -. st));
  res

let run =
  basic ~summary:"Run scalability experiments with synthetic benchmarks"
    [%map_open
      let seed = flag "seed" (required int) ~doc:"<int> random number generator seed"
      and n = anon ("n" %: int)
      and incr =
        flag "incremental" no_arg
          ~doc:"incremental analysis: reuse results across the program edit where possible"
      and dd =
        flag "demand" no_arg
          ~doc:"demand-driven analysis: analyze only as needed to respond to queries"
      and qpe =
        flag "qpe" (optional_with_default 3 int)
          ~doc:"<X> If \"-demand\" flag is enabled, issue X queries between each edit.  Default 3."
      in
      fun () ->
        let mode =
          match (incr, dd) with
          | true, true -> Mode.Incr_and_dd
          | true, false -> Mode.Incr_only
          | false, true -> Mode.Demand_only
          | false, false -> Mode.Batch
        in
        Format.printf "[INFO] initializing experiment in %a mode with seed %i\n" Mode.pp mode seed;
        Format.print_flush ();
        let fs_log =
          let logfile =
            Format.asprintf "out/log/%a_seed%i_n%i%s.log" Mode.pp_short mode seed n
              (if dd then "_qpe" ^ Int.to_string qpe else "")
          in
          Unix.openfile ~mode:[ Unix.O_WRONLY; Unix.O_CREAT ] (abs_of_rel_path logfile)
          |> Unix.out_channel_of_descr |> Format.formatter_of_out_channel
        in

        let gc_settings = Gc.get () in
        (* disable heap compaction *)
        gc_settings.max_overhead <- 1000000;
        (* per docs suggestion, switch to "best-fit" allocation when heap compaction is off *)
        gc_settings.allocation_policy <- 2;
        (* massively increase interval between GC cycles *)
        gc_settings.space_overhead <- 8000;
        Gc.set gc_settings;

        Random.init seed;
        let g = RE.init () in

        let issue_demand_queries init =
          apply_n_times ~n:qpe ~init ~f:(fun x _ -> time fs_log ~f:RE.random_query ~x)
        in
        let flush_mod k ~n = if Int.(equal 0 (n % k)) then Format.pp_print_flush fs_log () in
        let f =
          match mode with
          | Mode.Incr_and_dd ->
              fun x n ->
                let x = RE.random_edit x in
                flush_mod 100 ~n;
                issue_demand_queries x
          | Mode.Incr_only ->
              fun x n ->
                let x = RE.random_edit x in
                flush_mod 100 ~n;
                time fs_log ~f:RE.exit_query ~x
          | Mode.Demand_only ->
              fun x n ->
                let x = RE.random_edit (RE.G.drop_daigs x) in
                flush_mod 100 ~n;
                issue_demand_queries x
          | Mode.Batch ->
              fun x n ->
                let x = RE.random_edit (RE.G.drop_daigs x) in
                flush_mod 100 ~n;
                time fs_log ~f:RE.exit_query ~x
        in
        ignore @@ apply_n_times ~n ~init:g ~f]

let () = Command.run ~version:"0.1" run
