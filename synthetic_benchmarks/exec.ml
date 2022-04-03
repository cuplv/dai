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
end

let run =
  basic ~summary:"Run scalability experiments with synthetic benchmarks"
    [%map_open
      let seed = anon ("seed" %: int)
      and incr =
        flag "incremental" no_arg
          ~doc:"incremental analysis: reuse results across the program edit where possible"
      and dd =
        flag "demand" no_arg
          ~doc:"demand-driven analysis: analyze only as needed to respond to queries"
      in
      fun () ->
        let mode =
          match (incr, dd) with
          | true, true -> Mode.Incr_and_dd
          | true, false -> Mode.Incr_only
          | false, true -> Mode.Demand_only
          | false, false -> Mode.Batch
        in
        Format.printf "[INFO] initializing experiment in %a mode with seed %i\n" Mode.pp mode seed]
