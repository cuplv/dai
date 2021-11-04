open Dai
open Import
open Command
open Command.Let_syntax

let exclusions = [ "test"; "package-info.java"; "module-info.java" ]

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
            let file = dir ^ dir_sep ^ f in
            if is_directory_exn file then java_srcs file
            else if is_file_exn file && String.equal ".java" (extension f) then [ file ]
            else [])

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
      in
      fun () ->
        let srcs = java_srcs src_dir in
        Format.printf "Initializing DAI; java src_dir: %s\n" src_dir;
        (* Format.printf "Initializing DAI; java sources found:\n\t%a"
           (List.pp ~pre:"" ~suf:"\n\n" "\n\t" String.pp)
           srcs;*)
        let open Frontend in
        let open Result.Monad_infix in
        List.iter srcs ~f:(fun src ->
            let file = Src_file.of_file src in
            Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file
            >>| Cfg_parser.of_java_cst ~diagnostic
            |> ignore);
        if diagnostic then Cfg_parser.print_diagnostic_results () else ()]

let () = Command.run ~version:"0.1" analyze
