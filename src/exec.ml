open Dai
open Import
open Command
open Command.Let_syntax

let rec java_srcs dir =
  let open Sys in
  let open Stdlib.Filename in
  match is_directory dir with
  | `No | `Unknown -> failwith (Format.asprintf "can't get java sources from non-directory file %s" dir)
  | `Yes -> List.bind (ls_dir dir) ~f:(fun file ->
      let file = (dir ^ dir_sep ^ file) in
      if is_directory_exn file
      then java_srcs file
      else if is_file_exn file && String.equal ".java" (extension file)
      then [file]
      else []
    )

let analyze =
  basic
    ~summary: "Interactively analyze a Java program using the DAI framework."
    [%map_open
      let src_dir = anon("src_dir" %: string)
      and _batch = flag "batch" no_arg ~doc:"Run in batch mode, disabling interactivity"
      in
      fun () ->
        let srcs = java_srcs src_dir in
        Format.printf "Initializing DAI; java sources found:\n\t%a" (List.pp ~pre:"" ~suf:"\n\n" "\n\t" String.pp) srcs;
        List.iter srcs ~f:(fun src ->
            Format.printf "Parsing `%s`..." src;
            let open Frontend in
            let open Result.Monad_infix in
            let file = Src_file.of_file src in
            Tree.parse ~old_tree:None ~file
            >>= Tree.as_java_cst file
            >>| Cfg_parser.of_java_cst
            |> function
            | Ok _ -> Format.print_string "success\n"
            | Error _ -> Format.print_string "failure\n"
          )
    ]

let () = Command.run ~version:"0.1" analyze
