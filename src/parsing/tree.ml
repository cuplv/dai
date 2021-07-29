open Dai.Import
open Tree_sitter_java
module TSB = Tree_sitter_bindings

type t = TSB.Tree_sitter_API.ts_tree

type java_cst = CST.program

let parse ~old_tree ~file =
  try
    Result.return
    @@ TSB.Tree_sitter_API.Parser.parse Parse.ts_parser old_tree (Src_file.read_fn file)
  with e ->
    Printexc.print_backtrace Stdlib.stdout;
    raise e

(* todo: fail gracefully *)

let as_java_cst src ts_tree =
  let root = TSB.Tree_sitter_output.of_ts_tree ts_tree in
  match Parse.parse_input_tree { src; root } with
  | { program = Some p; errors = []; stat = _ } -> Ok p
  | { errors; _ } -> Error errors

let rec update_offsets (old_offsets : int list) (text_diff : Text_diff.t) =
  match (old_offsets, text_diff) with
  | offsets_hd :: offsets_tl, { prev_start_line; prev_length; next_start_line; new_lines }
    when prev_start_line > 1 ->
      let prev_start_line = prev_start_line - 1 in
      offsets_hd
      :: update_offsets offsets_tl { prev_start_line; prev_length; next_start_line; new_lines }
  | _, { prev_start_line = 1; prev_length; next_start_line = _; new_lines } ->
      let new_offsets = Array.to_list new_lines |> List.map ~f:(String.length >> Int.succ) in
      List.append new_offsets (List.drop old_offsets prev_length)
  | _ -> failwith "malformed text diff"

let cumulative_offset offsets ~(lines : int) = List.take offsets lines |> List.fold ~init:0 ~f:( + )

let apply (diff : Text_diff.t list) ~(offsets : int list) (tree : t) =
  let line_offset = ref 0 in
  List.fold diff ~init:(tree, offsets) ~f:(fun (tree, offsets) d ->
      let new_tree =
        match d with
        | { prev_start_line; prev_length; next_start_line; new_lines } ->
            let prev_start_line = !line_offset + prev_start_line in
            let prev_end_line = prev_start_line + prev_length in
            let next_end_line = next_start_line + Array.length new_lines in
            line_offset := !line_offset - prev_length + Array.length new_lines;
            let start_byte = cumulative_offset offsets ~lines:(prev_start_line - 1) in
            let prev_end_byte =
              cumulative_offset offsets ~lines:(prev_start_line + prev_length - 1)
            in
            let next_end_byte =
              Array.fold new_lines ~init:start_byte ~f:(fun bytes new_line ->
                  bytes + 1 + String.length new_line)
            in
            TSB.Tree_sitter_API.Tree.edit tree start_byte prev_end_byte next_end_byte
              next_start_line prev_end_line next_end_line
      in
      (new_tree, update_offsets offsets d))
  |> fst

open Result
open Option.Monad_infix

let%test "initial parse, valid syntax" =
  let file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorld.java" in
  parse ~old_tree:None ~file |> bind ~f:(as_java_cst file) |> is_ok

let%test "initial parse, invalid syntax" =
  let file = Src_file.of_file ~abspath:false "test_cases/java/SyntaxError.java" in
  parse ~old_tree:None ~file |> bind ~f:(as_java_cst file) |> is_error

let%test "incremental parse, valid syntax, one-hunk edit" =
  let prev_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorld.java" in
  let next_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorlds.java" in
  let prev_tree = parse ~old_tree:None ~file:prev_file in
  assert (bind ~f:(as_java_cst prev_file) prev_tree |> is_ok);
  let updated_prev_tree =
    prev_tree |> ok
    >>| apply
          (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
          ~offsets:(Src_file.line_offsets prev_file)
  in
  parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(as_java_cst next_file) |> is_ok

let%test "incremental parse, valid syntax, two additions" =
  let prev_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorld.java" in
  let next_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorlds2.java" in
  let prev_tree = parse ~old_tree:None ~file:prev_file in
  assert (bind ~f:(as_java_cst prev_file) prev_tree |> is_ok);
  let updated_prev_tree =
    prev_tree |> ok
    >>| apply
          (Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file))
          ~offsets:(Src_file.line_offsets prev_file)
  in
  parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(as_java_cst next_file) |> is_ok

let%test "incremental parse, valid syntax, two deletions" =
  let prev_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorlds2.java" in
  let next_file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorld.java" in
  let prev_tree = parse ~old_tree:None ~file:prev_file in
  assert (bind ~f:(as_java_cst prev_file) prev_tree |> is_ok);
  let diff = Text_diff.btwn ~prev:(Src_file.lines prev_file) ~next:(Src_file.lines next_file) in
  let updated_prev_tree =
    prev_tree |> ok >>| apply diff ~offsets:(Src_file.line_offsets prev_file)
  in
  parse ~old_tree:updated_prev_tree ~file:next_file |> bind ~f:(as_java_cst next_file) |> is_ok
