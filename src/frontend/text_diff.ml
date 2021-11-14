open Dai.Import

type t = {
  prev_start_line : int;
  prev_length : int;
  next_start_line : int;
  new_lines : string array;
}

let pp fs { prev_start_line; prev_length; next_start_line; new_lines } =
  Format.fprintf fs
    "text diff: {prev_start_line:%i; prev_length:%i; next_start_line:%i; length(new_lines):%i"
    prev_start_line prev_length next_start_line (Array.length new_lines)

module Patdiff = Patience_diff_lib.Patience_diff.Make (Core.String)
module Hunk = Patience_diff_lib.Patience_diff.Hunk

(* In patience-diff jargon, a "diff" is a collection of "hunks" and a "hunk" is a collection of "ranges", which are each either a "same" for unchanged regions, a "next" for additions, a "prev" for deletions, or a "replace" for modifications *)

(* The [context] named argument to [get_hunks] controls the maximum size of "same" regions -- e.g. to control the amount of unchanged-code context you want to see around changes in a git diff -- so I set it to 0 because we only care about the changes themselves. *)

(* The [big_enough] named arg "governs how aggressively we try to clean up spurious matches, by restricting our attention to only matches of length less than [big_enough]". I set it to 3, which is the patdiff default for line diffs *)

let btwn ~prev ~next =
  Patdiff.get_hunks ~transform:(fun x -> x) ~context:0 ~big_enough:3 ~prev ~next
  |> List.map ~f:(fun hunk ->
         match Hunk.ranges hunk with
         | [ Same _ ] ->
             {
               prev_start_line = Hunk.prev_start hunk;
               prev_length = Hunk.prev_size hunk;
               next_start_line = Hunk.next_start hunk;
               new_lines = [||];
             }
         | [ Same [||]; r; Same [||] ] ->
             let new_lines =
               match r with
               | Prev _ -> [||]
               | Next new_lines | Replace (_, new_lines) -> new_lines
               | Same _ -> failwith "Malformed hunk -- adjacent \"Same\" ranges"
               | Unified _ -> failwith "Unrecognized diff type: \"Unified\""
             in
             {
               prev_start_line = Hunk.prev_start hunk;
               prev_length = Hunk.prev_size hunk;
               next_start_line = Hunk.next_start hunk;
               new_lines;
             }
         | ranges ->
             let string_of_range : 'a Patience_diff_lib.Patience_diff.Range.t -> string = function
               | Prev _ -> "Prev"
               | Next _ -> "Next"
               | Replace _ -> "Replace"
               | Same _ -> "Same"
               | Unified _ -> "Unified"
             in
             failwith
               (Format.asprintf
                  "With ~context:0, patdiff should always yield 3-range hunks of form (Same [||]) \
                   :: r :: (Same [||]) :: []; got %a instead"
                  (List.pp "/" String.pp)
                  (List.map ranges ~f:string_of_range)))

let%test "adding adjacent lines" =
  let prev =
    [|
      "class Foo {";
      "  //My first program!";
      "  public static void main(String[] args){";
      "    System.out.println(\"Hello, world!\");";
      "  }";
      "}";
    |]
  in
  let next =
    [|
      "class Foo {";
      "  //My first program!";
      "  public static void main(String[] args){";
      "    System.out.println(\"Hello, world!\");";
      "    System.out.println(\"Hello, world!1\");";
      "    System.out.println(\"Hello, world!2\");";
      "    System.out.println(\"Hello, world!3\");";
      "    System.out.println(\"Hello, world!4\");";
      "  }";
      "}";
    |]
  in
  match btwn ~prev ~next with
  | [ { prev_start_line = 5; prev_length = 0; next_start_line = 5; new_lines = [| _; _; _; _ |] } ]
    ->
      true
  | _ -> false

let%test "adding non-adjacent lines" =
  let prev =
    [|
      "class Foo {";
      "  //My first program!";
      "  public static void main(String[] args){";
      "    System.out.println(\"Hello, world!\");";
      "  }";
      "}";
    |]
  in
  let next =
    [|
      "class Foo {";
      "  //My first program!";
      "  public static void main(String[] args){";
      "    System.out.println(\"Hello, world!-1\");";
      "    System.out.println(\"Hello, world!\");";
      "    System.out.println(\"Hello, world!+1\");";
      "  }";
      "}";
    |]
  in
  match btwn ~prev ~next with
  | [
   { prev_start_line = 4; prev_length = 0; next_start_line = 4; new_lines = [| _ |] };
   { prev_start_line = 5; prev_length = 0; next_start_line = 6; new_lines = [| _ |] };
  ] ->
      true
  | _ -> false

let%test "removing a line" =
  let prev =
    [|
      "class Foo {";
      "  //My first program!";
      "  public static void main(String[] args){";
      "    System.out.println(\"Hello, world!\");";
      "  }";
      "}";
    |]
  in
  let next =
    [|
      "class Foo {";
      "  public static void main(String[] args){";
      "    System.out.println(\"Hello, world!\");";
      "  }";
      "}";
    |]
  in
  match btwn ~prev ~next with
  | [ { prev_start_line = 2; prev_length = 1; next_start_line = 2; new_lines = [||] } ] -> true
  | _ -> false

let%test "replacing a line" =
  let prev =
    [|
      "class Foo {";
      "  //My first program!";
      "  public static void main(String[] args){";
      "    System.out.println(\"Hello, world!\");";
      "  }";
      "}";
    |]
  in
  let next =
    [|
      "class Foo {";
      "  /** now it's documented */";
      "  public static void main(String[] args){";
      "    System.out.println(\"Hello, world!\");";
      "  }";
      "}";
    |]
  in
  match btwn ~prev ~next with
  | [ { prev_start_line = 2; prev_length = 1; next_start_line = 2; new_lines = [| _ |] } ] -> true
  | _ -> false

let%test "1 replacement, 1 addition, 1 deletion, all non-adjacent" =
  let prev =
    [|
      "class Foo {";
      "  //My first program!";
      "  public static void main(String[] args){";
      "    System.out.println(\"Hello, world!\");";
      "  }";
      "}";
    |]
  in
  let next =
    [|
      "class Foo {";
      "  /** now it's documented */";
      "  public static void main(String[] args){";
      "  }";
      "//added line";
      "}";
    |]
  in
  match btwn ~prev ~next with
  | [
   { prev_start_line = 2; prev_length = 1; next_start_line = 2; new_lines = [| _ |] };
   { prev_start_line = 4; prev_length = 1; next_start_line = 4; new_lines = [||] };
   { prev_start_line = 6; prev_length = 0; next_start_line = 5; new_lines = [| _ |] };
  ] ->
      true
  | _ -> false
