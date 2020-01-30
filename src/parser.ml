open Import
open Yojson.Basic
open Yojson.Basic.Util
open Ast

let json_of_file file =
  from_string Shell.(run_full "semantic" [ "parse"; "--json"; file ])

let test_json =
  json_of_file "/Users/benno/Documents/CU/code/d1a/test_cases/foo.js"

let ident_of_json = member "name" >> to_string

let rec expr_of_json json =
  match member "term" json |> to_string_option with
  | Some "Float" ->
      let f = member "floatContent" json |> to_string |> Float.of_string in
      Expr.Lit (Lit.Float f)
  | Some "Int" ->
      let i = member "intContent" json |> to_string |> Int.of_string in
      Expr.Lit (Lit.Int i)
  | Some "TextElement" ->
      let s = member "textElementContent" json |> to_string in
      let s_quotes_stripped = String.slice s 1 (String.length s - 1) in
      Expr.Lit (Lit.String s_quotes_stripped)
  | Some "Boolean" ->
      let b = member "booleanContent" json |> to_bool in
      Expr.Lit (Lit.Bool b)
  | Some "Null" -> Expr.Lit Lit.Null
  | Some "Identifier" ->
      let id = member "name" json |> to_string in
      Expr.Var id
  | Some "Plus" -> binop Binop.Plus json
  | Some "Minus" -> binop Binop.Minus json
  | Some "Times" -> binop Binop.Times json
  | Some t ->
      failwith @@ "fell through expr_of_json cases with term: \"" ^ t
      ^ "\" and content: " ^ show json
  | None -> failwith "fell through expr_of_json cases with no \"term\" element"

and binop op json =
  let l = member "lhs" json |> expr_of_json in
  let r = member "rhs" json |> expr_of_json in
  Expr.Binop { l; op; r }

let rec assign_of_json json =
  let lhs = member "assignmentTarget" json |> ident_of_json in
  let rhs = member "assignmentValue" json |> expr_of_json in
  Stmt.Assign { lhs; rhs }

and if_of_json json =
  let cond = member "ifCondition" json |> expr_of_json in
  let then_body = member "ifThenBody" json |> stmt_of_json in
  let else_body = member "ifElseBody" json |> stmt_of_json in
  Stmt.If { cond; then_body; else_body }

and stmt_of_json json =
  match member "term" json |> to_string_option with
  | Some "Statements" ->
      member "statements" json |> convert_each stmt_of_json |> seqify_list
  | Some "Assignment" -> assign_of_json json
  | Some "VariableDeclaration" ->
      member "variableDeclarations" json
      |> convert_each assign_of_json
      |> seqify_list
  | Some "If" -> if_of_json json
  | Some x -> failwith @@ "got something else: " ^ x
  | None -> failwith @@ "got none :("

(** NB: assumes exactly one source file for now (by calling hd_exn) *)
let stmt_list =
  member "trees" >> to_list >> List.hd_exn >> member "tree"
  >> member "statements" >> convert_each stmt_of_json >> Ast.seqify_list

let%test "dummy" = String.equal ("Hello, " ^ "world!") "Hello, world!"

let _ = stmt_list test_json |> Stmt.pp Format.std_formatter
