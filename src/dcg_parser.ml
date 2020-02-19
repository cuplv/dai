open Import
open Yojson.Basic
open Yojson.Basic.Util
module Dcg = Dcg.With_default_articulation

let json_of_file file =
  from_string Shell.(run_full "semantic" [ "parse"; "--json"; file ])

let ident_of_json = member "name" >> to_string

let rec expr_of_json json =
  match member "term" json |> to_string_option with
  | Some "Float" ->
      let f = member "floatContent" json |> to_string |> Float.of_string in
      Dcg.Expr.Lit (Dcg.Lit.Float f)
  | Some "Int" ->
      let i = member "intContent" json |> to_string |> Int.of_string in
      Dcg.Expr.Lit (Dcg.Lit.Int i)
  | Some "TextElement" ->
      let s = member "textElementContent" json |> to_string in
      let s_quotes_stripped = String.slice s 1 (String.length s - 1) in
      Dcg.Expr.Lit (Dcg.Lit.String s_quotes_stripped)
  | Some "Boolean" ->
      let b = member "booleanContent" json |> to_bool in
      Dcg.Expr.Lit (Dcg.Lit.Bool b)
  | Some "Null" -> Dcg.Expr.Lit Dcg.Lit.Null
  | Some "Identifier" ->
      let id = member "name" json |> to_string in
      Dcg.Expr.Var id
  | Some "Plus" -> binop Dcg.Binop.Plus json
  | Some "Minus" -> binop Dcg.Binop.Minus json
  | Some "Times" -> binop Dcg.Binop.Times json
  | Some t ->
      failwith @@ "Unrecognized expression with term: \"" ^ t
      ^ "\" and content: " ^ show json
  | None -> failwith "Malformed expression JSON: no \"term\" element"

and binop op json =
  let l = member "lhs" json |> expr_of_json in
  let r = member "rhs" json |> expr_of_json in
  Dcg.Expr.Binop { l; op; r }

let rec assign_of_json name json =
  let lhs = member "assignmentTarget" json |> ident_of_json in
  let rhs = member "assignmentValue" json |> expr_of_json in
  Dcg.AStmt.cell name (Dcg.Stmt.Assign { lhs; rhs })

and if_of_json name json =
  let cond = member "ifCondition" json |> expr_of_json in
  let then_body =
    member "ifThenBody" json |> stmt_of_json (Name.extend name "ifThenBody")
  in
  let else_body =
    member "ifElseBody" json |> stmt_of_json (Name.extend name "ifElseBody")
  in
  Dcg.AStmt.cell name (If { cond; then_body; else_body })

and stmt_of_json name json =
  match member "term" json |> to_string_option with
  | Some "Statements" ->
      member "statements" json |> seqify_list name stmt_of_json
  | Some "Assignment" -> assign_of_json name json
  | Some "VariableDeclaration" ->
      member "variableDeclarations" json |> seqify_list name assign_of_json
  | Some "If" -> if_of_json (Name.extend name "If") json
  | Some x -> failwith @@ "Unrecognized statement type: " ^ x
  | None -> failwith "Malformed statement JSON: no \"term\" element"

and seqify_list nm elt_parser = function
  | `List _ as json ->
      let idx = ref 0 in
      let name_with_index stmt =
        let name = Name.extend nm (Int.to_string !idx) in
        Ref.replace idx Int.succ;
        elt_parser name stmt
      in
      convert_each name_with_index json
      |> List.fold ~init:Dcg.Stmt.Skip ~f:Dcg.AStmt.seq
  | _ -> failwith "can't seqify non-list json object"

(** NB: assumes exactly one source file for now (by calling hd_exn) *)
let stmt_list =
  member "trees" >> to_list >> List.hd_exn >> member "tree"
  >> member "statements"
  >> seqify_list (Name.gensym ()) stmt_of_json

let arith0 =
  stmt_list
  @@ json_of_file "/Users/benno/Documents/CU/code/d1a/test_cases/arith0.js"

let arith1 =
  stmt_list
  @@ json_of_file "/Users/benno/Documents/CU/code/d1a/test_cases/arith1.js"

let arith2 : Dcg.AStmt.t =
  stmt_list
  @@ json_of_file "/Users/benno/Documents/CU/code/d1a/test_cases/arith2.js"
