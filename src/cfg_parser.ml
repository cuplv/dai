open Import
open Yojson.Basic
open Yojson.Basic.Util

(** NB: assumes exactly one source file for now (by calling hd_exn) *)
let json_of_file file =
  from_string Shell.(run_full "semantic" [ "parse"; "--json"; file ])
  |> (member "trees" >> to_list >> List.hd_exn >> member "tree")

let ident_of_json = member "name" >> to_string

let rec expr_of_json json =
  match member "term" json |> to_string_option with
  | Some "Float" ->
      let f = member "floatContent" json |> to_string |> Float.of_string in
      Ast.Expr.Lit (Ast.Lit.Float f)
  | Some "Int" ->
      let i = member "intContent" json |> to_string |> Int.of_string in
      Ast.Expr.Lit (Ast.Lit.Int i)
  | Some "TextElement" ->
      let s = member "textElementContent" json |> to_string in
      let s_quotes_stripped = String.slice s 1 (String.length s - 1) in
      Ast.Expr.Lit (Ast.Lit.String s_quotes_stripped)
  | Some "Boolean" ->
      let b = member "booleanContent" json |> to_bool in
      Ast.Expr.Lit (Ast.Lit.Bool b)
  | Some "Null" -> Ast.Expr.Lit Ast.Lit.Null
  | Some "Identifier" ->
      let id = member "name" json |> to_string in
      Ast.Expr.Var id
  | Some "MemberAccess" ->
      let rcvr = member "lhs" json |> expr_of_json in
      let field = member "rhs" json |> expr_of_json in
      Ast.Expr.Deref { rcvr; field }
  | Some "Subscript" ->
      let rcvr = member "lhs" json |> expr_of_json in
      let field = member "rhs" json |> index 0 |> expr_of_json in
      Ast.Expr.Deref { rcvr; field }
  | Some "Array" -> (
      match convert_each to_int (member "sourceSpan" json |> member "start") with
      | [ line; col ] ->
          Ast.Expr.Array
            {
              elts = convert_each expr_of_json (member "arrayElements" json);
              alloc_site = (line, col);
            }
      | _ -> failwith "malformed expression json: no source location info" )
  | Some "Plus" -> binop Ast.Binop.Plus json
  | Some "Minus" -> binop Ast.Binop.Minus json
  | Some "Times" -> binop Ast.Binop.Times json
  | Some "Equal" -> binop Ast.Binop.Eq json
  | Some "LessThan" -> binop Ast.Binop.Lt json
  | Some "GreaterThan" -> binop Ast.Binop.Gt json
  | Some "LessThanEqual" -> binop Ast.Binop.Le json
  | Some "GreaterThanEqual" -> binop Ast.Binop.Ge json
  | Some "And" -> binop Ast.Binop.And json
  | Some "Or" -> binop Ast.Binop.Or json
  | Some "Not" -> unop Ast.Unop.Not json
  | Some "Negate" -> unop Ast.Unop.Neg json
  | Some t ->
      failwith @@ "Unrecognized expression with term: \"" ^ t ^ "\" and content: " ^ show json
  | None -> failwith "Malformed expression JSON: no \"term\" element"

and binop op json =
  let l = member "lhs" json |> expr_of_json in
  let r = member "rhs" json |> expr_of_json in
  Ast.Expr.Binop { l; op; r }

and unop op json =
  let e = member "value" json |> expr_of_json in
  Ast.Expr.Unop { op; e }

let assign_of_json json =
  let lhs_json = member "assignmentTarget" json in
  let rhs_json = member "assignmentValue" json in
  match member "term" rhs_json |> to_string_option with
  | Some "Call" ->
      let lhs = ident_of_json lhs_json in
      let fn = member "callFunction" rhs_json |> member "name" |> to_string in
      let actuals = member "callParams" rhs_json |> convert_each expr_of_json in
      Ast.Stmt.Call { lhs; fn; actuals }
  | _ -> (
      let rhs = expr_of_json rhs_json in
      match member "term" lhs_json |> to_string_option with
      | Some "Identifier" ->
          let lhs = ident_of_json lhs_json in
          Ast.Stmt.Assign { lhs; rhs }
      | Some "Subscript" ->
          let rcvr = member "lhs" lhs_json |> member "name" |> to_string in
          let field = member "rhs" lhs_json |> index 0 |> expr_of_json in
          Ast.Stmt.Write { rcvr; field; rhs }
      | Some "MemberAccess" ->
          let rcvr = member "lhs" lhs_json |> member "name" |> to_string in
          let field = member "rhs" lhs_json |> expr_of_json in
          Ast.Stmt.Write { rcvr; field; rhs }
      | _ -> failwith "malformed assignment JSON" )

type edge = Cfg.Loc.t * Cfg.Loc.t * Ast.Stmt.t

let cfg_of_json json : Cfg.t =
  let is_fn_decl stmt_json =
    match member "term" stmt_json |> to_string with "Function" -> true | _ -> false
  in
  let rec edge_list_of_json entry exit ret json : edge list =
    match member "term" json |> to_string_option with
    | Some "Statements" | Some "StatementBlock" ->
        let rec of_json_list l curr_loc =
          match l with
          | [ last ] -> edge_list_of_json curr_loc exit ret last
          | curr :: rest ->
              let next_loc = Cfg.Loc.fresh () in
              List.append
                (edge_list_of_json curr_loc next_loc ret curr)
                (of_json_list rest next_loc)
          | [] -> [ (curr_loc, exit, Ast.Stmt.Skip) ]
          (* this case only reachable if initial list empty *)
        in
        of_json_list (member "statements" json |> to_list |> List.drop_while ~f:is_fn_decl) entry
    | Some "Assignment" -> [ (entry, exit, assign_of_json json) ]
    | Some "VariableDeclaration" ->
        let rec of_assign_list l curr_loc =
          match l with
          | [ last ] -> [ (curr_loc, exit, last) ]
          | curr :: rest ->
              let next_loc = Cfg.Loc.fresh () in
              (curr_loc, next_loc, curr) :: of_assign_list rest next_loc
          | [] -> [ (curr_loc, exit, Ast.Stmt.Skip) ]
          (* this case only reachable if initial list empty *)
        in
        of_assign_list (member "variableDeclarations" json |> convert_each assign_of_json) entry
    | Some "If" ->
        let if_head = Cfg.Loc.fresh () in
        let else_head = Cfg.Loc.fresh () in
        let cond = member "ifCondition" json |> expr_of_json in
        let cond_neg = Ast.Expr.Unop { op = Ast.Unop.Not; e = cond } in
        let if_branch = edge_list_of_json if_head exit ret (member "ifThenBody" json) in
        let else_branch = edge_list_of_json else_head exit ret (member "ifElseBody" json) in
        (* edge into if-branch, assuming condition *)
        (entry, if_head, Ast.Stmt.Assume cond)
        :: (entry, else_head, Ast.Stmt.Assume cond_neg)
        :: (* edge into else-branch, assuming condition's negation *)
           List.append if_branch else_branch
        (* if-branch and else-branch *)
    | Some "While" ->
        let loop_body_entry = Cfg.Loc.fresh () in
        let loop_body_exit = Cfg.Loc.fresh () in
        let cond = member "whileCondition" json |> expr_of_json in
        let cond_neg = Ast.Expr.Unop { op = Ast.Unop.Not; e = cond } in
        let loop_body =
          edge_list_of_json loop_body_entry loop_body_exit ret (member "whileBody" json)
        in
        (* edge from loop head into loop body, assuming loop condition *)
        (* edge from loop head to exit, assuming loop condition's negation *)
        (* edge from end of loop body back to loop head *)
        (* loop body *)
        (entry, loop_body_entry, Ast.Stmt.Assume cond)
        :: (entry, exit, Ast.Stmt.Assume cond_neg)
        :: (loop_body_exit, entry, Ast.Stmt.Skip)
        :: loop_body
    | Some "Return" ->
        (* "return e" statement is interpreted as an edge to program exit with "RETVAL := e"*)
        let rval = member "value" json |> expr_of_json in
        [ (entry, ret, Ast.Stmt.Assign { lhs = "RETVAL"; rhs = rval }) ]
    | Some "Context" ->
        (* "Context" packages a comment and a statement; ignore the comment and just parse the statement *)
        edge_list_of_json entry exit ret (member "contextSubject" json)
    | Some "Empty" ->
        (* "Empty" statements are else-branches of else-branch-less conditionals *)
        [ (entry, exit, Ast.Stmt.Skip) ]
    | Some "Function" -> []
    | Some t ->
        failwith @@ "Unrecognized statement with term : \"" ^ t ^ "\" and content: " ^ show json
    | None -> failwith @@ "Malformed statement JSON: no \"term\" element; content: " ^ show json
  in
  Cfg.Loc.reset ();
  (* We assume all function definitions are top level declarations preceding any other statements *)
  let fn_decls_of_json json : (Cfg.Fn.t * edge list) list =
    match member "term" json |> to_string_option with
    | Some "Statements" ->
        let stmts = member "statements" json |> to_list in
        let fn_decls = List.take_while stmts ~f:is_fn_decl in
        List.map fn_decls ~f:(fun json ->
            let entry = Cfg.Loc.fresh () in
            let exit = Cfg.Loc.fresh () in
            let name : string = member "functionName" json |> member "name" |> to_string in
            let formals : string list =
              member "functionParameters" json
              |> convert_each (member "requiredParameterSubject" >> member "name" >> to_string)
            in
            let body : edge list =
              member "functionBody" json |> edge_list_of_json entry exit exit
            in
            (Cfg.Fn.make ~name ~formals ~entry ~exit ~body, body))
    | _ -> failwith "Malformed JSON parse tree"
  in
  let fns = fn_decls_of_json json in
  let edges =
    List.fold fns ~init:(edge_list_of_json Cfg.Loc.entry Cfg.Loc.exit Cfg.Loc.exit json)
      ~f:(fun es (_, fn_body) -> List.append fn_body es)
  in
  let cfg = Graph.create (module Cfg.G) ~edges () in
  (cfg, List.map ~f:fst fns |> Cfg.Fn.Set.of_list)

let%test "cfg_parse and dump dot: arith_syntax.js" =
  let cfg = cfg_of_json @@ json_of_file (Unix.getcwd () ^ "/arith_syntax.js") in
  Cfg.dump_dot cfg ~filename:"arith.dot";
  true

let%test "cfg_parse and dump dot: while_syntax.js" =
  let cfg = cfg_of_json @@ json_of_file (Unix.getcwd () ^ "/while_syntax.js") in
  Cfg.dump_dot cfg ~filename:"while.dot";
  true

let%test "cfg_parse and dump dot: array_syntax.js" =
  let cfg = cfg_of_json @@ json_of_file (Unix.getcwd () ^ "/array_syntax.js") in
  Cfg.dump_dot cfg ~filename:"array.dot";
  true

let%test "cfg_parse and dump dot: list_append.js" =
  let cfg = cfg_of_json @@ json_of_file (Unix.getcwd () ^ "/list_append.js") in
  Cfg.dump_dot cfg ~filename:"list.dot";
  true

let%test "cfg_parse and dump dot: functions.js" =
  let cfg = cfg_of_json @@ json_of_file (Unix.getcwd () ^ "/functions.js") in
  Cfg.dump_dot cfg ~filename:"functions.dot";
  true

let%test "back edge classification: while_syntax.js" =
  let cfg, _ = cfg_of_json @@ json_of_file (Unix.getcwd () ^ "/while_syntax.js") in
  Int.equal 1
  @@ Graph.depth_first_search
       (module Cfg.G)
       ~start:Cfg.Loc.entry
       ~leave_edge:(function `Back -> fun _e acc -> acc + 1 | _ -> fun _e acc -> acc)
       ~init:0 cfg
