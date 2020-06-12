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
  | Some "Plus" -> binop Ast.Binop.Plus json
  | Some "Minus" -> binop Ast.Binop.Minus json
  | Some "Times" -> binop Ast.Binop.Times json
  | Some "LessThan" -> binop Ast.Binop.Lt json
  | Some "Not" -> unop Ast.Unop.Not json
  | Some t ->
      failwith @@ "Unrecognized expression with term: \"" ^ t
      ^ "\" and content: " ^ show json
  | None -> failwith "Malformed expression JSON: no \"term\" element"

and binop op json =
  let l = member "lhs" json |> expr_of_json in
  let r = member "rhs" json |> expr_of_json in
  Ast.Expr.Binop { l; op; r }

and unop op json =
  let e = member "value" json |> expr_of_json in
  Ast.Expr.Unop { op; e }

let assign_of_json json =
  let lhs = member "assignmentTarget" json |> ident_of_json in
  let rhs = member "assignmentValue" json |> expr_of_json in
  Ast.Stmt.Assign { lhs; rhs }

type edge_list = (Cfg.Loc.t * Cfg.Loc.t * Ast.Stmt.t) list

let cfg_of_json json : Cfg.t =
  let rec edge_list_of_json entry exit json : edge_list =
    match member "term" json |> to_string_option with
    | Some "Statements" ->
        let rec of_json_list l curr_loc =
          match l with
          | [ last ] -> edge_list_of_json curr_loc exit last
          | curr :: rest ->
              let next_loc = Cfg.Loc.fresh () in
              List.append
                (edge_list_of_json curr_loc next_loc curr)
                (of_json_list rest next_loc)
          | [] -> [ (curr_loc, exit, Ast.Stmt.Skip) ]
          (* this case only reachable if initial list empty *)
        in
        of_json_list (member "statements" json |> to_list) entry
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
        of_assign_list
          (member "variableDeclarations" json |> convert_each assign_of_json)
          entry
    | Some "If" ->
        let if_head = Cfg.Loc.fresh () in
        let else_head = Cfg.Loc.fresh () in
        let cond = member "ifCondition" json |> expr_of_json in
        let cond_neg = Ast.Expr.Unop { op = Ast.Unop.Not; e = cond } in
        let if_branch =
          edge_list_of_json if_head exit (member "ifThenBody" json)
        in
        let else_branch =
          edge_list_of_json else_head exit (member "ifElseBody" json)
        in
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
          edge_list_of_json loop_body_entry loop_body_exit
            (member "whileBody" json)
        in
        (* edge from loop head into loop body, assuming loop condition *)
        (* edge from loop head to exit, assuming loop condition's negation *)
        (entry, loop_body_entry, Ast.Stmt.Assume cond)
        :: (entry, exit, Ast.Stmt.Assume cond_neg)
        :: (loop_body_exit, entry, Ast.Stmt.Skip)
        :: (* from end of loop body back to loop head *)
           loop_body
        (* loop body *)
    | Some t ->
        failwith @@ "Unrecognized statement with term : \"" ^ t
        ^ "\" and content: " ^ show json
    | None ->
        failwith @@ "Malformed statement JSON: no \"term\" element; content: "
        ^ show json
  in
  Cfg.Loc.reset ();
  Graph.create
    (module Cfg.G)
    ~edges:(edge_list_of_json Cfg.Loc.entry Cfg.Loc.exit json)
    ()

let%test "cfg_parse and dump dot: arith0.js" =
  let cfg =
    cfg_of_json
    @@ json_of_file "/Users/benno/Documents/CU/code/d1a/test_cases/arith0.js"
  in
  Cfg.dump_dot cfg ~filename:"/Users/benno/Documents/CU/code/d1a/arith0.dot"
    ~print:true;
  true

let%test "cfg_parse and dump dot: while.js" =
  let cfg =
    cfg_of_json
    @@ json_of_file "/Users/benno/Documents/CU/code/d1a/test_cases/while.js"
  in
  Cfg.dump_dot cfg ~filename:"/Users/benno/Documents/CU/code/d1a/while.dot"
    ~print:true;
  true

let%test "back edge classification" =
  let cfg =
    cfg_of_json
    @@ json_of_file "/Users/benno/Documents/CU/code/d1a/test_cases/while.js"
  in
  Int.equal 1
  @@ Graph.depth_first_search
       (module Cfg.G)
       ~start:Cfg.Loc.entry
       ~leave_edge:(function
         | `Back ->
             fun e acc ->
               Format.printf "Back edge: %a\n" Ast.Stmt.pp (Cfg.G.Edge.label e);
               acc + 1
         | _ ->
             fun e acc ->
               Format.printf "Forward edge: %a\n" Ast.Stmt.pp
                 (Cfg.G.Edge.label e);
               acc)
       ~init:0 cfg

module Soc_interpreter = Cfg.Interpreter (Set_of_concrete.Env)

let%test "collecting semantics: arith0.js" =
  let cfg =
    cfg_of_json
    @@ json_of_file "/Users/benno/Documents/CU/code/d1a/test_cases/arith0.js"
  in
  let collection = Soc_interpreter.collect cfg in
  Set.pp Soc_interpreter.State.pp Format.std_formatter collection;
  match Set.find collection ~f:(fst >> Cfg.Loc.equal Cfg.Loc.exit) with
  | Some (_, state) ->
      let expected =
        Sexp.(
          List
            [
              List [ Atom "b"; List [ List [ Atom "Bool"; Atom "true" ] ] ];
              List [ Atom "x"; List [ List [ Atom "Float"; Atom "1" ] ] ];
            ])
      in
      Sexp.equal expected (Set_of_concrete.Env.sexp_of_t state)
  | None -> false
