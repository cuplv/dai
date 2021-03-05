open D1a
open Import
open Ast

module Make (Dom : D1a.Abstract.DomNoCtx) = struct
  module Dom = Context.MakeInsensitive (Dom)
  module D = Daig.Make (Dom)
  module E = D.G.Edge
  module N = D.G.Node
  module Nm = D.Name

  (** For each [gen_*_ident], generate a new identifier 20% of time, sample a previously-generated one 80% of time *)
  let arith_vars = ref 1

  let gen_arith_ident () =
    if phys_equal (Random.int 5) 1 then arith_vars := !arith_vars + 1;
    "a" ^ Int.to_string @@ Random.int !arith_vars

  let bool_vars = ref 1

  let gen_bool_ident () =
    if phys_equal (Random.int 5) 1 then bool_vars := !bool_vars + 1;
    "b" ^ Int.to_string @@ Random.int !bool_vars

  let array_vars = ref 1

  let gen_array_ident () =
    if phys_equal (Random.int 5) 1 then array_vars := !array_vars + 1;
    "arr" ^ Int.to_string @@ Random.int !array_vars

  (** add an if-then-else construct to [daig] at location [l] *)
  let add_ite_at l ~cond ~if_stmt ~else_stmt daig =
    let g, (cfg, fns) = daig in
    (* need three fresh locations: one for each branch, and one for the join point *)
    let post_loc = Cfg.Loc.fresh () in
    let if_loc = Cfg.Loc.fresh () in
    let else_loc = Cfg.Loc.fresh () in
    let assume = Stmt.Assume cond in
    let assume_neg = Stmt.Assume (Expr.Unop { op = Unop.Not; e = cond }) in

    (* First, add the ITE to the cfg representation *)
    let new_cfg =
      let open Cfg.G.Edge in
      Seq.fold (Cfg.G.Node.outputs l cfg) ~init:cfg ~f:(fun cfg edge ->
          remove edge cfg |> insert (create post_loc (dst edge) (label edge)))
      |> insert (create l if_loc assume)
      |> insert (create l else_loc assume_neg)
      |> insert (create if_loc post_loc if_stmt)
      |> insert (create else_loc post_loc else_stmt)
    in
    (* add any new defs to the metadata struct for the containing function*)
    let new_fns =
      match Cfg.containing_fn l (cfg, fns) with
      | Some ({ name; formals; locals; entry; exit } as fn) ->
          let add_new_locals = Option.cons (Stmt.def if_stmt) >> Option.cons (Stmt.def else_stmt) in
          let new_fn : Cfg.Fn.t = { name; formals; locals = add_new_locals locals; entry; exit } in
          Set.remove fns fn |> flip Set.add new_fn
      | None -> fns
      (* [l] is in the main procedure, so don't need to modify [fns]*)
    in

    let assume_ref = D.Ref.Stmt { stmt = assume; name = Nm.Edge (l, if_loc) } in
    let assume_neg_ref = D.Ref.Stmt { stmt = assume_neg; name = Nm.Edge (l, else_loc) } in
    let if_body_ref = D.Ref.Stmt { stmt = if_stmt; name = Nm.Edge (if_loc, post_loc) } in
    let else_body_ref = D.Ref.Stmt { stmt = else_stmt; name = Nm.Edge (else_loc, post_loc) } in
    List.fold (D.contexts_at_loc ~loc:l daig)
      ~init:(g, (new_cfg, new_fns))
      ~f:(fun (daig : D.t) ctx ->
        let l_ref, iter =
          match
            ( D.ref_by_name (Nm.Loc (l, ctx)) daig,
              D.ref_by_name (Nm.Iterate (0, Nm.Loc (l, ctx))) daig )
          with
          | Some r, _ -> (r, fun n -> n)
          | _, Some r -> (r, fun n -> Nm.(Iterate (0, n)))
          | None, None -> failwith "location does not exist"
        in
        let daig = D.dirty_from l_ref daig in
        let post_ref = D.Ref.AState { state = None; name = iter @@ Nm.Loc (post_loc, ctx) } in
        let if_ref = D.Ref.AState { state = None; name = iter @@ Nm.Loc (if_loc, ctx) } in
        let else_ref = D.Ref.AState { state = None; name = iter @@ Nm.Loc (else_loc, ctx) } in
        let if_post_ref =
          D.Ref.AState { state = None; name = iter @@ Nm.Prod (Idx 0, Loc (post_loc, ctx)) }
        in
        let else_post_ref =
          D.Ref.AState { state = None; name = iter @@ Nm.Prod (Idx 1, Loc (post_loc, ctx)) }
        in
        (* first, shift outgoing edges from l to instead go from new location [post_loc] *)
        let g, cfg = daig in
        Seq.fold (N.outputs l_ref g) ~init:g ~f:(fun g edge ->
            E.remove edge g |> E.insert (E.create post_ref (E.dst edge) (E.label edge)))
        (* add "l -[assume cond]-> if_loc", "l -[assume !cond]-> else_loc" edges and refs *)
        |> N.insert assume_ref
        |> N.insert assume_neg_ref |> N.insert if_ref |> N.insert else_ref
        |> E.insert (E.create l_ref if_ref `Transfer)
        |> E.insert (E.create assume_ref if_ref `Transfer)
        |> E.insert (E.create l_ref else_ref `Transfer)
        |> E.insert (E.create assume_neg_ref else_ref `Transfer)
        (* add "if_loc -[if_stmt]-> 0*post_loc", "else_loc -[else_stmt]-> 1*post_loc", "post_loc <- 0*post_loc |_| 1*post_loc" *)
        |> N.insert if_post_ref
        |> N.insert else_post_ref |> N.insert if_body_ref |> N.insert else_body_ref
        |> E.insert (E.create if_body_ref if_post_ref `Transfer)
        |> E.insert (E.create if_ref if_post_ref `Transfer)
        |> E.insert (E.create else_body_ref else_post_ref `Transfer)
        |> E.insert (E.create else_ref else_post_ref `Transfer)
        |> E.insert (E.create if_post_ref post_ref `Join)
        |> E.insert (E.create else_post_ref post_ref `Join)
        |> flip pair cfg)

  let add_loop_at l ~cond ~body daig =
    let g, (cfg, fns) = daig in
    let body_loc = Cfg.Loc.fresh () in
    let exit_loc = Cfg.Loc.fresh () in
    let assume = Stmt.Assume cond in
    let assume_neg = Stmt.Assume (Expr.Unop { op = Unop.Not; e = cond }) in
    let new_cfg =
      let open Cfg.G.Edge in
      Seq.fold (Cfg.G.Node.outputs l cfg) ~init:cfg ~f:(fun cfg edge ->
          remove edge cfg |> insert (create exit_loc (dst edge) (label edge)))
      |> insert (create l body_loc assume)
      |> insert (create l exit_loc assume_neg)
      |> insert (create body_loc l body)
    in

    let new_fns =
      match
        Set.find fns ~f:(fun fn -> Graph.is_reachable (module Cfg.G) cfg (Cfg.Fn.entry fn) l)
      with
      | Some ({ name; formals; locals; entry; exit } as fn) ->
          let locals = Option.cons (Stmt.def body) locals in
          let exit = if Cfg.Loc.equal l exit then exit_loc else exit in
          let new_fn : Cfg.Fn.t = { name; formals; locals; entry; exit } in
          Set.remove fns fn |> flip Set.add new_fn
      | None -> fns
    in

    let assume_stmt_ref = D.Ref.Stmt { stmt = assume; name = Nm.Edge (l, body_loc) } in
    let assume_neg_stmt_ref = D.Ref.Stmt { stmt = assume_neg; name = Nm.Edge (l, exit_loc) } in
    let backedge_stmt_ref = D.Ref.Stmt { stmt = body; name = Nm.Edge (body_loc, l) } in

    List.fold (D.contexts_at_loc ~loc:l daig)
      ~init:(g, (new_cfg, new_fns))
      ~f:(fun (daig : D.t) ctx ->
        let l_ref, contents =
          match
            (D.ref_by_name (Nm.Loc (l, ctx)) daig, D.ref_by_name (Nm.Iterate (0, Loc (l, ctx))) daig)
          with
          | Some (AState { state; name = _ } as r), None -> (r, state)
          | _ -> failwith "Can only insert loops in straightline code"
        in
        let body_ref =
          D.Ref.AState { state = None; name = Nm.Iterate (0, Nm.Loc (body_loc, ctx)) }
        in
        let exit_ref = D.Ref.AState { state = None; name = Nm.Loc (exit_loc, ctx) } in
        let l_zero_ref =
          D.Ref.AState { state = contents; name = Nm.Iterate (0, Nm.Loc (l, ctx)) }
        in
        let l_one_ref = D.Ref.AState { state = None; name = Nm.Iterate (1, Loc (l, ctx)) } in
        let prewiden_ref =
          D.Ref.AState
            {
              state = None;
              name = Nm.Prod (Nm.Iterate (0, Nm.Loc (l, ctx)), Nm.Iterate (1, Nm.Loc (l, ctx)));
            }
        in
        (* first, shift incoming edges to l to instead go to its 0th iterate [l^(0)],
           and outgoing edges from l to instead go from the new [exit_loc] *)
        let g, cfg = daig in
        Seq.fold (N.inputs l_ref g) ~init:g ~f:(fun g edge ->
            E.remove edge g |> E.(insert (create (src edge) l_zero_ref (label edge))))
        |> fun g ->
        Seq.fold (N.outputs l_ref g) ~init:g ~f:(fun g edge ->
            E.remove edge g |> E.insert E.(create exit_ref (dst edge) (label edge)))
        (* next, add transfers for loop body and backedge *)
        |> N.insert assume_stmt_ref
        |> N.insert backedge_stmt_ref
        |> E.insert (E.create assume_stmt_ref body_ref `Transfer)
        |> E.insert (E.create l_zero_ref body_ref `Transfer)
        |> E.insert (E.create backedge_stmt_ref prewiden_ref `Transfer)
        |> E.insert (E.create body_ref prewiden_ref `Transfer)
        (* finally, widen l0 and prewiden to l1, fix l1 and l0 to l, and assume !cond to loop exit *)
        |> E.insert (E.create l_zero_ref l_one_ref `Widen)
        |> E.insert (E.create prewiden_ref l_one_ref `Widen)
        |> E.insert (E.create l_zero_ref l_ref `Fix)
        |> E.insert (E.create l_one_ref l_ref `Fix)
        |> N.insert assume_neg_stmt_ref
        |> E.insert (E.create l_ref exit_ref `Transfer)
        |> E.insert (E.create assume_neg_stmt_ref exit_ref `Transfer)
        |> flip pair cfg |> D.dirty_from exit_ref
        $> fun _ -> D.Ref.dirty l_ref)

  let functions = ref (-1)

  let add_fresh_fn daig =
    let g, (cfg, fns) = daig in
    let entry = Cfg.Loc.fresh () in
    let exit = Cfg.Loc.fresh () in
    functions := !functions + 1;
    let name = "f" ^ Int.to_string @@ !functions in
    let formal = gen_arith_ident () in
    let body_stmt = Ast.Stmt.Assign { lhs = Cfg.retvar; rhs = Ast.Expr.Var formal } in
    let new_fn =
      Cfg.Fn.make ~name ~formals:[ formal ] ~entry ~exit ~body:[ (entry, exit, body_stmt) ]
    in
    (g, (Cfg.G.Edge.(insert (create entry exit body_stmt) cfg), Cfg.Fn.Set.add fns new_fn))
    |> pair name

  (** Generate a random arithmetic [Ast.Expr.t], uniformly drawn from:
   * number literal
   * binop of two random arithmetic exprs
   * negation of random arithmetic expr
   * array access
   * arithmetic variable
   *)
  let rec gen_arith_expr () =
    let open Ast in
    match Random.int 5 with
    | 0 -> Expr.Lit (Lit.Int (Random.int 100))
    | 1 ->
        let op =
          match Random.int 5 with
          | 0 -> Binop.Plus
          | 1 -> Binop.Minus
          | 2 -> Binop.Times
          | 3 -> Binop.Divided_by
          | _ -> Binop.Mod
        in
        Expr.Binop { l = gen_arith_expr (); op; r = gen_arith_expr () }
    | 2 -> Expr.Unop { op = Unop.Neg; e = gen_arith_expr () }
    | 3 ->
        Expr.Deref
          {
            rcvr = Expr.Var ("arr" ^ Int.to_string @@ Random.int !array_vars);
            field = gen_arith_expr ();
          }
    | _ -> Expr.Var ("a" ^ Int.to_string @@ Random.int !arith_vars)

  (** Generate a random boolean [Ast.Expr.t], uniformly drawn from:
   * boolean literal
   * inequality of two random arithmetic exprs
   * negation of random boolean expr
   * boolean variable
*)
  let rec gen_bool_expr () =
    let open Ast in
    match Random.int 4 with
    (*    | 0 -> Expr.Lit (Lit.Bool (Random.bool ()))*)
    | 1 ->
        let op =
          match Random.int 6 with
          | 0 -> Binop.Lt
          | 1 -> Binop.Le
          | 2 -> Binop.Gt
          | 3 -> Binop.Ge
          | 4 -> Binop.Eq
          | _ -> Binop.NEq
        in
        Expr.Binop { l = gen_arith_expr (); op; r = gen_arith_expr () }
    | 2 -> Expr.Unop { op = Unop.Not; e = gen_bool_expr () }
    | _ -> Expr.Var ("b" ^ Int.to_string @@ Random.int !bool_vars)

  (**
   * Add a function call at a given location in a daig.  Callee is sampled at random, after adding a new function to the program if needed to avoid recursion or with 20% probability otherwise.
   * Avoid recursion by imposing a total order on functions [main < f1 < f2 < f3 < ...] and allowing a call from [f] to [g] only when [f < g]
   *)
  let add_call_at loc daig =
    let caller =
      Cfg.containing_fn loc (snd daig)
      >>| (Cfg.Fn.name >> flip String.drop_prefix 1 >> Int.of_string)
    in
    let callee, daig =
      if
        !functions < 0
        || Option.exists caller ~f:(Int.equal !functions)
        || Int.equal 0 (Random.int 2)
      then add_fresh_fn daig
      else
        let callee =
          Option.(map caller ~f:Int.succ |> value ~default:0)
          |> flip Random.int_incl !functions |> Int.to_string |> ( ^ ) "f"
        in
        callee,daig
    in
    let call =
      Ast.Stmt.Call { lhs = gen_arith_ident (); fn = callee; actuals = [ gen_arith_expr () ] }
    in
    D.add_stmt_at loc call daig

  (** Generate a single random [Ast.Stmt.t] with the following distribution:
   * 80% [Assign] (x := e)
     * 75% of assignments are to arithmetic variables, 25% to bools
   * 10% [Assume] (assume e)
   * 10% [Skip] (skip)
   *)
  let gen_stmt () =
    let open Ast.Stmt in
    match Random.int 10 with
    | 0 -> Skip
    | 1 -> Assume (gen_bool_expr ())
    | 2 | 3 -> Assign { lhs = gen_bool_ident (); rhs = gen_bool_expr () }
    | _ -> Assign { lhs = gen_arith_ident (); rhs = gen_arith_expr () }

  (** Edit [daig]'s underlying program at random, according to the following distribution:
      * 50% insert a statement 
      * 20% modify a statement TODO
      * 20% insert a conditional
      * 10% insert a while loop
      *)
  let random_edit daig =
    let sample_loc_no_fn_exits () =
      let fn_exits = (snd >> snd) daig |> Set.to_list |> List.map ~f:Cfg.Fn.exit in
      let l = ref (Cfg.Loc.sample ()) in
      while List.exists fn_exits ~f:(Cfg.Loc.equal !l) do
        l := Cfg.Loc.sample ()
      done;
      !l
    in
    match Random.int 20 with
    | 0 ->
        (* no nested loops, so keep sampling locations until finding one not in any loop *)
        let l = ref (Cfg.Loc.sample ()) in
        let _, (cfg, fns) = daig in
        let fn_entries = Set.to_list fns |> List.map ~f:Cfg.Fn.entry in
        let locs_in_loops =
          List.append (Cfg.loop_heads cfg) (Map.keys @@ Cfg.containing_loop_heads cfg)
        in
        let equals_l x = Cfg.Loc.equal !l x in
        while
          equals_l Cfg.Loc.entry
          || List.exists fn_entries ~f:equals_l
          || List.exists locs_in_loops ~f:equals_l
        do
          l := Cfg.Loc.sample ()
        done;
        add_loop_at !l ~cond:(gen_bool_expr ()) ~body:(gen_stmt ()) daig
    | 1 | 2 ->
        let l = sample_loc_no_fn_exits () in
        add_ite_at l ~cond:(gen_bool_expr ()) ~if_stmt:(gen_stmt ()) ~else_stmt:(gen_stmt ()) daig
    | 3 ->
        let l = sample_loc_no_fn_exits () in
        add_call_at l daig
    | _ ->
        let l = sample_loc_no_fn_exits () in
        D.add_stmt_at l (gen_stmt ()) daig

  let issue_random_query daig = D.get_by_loc (Cfg.Loc.sample ()) Dom.Ctx.init daig |> snd

  let issue_exit_query daig = D.get_by_loc Cfg.Loc.exit Dom.Ctx.init daig |> snd
end

(*module AB = Make (Array_bounds)

let cond = Expr.Var "cond"

let stmt1 = Stmt.Assign { lhs = "lorem"; rhs = Expr.Lit (Lit.String "ipsum") }

let stmt2 = Stmt.Assign { lhs = "foo"; rhs = Expr.Lit (Lit.String "bar") }

let%test "add an ite before a while loop" =
  Random.init 12345;

  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json) (Util.test_case "while_syntax")
  in
  let daig = AB.D.of_cfg cfg in
  let edited_daig = AB.add_ite_at Cfg.Loc.entry ~cond ~if_stmt:stmt1 ~else_stmt:stmt2 daig in
  AB.D.dump_dot edited_daig ~filename:"edit_test1.dot";
  true

let%test "add an ite inside of a while loop" =
  Random.init 12345;
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      (Util.test_case "while_syntax")
  in
  let daig = AB.D.of_cfg cfg in
  let edited_daig =
    AB.add_ite_at Cfg.Loc.(of_int_unsafe 2) ~cond ~if_stmt:stmt1 ~else_stmt:stmt2 daig
  in
  AB.D.dump_dot edited_daig ~filename:"edit_test2.dot";
  true

let%test "add an ite inside of an ite" =
  Random.init 12345;
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      (Util.test_case "arith_syntax")
  in
  let daig = AB.D.of_cfg cfg in
  let edited_daig =
    AB.add_ite_at Cfg.Loc.(of_int_unsafe 3) ~cond ~if_stmt:stmt1 ~else_stmt:stmt2 daig
  in
  AB.D.dump_dot edited_daig ~filename:"edit_test3.dot";
  true

let%test "add a while inside of an ite" =
  Random.init 12345;
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      (Util.test_case "arith_syntax")
  in
  let daig = AB.D.of_cfg cfg in
  let edited_daig = AB.add_loop_at Cfg.Loc.(of_int_unsafe 3) ~cond ~body:stmt1 daig in
  AB.D.dump_dot edited_daig ~filename:"edit_test4.dot";
  true

let%test "add a while in straightline code" =
  Random.init 12345;
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      (Util.test_case "arith_syntax")
  in
  let daig = AB.D.of_cfg cfg in
  let edited_daig = AB.add_loop_at Cfg.Loc.(of_int_unsafe 1) ~cond ~body:stmt1 daig in
  AB.D.dump_dot edited_daig ~filename:"edit_test5.dot";
  true

let%test "fuzz 100 edits/queries" =
  Random.init 12345;
  Cfg.Loc.reset ();
  let init = AB.D.of_cfg @@ Cfg.empty () in
  ignore @@ apply_n_times ~n:100 ~f:(AB.random_edit >> AB.issue_random_query) ~init;
  true
 *)
