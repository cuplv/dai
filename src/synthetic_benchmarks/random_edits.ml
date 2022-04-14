open Dai
open Import
open Syntax
open Ast
open Frontend
module Fn = Cfg.Fn

let sample_list_exn xs =
  let n = Random.int (List.length xs) in
  List.nth_exn xs n

let sample_seq_exn xs =
  let n = Random.int (Seq.length xs) in
  Seq.nth_exn xs n

let rng_state = Splittable_random.State.of_int (Caml.Random.int 1000)

let query_sample_list_exn xs =
  let n = Splittable_random.int rng_state ~lo:0 ~hi:(List.length xs - 1) in
  List.nth_exn xs n

let query_sample_seq_exn xs =
  let n = Splittable_random.int rng_state ~lo:0 ~hi:(Seq.length xs - 1) in
  Seq.nth_exn xs n

module G = Analysis.Dsg.Make (Domain.Oct_array_bounds)
module D = G.D
module N = Cfg.G.Node
module E = Cfg.G.Edge
module Nm = D.Name
module H = Hashtbl.Make (Int)

let functions = ref 0

let fn_of_int = Int.to_string >> ( ^ ) "f"

let int_of_fn = flip String.drop_prefix 1 >> Int.of_string

let callgraph = ref Callgraph.empty

let entrypoint_ref = ref None

let entrypoint dsg =
  match !entrypoint_ref with
  | Some e -> e
  | None ->
      let f0 = fn_of_int 0 in
      let e =
        Fn.Map.keys dsg
        |> List.find_exn ~f:(fun (f : Fn.t) -> String.equal f0 f.method_id.method_name)
      in
      entrypoint_ref := Some e;
      e

(** For each [gen_*_ident], generate a new identifier 20% of time, sample a previously-generated one 80% of time.
      Maintains a separate counter of previously-used identifiers per procedure.
  *)

let arith_vars : int ref H.t = H.create ~size:512 ()

let gen_arith_ident (fn : Fn.t) =
  let fn_id = int_of_fn fn.method_id.method_name in
  match H.find arith_vars fn_id with
  | None ->
      H.add_exn arith_vars ~key:fn_id ~data:(ref 1);
      "a0"
  | Some vs ->
      if phys_equal 0 (Random.int 5) then Int.incr vs;
      "a" ^ Int.to_string @@ Random.int !vs

let bool_vars : int ref H.t = H.create ~size:512 ()

let gen_bool_ident (fn : Fn.t) =
  let fn_id = int_of_fn fn.method_id.method_name in
  match H.find bool_vars fn_id with
  | None ->
      H.add_exn bool_vars ~key:fn_id ~data:(ref 1);
      "b0"
  | Some vs ->
      if phys_equal 0 (Random.int 5) then Int.incr vs;
      "b" ^ Int.to_string @@ Random.int !vs

let array_vars : int ref H.t = H.create ~size:512 ()

let gen_array_ident (fn : Fn.t) =
  let fn_id = int_of_fn fn.method_id.method_name in
  match H.find array_vars fn_id with
  | None ->
      H.add_exn array_vars ~key:fn_id ~data:(ref 1);
      "arr0"
  | Some vs ->
      if phys_equal 0 (Random.int 5) then Int.incr vs;
      "arr" ^ Int.to_string @@ Random.int !vs

(** Generate a random arithmetic [Ast.Expr.t], uniformly drawn from:
   * number literal
   * binop of two random arithmetic exprs
   * negation of random arithmetic expr
   * array access
   * arithmetic variable
   *)
let rec gen_arith_expr fn =
  match Random.int 5 with
  | 0 -> Expr.Lit (Lit.Float (Random.float 100. |> Float.sub 50.))
  | 1 ->
      let op =
        match Random.int 5 with
        | 0 -> Binop.Plus
        | 1 -> Binop.Minus
        | 2 -> Binop.Times
        | 3 -> Binop.Divided_by
        | _ -> Binop.Mod
      in
      Expr.Binop { l = gen_arith_expr fn; op; r = gen_arith_expr fn }
  | 2 -> Expr.Unop { op = Unop.Neg; e = gen_arith_expr fn }
  | 3 -> Expr.Array_access { rcvr = Expr.Var (gen_array_ident fn); idx = gen_arith_expr fn }
  | _ -> Expr.Var (gen_arith_ident fn)

(** Generate a random boolean [Expr.t], uniformly drawn from:
   * boolean literal
   * inequality of two random arithmetic exprs
   * negation of random boolean expr
   * boolean variable
*)
let rec gen_bool_expr fn =
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
      Expr.Binop { l = gen_arith_expr fn; op; r = gen_arith_expr fn }
  | 2 -> Expr.Unop { op = Unop.Not; e = gen_bool_expr fn }
  | _ -> Expr.Var (gen_bool_ident fn)

let gen_array_literal (fn : Fn.t) =
  let length = 1 + Random.int 20 in
  let rec elts = function 0 -> [] | k -> gen_arith_expr fn :: elts (k - 1) in
  Expr.Array_literal { elts = elts length; alloc_site = Alloc_site.fresh () }

(** Generate a single random [Stmt.t] with the following distribution:
   * 70% [Assign] (x := e)
     * 75% of assignments are to arithmetic variables, 25% to bools
   * 10% [Skip] (skip)
   *)
let gen_stmt fn =
  let open Stmt in
  match Random.int 10 with
  | 0 -> Skip
  | 1 -> Assign { lhs = gen_bool_ident fn; rhs = gen_bool_expr fn }
  | 2 | 3 -> Assign { lhs = gen_array_ident fn; rhs = gen_array_literal fn }
  | _ -> Assign { lhs = gen_arith_ident fn; rhs = gen_arith_expr fn }

let dummy_method_id : Method_id.t =
  {
    package = [ "foo"; "bar" ];
    class_name = "Dummy";
    method_name = "doThing";
    static = true;
    arg_types = [ "int" ];
  }

let dummy_edit_result : Tree_diff.cfg_edit_result =
  {
    cfg = Graph.create (module Cfg.G) ();
    new_loc_map = Loc_map.empty;
    added_edges = [];
    deleted_edges = [];
    added_loc = None;
    added_for_loop_backedge = None;
  }

let gen_entry_state () =
  let randfloat lo hi = Splittable_random.float rng_state ~lo ~hi in
  let inf = randfloat (-10.) 10. in
  let inf' = randfloat (-10.) 10. in
  let inf'' = randfloat (-10.) 10. in
  let sup = inf +. randfloat 0. 20. in
  let sup' = inf' +. randfloat 0. 20. in
  let sup'' = inf'' +. randfloat 0. 20. in
  Domain.Oct_array_bounds.of_alist [ ("a0", inf, sup); ("a1", inf', sup'); ("a2", inf'', sup'') ]

let add_fn dsg : G.t =
  let method_name = fn_of_int !functions in
  Int.incr functions;
  let method_id : Method_id.t = { dummy_method_id with method_name } in
  let fn : Fn.t =
    {
      method_id;
      formals = [ "a0"; "a1"; "a2" ];
      locals = [];
      entry = Cfg.Loc.fresh ();
      exit = Cfg.Loc.fresh ();
      exc_exit = Cfg.Loc.fresh ();
    }
  in
  ignore @@ gen_arith_ident fn;
  ignore @@ gen_arith_ident fn;
  let edges =
    let intermediate_loc = Cfg.Loc.fresh () in
    let stmt = gen_stmt fn in
    let ret_stmt = Stmt.Assign { lhs = Cfg.retvar; rhs = gen_arith_expr fn } in
    [ (fn.entry, intermediate_loc, stmt); (intermediate_loc, fn.exit, ret_stmt) ]
  in
  let cfg = Graph.create (module Cfg.G) ~edges () in
  let cfgs = Fn.Map.(empty |> add_exn ~key:fn ~data:cfg) in
  G.add_exn dsg ~cfgs

let add_to_cfg ~at_loc ~new_loc cfg added_edges =
  let rebased_edges = ref [] in
  let cfg =
    Seq.fold (N.outputs at_loc cfg) ~init:cfg ~f:(fun cfg edge ->
        let new_edge = E.(new_loc, dst edge, label edge) in
        rebased_edges := new_edge :: !rebased_edges;
        E.remove edge cfg |> E.(insert (uncurry3 create new_edge)))
    |> fun init ->
    List.fold added_edges ~init ~f:(fun acc (src, dst, lbl) ->
        let e = E.create src dst lbl in
        E.insert e acc)
  in

  { dummy_edit_result with cfg; added_edges; added_loc = Some new_loc }

let add_stmt_at at_loc cfg fn =
  let new_loc = Cfg.Loc.fresh () in
  add_to_cfg ~at_loc ~new_loc cfg [ (at_loc, new_loc, gen_stmt fn) ]

let add_loop_at at_loc cfg fn =
  let loop_head = Cfg.Loc.fresh () in
  let body_loc = Cfg.Loc.fresh () in
  let exit_loc = Cfg.Loc.fresh () in
  let edges =
    let cond = gen_bool_expr fn in
    let body_stmt = gen_stmt fn in
    let assume = Stmt.Assume cond in
    let assume_neg = Stmt.Assume (Expr.Unop { op = Unop.Not; e = cond }) in
    [
      (at_loc, loop_head, Stmt.Skip);
      (loop_head, body_loc, assume);
      (loop_head, exit_loc, assume_neg);
      (body_loc, loop_head, body_stmt);
    ]
  in
  add_to_cfg ~at_loc ~new_loc:exit_loc cfg edges

let add_call_at at_loc cfg (caller : Fn.t) dsg =
  let new_loc = Cfg.Loc.fresh () in
  let caller_idx = caller.method_id.method_name |> int_of_fn in
  let dsg =
    if Int.equal !functions (succ caller_idx) || Int.equal 0 (Random.int 20) then add_fn dsg
    else dsg
  in
  let callee_name =
    let min = caller_idx in
    let max = !functions in
    fn_of_int (min + Random.int (max - min))
  in
  let callee : Fn.t =
    List.find_exn (Fn.Map.keys dsg) ~f:(fun fn -> String.equal fn.method_id.method_name callee_name)
  in
  callgraph := Callgraph.add ~caller ~callee !callgraph;
  let call =
    Stmt.Call
      {
        lhs = Some (gen_arith_ident caller);
        rcvr = "this";
        meth = callee_name;
        actuals = [ gen_arith_expr caller; gen_arith_expr caller; gen_arith_expr caller ];
        alloc_site = None;
      }
  in
  (dsg, add_to_cfg ~at_loc ~new_loc cfg [ (at_loc, new_loc, call) ])

let add_ite_at at_loc cfg fn : Tree_diff.cfg_edit_result =
  let exit_loc = Cfg.Loc.fresh () in
  let if_loc = Cfg.Loc.fresh () in
  let else_loc = Cfg.Loc.fresh () in
  let edges =
    let cond = gen_bool_expr fn in
    let assume = Stmt.Assume cond in
    let assume_neg = Stmt.Assume (Expr.Unop { op = Unop.Not; e = cond }) in
    [
      (at_loc, if_loc, assume);
      (at_loc, else_loc, assume_neg);
      (if_loc, exit_loc, gen_stmt fn);
      (else_loc, exit_loc, gen_stmt fn);
    ]
  in
  add_to_cfg ~at_loc ~new_loc:exit_loc cfg edges

let random_edit dsg =
  let fn : Fn.t = Fn.Map.keys dsg |> sample_list_exn in
  let cfg, _ = Fn.Map.find_exn dsg fn in
  let excluded_locs = fn.entry :: fn.exit :: fn.exc_exit :: Cfg.loop_heads cfg in
  let locs =
    let f l = not (List.mem excluded_locs l ~equal:Cfg.Loc.equal) in
    Cfg.G.nodes cfg |> Seq.filter ~f
  in
  assert (not @@ Seq.is_empty locs);
  let l = sample_seq_exn locs in

  let dsg, cfg_edit =
    match Random.int 20 with
    | 0 -> (dsg, add_loop_at l cfg fn)
    | 1 | 2 -> add_call_at l cfg fn dsg
    | 3 | 4 -> (dsg, add_ite_at l cfg fn)
    | _ -> (dsg, add_stmt_at l cfg fn)
  in
  (* dummy tree edit *)
  let edit = Tree_diff.Add_statements { method_id = fn.method_id; at_loc = l; stmts = [] } in
  let new_daigs =
    Map.map
      (Map.find_exn dsg fn |> snd)
      ~f:(fun daig ->
        let res = D.apply_edit ~daig ~cfg_edit ~fn edit in
        D.assert_wf res;
        res)
  in
  let dsg = Map.set dsg ~key:fn ~data:(cfg_edit.cfg, new_daigs) in
  G.dirty_interproc_deps fn dsg

let random_query dsg =
  let _, dsg =
    let fn = Fn.Map.keys dsg |> query_sample_list_exn in
    let cfg, _ = Fn.Map.find_exn dsg fn in
    let loc = Cfg.G.nodes cfg |> query_sample_seq_exn in
    let cg = !callgraph in
    let fields = Declared_fields.empty in
    let entry_state = gen_entry_state () in
    G.query ~fn ~loc ~cg ~fields ~entry_state dsg
  in
  dsg

let exit_query dsg =
  let _, dsg =
    let fn = entrypoint dsg in
    let loc = fn.exit in
    let cg = !callgraph in
    let fields = Declared_fields.empty in
    let entry_state = Domain.Oct_array_bounds.init () in
    G.query ~fn ~loc ~cg ~fields ~entry_state dsg
  in
  dsg

let init () =
  (*
    callgraph := Callgraph.empty;
    functions := 0;
    entrypoint_ref := None;
    H.clear arith_vars;
    H.clear bool_vars;
    H.clear array_vars;*)
  add_fn (G.init ~cfgs:Fn.Map.empty)
