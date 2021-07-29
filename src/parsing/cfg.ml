open Dai.Import
open Tree_sitter_java
module Loc = Dai.Cfg.Loc

type edge = Loc.t * Loc.t * Dai.Ast.Stmt.t

let tmp_var_counter = ref 0

let fresh_tmp_var () =
  let v = "_dai_tmp" ^ Int.to_string !tmp_var_counter in
  tmp_var_counter := !tmp_var_counter + 1;
  v

(** Convert an expression concrete syntax tree to an expression in our IR, along with potentially some preceding statements for any function invocations and assignments therein, and a shifted current program location to accomodate those intermediate statements.
    That is, 
      * if `cst` represents a simple expression with no function invocations or assignments, return value is (<that expression in our IR>, (curr_loc,[]))
      * if `cst` contains function invocations f_1 ... f_k and assignments x_1=e_1 ... x_n=e_n, return value is 
          (
             <that expression in our IR>[ tmp_var_i / f_i][x_i / x_i=e_i],
             Some (fresh_loc_k+n, [curr_loc -[tmp_var_1 := f_1]-> fresh_loc_1, ... , fresh_loc_(k-1) -[tmp_var_k := f_k]-> fresh_loc_k] ++ [fresh_loc_k -[x_1=e_1]-> fresh_loc_k+1, ... , fresh_loc_(k+n-1) -[x_n=e_n]-> fresh_loc_(k+n)])
*)
let rec expr (curr_loc : Loc.t) (cst : CST.expression) : Dai.Ast.Expr.t * (Loc.t * edge list) =
  let open Dai.Ast in
  match cst with
  | `Assign_exp (lhs, op, rhs) ->
      let rhs_expr, (curr_loc, rhs_intermediates) = expr curr_loc rhs in
      let lhs_expr, (curr_loc, lhs_intermediates) =
        match lhs with
        | `Id (_, ident) -> (Expr.Var ident, (curr_loc, []))
        | `Choice_open _ -> failwith "todo: Choice_open"
        | `Field_access (rcvr, _, _, field) ->
            let rcvr, aux_info =
              match rcvr with
              | `Super _ -> (Expr.Var "super", (curr_loc, []))
              | `Prim_exp _ as e ->
                  expr_as_var curr_loc e |> fun (v, aux_info) -> (Expr.Var v, aux_info)
            in
            let field =
              match field with
              | `Id (_, fld) -> fld
              | `Choice_open _ -> failwith "todo: Choice_open"
              | `This _ -> "this"
            in
            (Expr.Deref { rcvr; field }, aux_info)
        | `Array_access (rcvr, _, idx, _) ->
            let idx, (curr_loc, idx_intermediates) = expr curr_loc idx in
            let rcvr, (curr_loc, rcvr_intermediates) = expr_as_var curr_loc (`Prim_exp rcvr) in
            ( Expr.Array_access { rcvr = Expr.Var rcvr; idx },
              (curr_loc, idx_intermediates @ rcvr_intermediates) )
      in
      let rhs_expr_with_op =
        match op with
        | `EQ _ -> rhs_expr
        | `PLUSEQ _ -> Expr.Binop { l = lhs_expr; op = Binop.Plus; r = rhs_expr }
        | `DASHEQ _ -> Expr.Binop { l = lhs_expr; op = Binop.Minus; r = rhs_expr }
        | `STAREQ _ -> Expr.Binop { l = lhs_expr; op = Binop.Times; r = rhs_expr }
        | `SLASHEQ _ -> Expr.Binop { l = lhs_expr; op = Binop.Divided_by; r = rhs_expr }
        | `AMPEQ _ -> failwith "todo: AMPEQ"
        | `BAREQ _ -> failwith "todo: BAREQ"
        | `HATEQ _ -> failwith "todo: HATEQ"
        | `PERCEQ _ -> failwith "todo: PERCEQ"
        | `LTLTEQ _ -> failwith "todo: LTLTEQ"
        | `GTGTEQ _ -> failwith "todo: GTGTEQ"
        | `GTGTGTEQ _ -> failwith "todo: GTGTGTEQ"
      in
      let stmt =
        match lhs with
        | `Id (_, lhs) -> Stmt.Assign { lhs; rhs = rhs_expr_with_op }
        | `Choice_open _ -> failwith "todo: Choice_open"
        | `Field_access _ ->
            (* rcvr guaranteed to be a Var by expr_as_var call above *)
            let rcvr, field =
              match lhs_expr with
              | Expr.Deref { rcvr = Expr.Var v; field } -> (v, field)
              | _ -> failwith "unreachable"
            in
            Stmt.Write { rcvr; field; rhs = rhs_expr_with_op }
        | `Array_access _ ->
            let rcvr, idx =
              match lhs_expr with
              | Expr.Array_access { rcvr = Expr.Var v; idx } -> (v, idx)
              | _ -> failwith "unreachable"
            in
            Stmt.Array_write { rcvr; idx; rhs = rhs_expr_with_op }
      in
      let next_loc = Loc.fresh () in
      (lhs_expr, (next_loc, (curr_loc, next_loc, stmt) :: (rhs_intermediates @ lhs_intermediates)))
  | `Bin_exp b ->
      let l, op, r =
        match b with
        | `Exp_GT_exp (e1, _, e2) -> (e1, Binop.Gt, e2)
        | `Exp_LT_exp (e1, _, e2) -> (e1, Binop.Lt, e2)
        | `Exp_GTEQ_exp (e1, _, e2) -> (e1, Binop.Ge, e2)
        | `Exp_LTEQ_exp (e1, _, e2) -> (e1, Binop.Le, e2)
        | `Exp_EQEQ_exp (e1, _, e2) -> (e1, Binop.Eq, e2)
        | `Exp_BANGEQ_exp (e1, _, e2) -> (e1, Binop.NEq, e2)
        | `Exp_AMPAMP_exp (e1, _, e2) -> (e1, Binop.And, e2)
        | `Exp_BARBAR_exp (e1, _, e2) -> (e1, Binop.Or, e2)
        | `Exp_PLUS_exp (e1, _, e2) -> (e1, Binop.Plus, e2)
        | `Exp_DASH_exp (e1, _, e2) -> (e1, Binop.Minus, e2)
        | `Exp_STAR_exp (e1, _, e2) -> (e1, Binop.Times, e2)
        | `Exp_SLASH_exp (e1, _, e2) -> (e1, Binop.Divided_by, e2)
        | `Exp_AMP_exp (_, _, _) -> failwith "todo: Exp_AMP_exp"
        | `Exp_BAR_exp (_, _, _) -> failwith "todo: Exp_BAR_exp"
        | `Exp_HAT_exp (_, _, _) -> failwith "todo: Exp_HAT_exp"
        | `Exp_PERC_exp (e1, _, e2) -> (e1, Binop.Mod, e2)
        | `Exp_LTLT_exp (_, _, _) -> failwith "todo: Exp_LTLT_exp"
        | `Exp_GTGT_exp (_, _, _) -> failwith "todo: Exp_GTGT_exp"
        | `Exp_GTGTGT_exp (_, _, _) -> failwith "todo: Exp_GTGTGT_exp"
      in
      let l, (curr_loc, l_intermediates) = expr curr_loc l in
      let r, (curr_loc, r_intermediates) = expr curr_loc r in
      (Expr.Binop { l; op; r }, (curr_loc, l_intermediates @ r_intermediates))
  | `Cast_exp (_, _type, _, _, e) -> expr curr_loc e
  | `Inst_exp _ -> failwith "todo: Inst_exp"
  | `Lambda_exp _ -> failwith "todo: Lambda_exp"
  | `Prim_exp (`Lit l) ->
      let e =
        Expr.Lit
          ( match l with
          | `Deci_int_lit (_, raw)
          | `Hex_int_lit (_, raw)
          | `Octal_int_lit (_, raw)
          | `Bin_int_lit (_, raw) ->
              Lit.Int (Int.of_string raw)
          | `Deci_floa_point_lit (_, raw) | `Hex_floa_point_lit (_, raw) ->
              Lit.Float (Float.of_string raw)
          | `True _ -> Lit.Bool true
          | `False _ -> Lit.Bool false
          | `Char_lit (_, raw) -> Lit.Char (Char.of_string raw)
          | `Str_lit (_, raw) ->
              let contents = String.slice raw 1 (String.length raw - 1) in
              Lit.String contents
          | `Null_lit _ -> Lit.Null )
      in
      (e, (curr_loc, []))
  | `Prim_exp (`Class_lit _) -> failwith "todo: Class_lit"
  | `Prim_exp (`This _) -> (Expr.Var "this", (curr_loc, []))
  | `Prim_exp (`Id (_, ident)) -> (Expr.Var ident, (curr_loc, []))
  | `Prim_exp (`Choice_open _) -> failwith "todo: Prim_exp (Choice_open) "
  | `Prim_exp (`Paren_exp (_, e, _)) -> expr curr_loc e
  | `Prim_exp (`Obj_crea_exp (`Unqu_obj_crea_exp unqualified))
  | `Prim_exp (`Obj_crea_exp (`Prim_exp_DOT_unqu_obj_crea_exp (_, _, unqualified))) ->
      (* todo: deal with fully-qualified constructors *)
      let _, _targs, typ, (_, args, _), _initializer = unqualified in
      let ctor_name =
        match typ with `Id (_, typ) -> typ | _ -> failwith "unrecognized constructor simple-type"
      in
      let args = match args with Some (e, es) -> e :: List.map ~f:snd es | None -> [] in
      let actuals, (curr_loc, arg_intermediates) =
        List.fold args
          ~init:([], (curr_loc, []))
          ~f:(fun (acc_exprs, (curr_loc, acc_intermediates)) arg ->
            let arg_expr, (curr_loc, arg_intermediates) = expr curr_loc arg in
            (acc_exprs @ [ arg_expr ], (curr_loc, acc_intermediates @ arg_intermediates)))
      in
      let lhs = fresh_tmp_var () in
      let next_loc = Loc.fresh () in
      let call =
        (curr_loc, next_loc, Stmt.Call { lhs; rcvr = ctor_name; meth = "constructor"; actuals })
      in
      (Expr.Var lhs, (next_loc, arg_intermediates @ [ call ]))
  | `Prim_exp (`Field_access (rcvr, _, _, field)) ->
      let rcvr, aux_info =
        match rcvr with
        | `Super _ -> (Expr.Var "super", (curr_loc, []))
        | `Prim_exp _ as e -> expr_as_var curr_loc e |> fun (v, aux_info) -> (Expr.Var v, aux_info)
      in
      let field =
        match field with
        | `Id (_, fld) -> fld
        | `Choice_open _ -> failwith "todo: Choice_open"
        | `This _ -> "this"
      in
      (Expr.Deref { rcvr; field }, aux_info)
  | `Prim_exp (`Array_access (rcvr, _, idx, _)) ->
      let idx, (curr_loc, idx_intermediates) = expr curr_loc idx in
      let rcvr, (curr_loc, rcvr_intermediates) = expr_as_var curr_loc (`Prim_exp rcvr) in
      ( Expr.Array_access { rcvr = Expr.Var rcvr; idx },
        (curr_loc, idx_intermediates @ rcvr_intermediates) )
  | `Prim_exp (`Meth_invo (rcvr_and_meth, (_, args, _))) ->
      let rcvr, meth, (curr_loc, rcvr_intermediates) =
        match rcvr_and_meth with
        | `Choice_id (`Id (_, meth)) -> ("this", meth, (curr_loc, []))
        | `Choice_id (`Choice_open _) -> failwith "todo: Choice_id(Choice_open)"
        | `Choice_prim_exp_DOT_opt_super_DOT_opt_type_args_choice_id
            (rcvr, _dot, super, _typeargs, meth) -> (
            let rcvr, aux_info =
              match rcvr with
              | `Prim_exp _ as e when Option.is_none super -> expr_as_var curr_loc e
              | _ -> failwith "todo: handle \"super\" in method invocations"
            in
            match meth with
            | `Id (_, meth_name) -> (rcvr, meth_name, aux_info)
            | `Choice_open _ -> failwith "todo: Choice_open in Meth_invo" )
      in
      let args = match args with Some (e, es) -> e :: List.map ~f:snd es | None -> [] in
      let actuals, (curr_loc, arg_intermediates) =
        List.fold args
          ~init:([], (curr_loc, []))
          ~f:(fun (acc_exprs, (curr_loc, acc_intermediates)) arg ->
            let arg_expr, (curr_loc, arg_intermediates) = expr curr_loc arg in
            (acc_exprs @ [ arg_expr ], (curr_loc, acc_intermediates @ arg_intermediates)))
      in
      let lhs = fresh_tmp_var () in
      let next_loc = Loc.fresh () in
      let call = (curr_loc, next_loc, Stmt.Call { lhs; rcvr; meth; actuals }) in
      (Expr.Var lhs, (next_loc, rcvr_intermediates @ arg_intermediates @ [ call ]))
  | `Prim_exp (`Meth_ref _) -> failwith "todo: Prim_exp (Meth_ref)"
  | `Prim_exp (`Array_crea_exp _) -> failwith "todo: Prim_exp (Array_crea_exp)"
  | `Switch_exp _ -> failwith "todo: Switch_exp"
  | `Tern_exp (if_exp, _, then_exp, _, else_exp) ->
      let tmp = fresh_tmp_var () in
      let if_exp, (curr_loc, cond_intermediates) = expr curr_loc if_exp in
      let then_branch_head = Loc.fresh () in
      let else_branch_head = Loc.fresh () in
      let next_loc = Loc.fresh () in
      let then_exp, (then_branch_tail, then_intermediates) = expr then_branch_head then_exp in
      let else_exp, (else_branch_tail, else_intermediates) = expr else_branch_head else_exp in
      let stmts =
        (curr_loc, then_branch_head, Stmt.Assume if_exp)
        :: (curr_loc, else_branch_head, Stmt.Assume (Expr.Unop { op = Unop.Not; e = if_exp }))
        :: (then_branch_tail, next_loc, Stmt.Assign { lhs = tmp; rhs = then_exp })
        :: (else_branch_tail, next_loc, Stmt.Assign { lhs = tmp; rhs = else_exp })
        :: (cond_intermediates @ then_intermediates @ else_intermediates)
      in
      (Expr.Var tmp, (curr_loc, stmts))
  | `Un_exp u ->
      let e, op =
        match u with
        | `PLUS_exp (_, e) -> (e, Unop.Plus)
        | `DASH_exp (_, e) -> (e, Unop.Neg)
        | `BANG_exp (_, e) -> (e, Unop.Not)
        | `TILDE_exp (_, e) -> (e, Unop.BNot)
      in
      let e, (curr_loc, intermediates) = expr curr_loc e in
      (Expr.Unop { op; e }, (curr_loc, intermediates))
  | `Update_exp u -> (
      (* translate (pre/post)-(increment/decrement) as follows:
         ++x    -->    let x = x+1 in x
         --x    -->    let x = x-1 in x
         x++    -->    let x = x+1 in x-1
         x--    -->    let x = x-1 in x+1
         except when the operand is not a variable, in which case ignore post-(incr/decr) and interpret pre-(incr/decr) as addition/subtraction of 1
      *)
      let e, op, is_pre =
        match u with
        | `Exp_PLUSPLUS (e, _) -> (e, Binop.Plus, false)
        | `Exp_DASHDASH (e, _) -> (e, Binop.Minus, false)
        | `PLUSPLUS_exp (_, e) -> (e, Binop.Plus, true)
        | `DASHDASH_exp (_, e) -> (e, Binop.Minus, true)
      in
      let e, (curr_loc, intermediates) = expr curr_loc e in
      match e with
      | Expr.Var v as var ->
          let next_loc = Loc.fresh () in
          let update_edge =
            ( curr_loc,
              next_loc,
              Stmt.Assign { lhs = v; rhs = Expr.Binop { l = var; op; r = Expr.Lit (Lit.Int 1) } } )
          in
          if is_pre then (var, (next_loc, update_edge :: intermediates))
          else
            let inverse_op =
              match op with
              | Binop.Plus -> Binop.Minus
              | Binop.Minus -> Binop.Plus
              | _ -> failwith "unreachable"
            in
            ( Expr.Binop { l = var; op = inverse_op; r = Expr.Lit (Lit.Int 1) },
              (next_loc, update_edge :: intermediates) )
      | Expr.Deref _ | Expr.Array_access _ ->
          failwith "TODO: handle unary (pre/post)-(increment/decrement) on fields/array elements"
      | _ ->
          if is_pre then
            (Expr.Binop { l = e; op; r = Expr.Lit (Lit.Int 1) }, (curr_loc, intermediates))
          else (e, (curr_loc, intermediates)) )

and expr_as_var (curr_loc : Loc.t) (cst : CST.expression) : string * (Loc.t * edge list) =
  let open Dai.Ast in
  let e, (curr_loc, intermediates) = expr curr_loc cst in
  match e with
  | Expr.Var v -> (v, (curr_loc, intermediates))
  | e ->
      let tmp = fresh_tmp_var () in
      let next_loc = Loc.fresh () in
      (tmp, (next_loc, intermediates @ [ (curr_loc, next_loc, Stmt.Assign { lhs = tmp; rhs = e }) ]))

(** lift [expr] to non-empty lists of expressions, according to Java semantics: i.e. throwing away value of all but last *)
let rec expr_of_nonempty_list (curr_loc : Loc.t) :
    CST.expression list -> Dai.Ast.Expr.t * (Loc.t * edge list) = function
  | [] -> failwith "only non-empty lists of expressions supported"
  | [ e ] -> expr curr_loc e
  | e :: es ->
      let _, (intermediate_loc, intermediate_stmts) = expr curr_loc e in
      expr_of_nonempty_list intermediate_loc es |> fun (e, (l, stmts)) ->
      (e, (l, intermediate_stmts @ stmts))

let rec locals_declared_in_block : CST.block -> string list = function
  | _leftcurly, stmts, _rightcurly ->
      let name_of_var_declarator : CST.variable_declarator -> string = function
        | (`Id (_, name), _), _initializer -> name
        | _ -> failwith "todo: name_of_var_declarator"
      in
      let rec local_decls_of_stmt : CST.statement -> string list = function
        | `Assert_stmt _ -> []
        | `Blk b -> locals_declared_in_block b
        | `Brk_stmt _ -> []
        | `Cont_stmt _ -> []
        | `Decl _ -> []
        | `Do_stmt (_, s, _, _, _) -> local_decls_of_stmt s
        | `Enha_for_stmt (_, _, _, _, binding, _, _, _, s) -> (
            match binding with
            | `Id (_, v), _ -> v :: local_decls_of_stmt s
            | _ -> local_decls_of_stmt s )
        | `Exp_stmt _ -> []
        | `For_stmt (_, _, binding, _, _, _, _, s) -> (
            match binding with
            | `Local_var_decl (_, _, _, _) as l -> local_decls_of_stmt l @ local_decls_of_stmt s
            | _ -> local_decls_of_stmt s )
        | `If_stmt (_, _, t_stmt, f_branch) -> (
            match f_branch with
            | None -> local_decls_of_stmt t_stmt
            | Some (_, f_stmt) -> local_decls_of_stmt t_stmt @ local_decls_of_stmt f_stmt )
        | `Labe_stmt (_label, _, s) -> local_decls_of_stmt s
        | `Local_var_decl (_, _, (v, vs), _) ->
            name_of_var_declarator v :: List.map vs ~f:(snd >> name_of_var_declarator)
        | `Ret_stmt _ -> []
        | `SEMI _ -> []
        | `Switch_exp _ -> []
        | `Sync_stmt (_, _, b) -> locals_declared_in_block b
        | `Throw_stmt _ -> []
        | `Try_stmt (_, b, _) -> locals_declared_in_block b
        | `Try_with_resous_stmt _ -> []
        | `While_stmt (_, _, s) -> local_decls_of_stmt s
        | `Yield_stmt _ -> []
      in
      List.bind stmts ~f:local_decls_of_stmt

let rec edge_list_of_block name loc_map ~entry ~exit ~ret : CST.block -> Loc_map.t * edge list =
  let open Dai.Ast in
  function
  | _leftcurly, stmts, _rightcurly ->
      let rec edge_list_of_stmt loc_map entry exit ret stmt : Loc_map.t * edge list =
        let loc_map = Loc_map.add name stmt { entry; exit; ret } loc_map in
        match stmt with
        | `Assert_stmt assertion ->
            let expr, (entry, intermediate_stmts) =
              match assertion with
              | `Assert_exp_SEMI (_, e, _) | `Assert_exp_COLON_exp_SEMI (_, e, _, _, _) ->
                  expr entry e
            in
            (loc_map, (entry, exit, Stmt.Assume expr) :: intermediate_stmts)
        | `Blk b -> edge_list_of_block name loc_map ~entry ~exit ~ret b
        | `Brk_stmt _ -> failwith "todo: Brk_stmt"
        | `Cont_stmt _ -> failwith "todo: Cont_stmt"
        | `Decl _ -> failwith "todo: Decl"
        | `Do_stmt (_, body, _, (_, cond, _), _) ->
            let body_exit = Loc.fresh () in
            let cond, (body_entry, cond_intermediate_stmts) = expr entry cond in
            let cond_neg = Expr.Unop { op = Unop.Not; e = cond } in
            let loc_map, body = edge_list_of_stmt loc_map body_entry body_exit ret body in
            (body_exit, entry, Stmt.Assume cond) :: (body_exit, exit, Stmt.Assume cond_neg) :: body
            |> List.append cond_intermediate_stmts
            |> pair loc_map
        | `Enha_for_stmt _ -> failwith "todo: Enha_for_stmt"
        | `Exp_stmt (e, _) ->
            let _value_of_e, (intermediate_loc, intermediate_stmts) = expr entry e in
            (loc_map, (intermediate_loc, exit, Stmt.Skip) :: intermediate_stmts)
        | `For_stmt (_, _, init, cond, _, update, _, body) ->
            let body_entry = Loc.fresh () in
            let body_exit = Loc.fresh () in
            let cond, (cond_intermediate_loc, cond_intermediate_stmts) =
              match cond with
              | None -> (Expr.Lit (Lit.Bool true), (entry, []))
              | Some cond -> expr entry cond
            in
            let cond_neg = Expr.Unop { op = Unop.Not; e = cond } in
            let update_intermediate_loc, update_intermediate_stmts =
              match update with
              | None -> (body_exit, [])
              | Some (e, es) ->
                  let exprs = e :: List.map ~f:snd es in
                  expr_of_nonempty_list body_exit exprs |> snd
              (* discard value of update expression, as in Java semantics *)
            in
            let loc_map, (init_intermediate_loc, init_intermediate_stmts) =
              match init with
              | `Local_var_decl _ as decl ->
                  let l = Loc.fresh () in
                  let loc_map, es = edge_list_of_stmt loc_map entry l ret decl in
                  (loc_map, (l, es))
              | `Opt_exp_rep_COMMA_exp_SEMI (None, _) -> (loc_map, (body_exit, []))
              | `Opt_exp_rep_COMMA_exp_SEMI (Some (e, es), _) ->
                  let exprs = e :: List.map ~f:snd es in
                  (loc_map, expr_of_nonempty_list entry exprs |> snd)
              (* discard value of initializer expression, as in Java semantics *)
            in
            let loc_map, body = edge_list_of_stmt loc_map body_entry body_exit ret body in
            (cond_intermediate_loc, body_entry, Stmt.Assume cond)
            :: (cond_intermediate_loc, exit, Stmt.Assume cond_neg)
            :: (update_intermediate_loc, init_intermediate_loc, Stmt.Skip)
            :: body
            |> List.append
                 (init_intermediate_stmts @ update_intermediate_stmts @ cond_intermediate_stmts)
            |> pair loc_map
        | `If_stmt (_, (_, cond, _), t_branch, f_branch_opt) ->
            let t_branch_entry = Loc.fresh () in
            let cond, (entry, cond_intermediate_stmts) = expr entry cond in
            let cond_neg = Expr.Unop { op = Unop.Not; e = cond } in
            let loc_map, t_branch =
              edge_list_of_stmt loc_map t_branch_entry exit ret t_branch |> fun (loc_map, edges) ->
              (loc_map, (entry, t_branch_entry, Stmt.Assume cond) :: edges)
            in
            let loc_map, f_branch =
              match f_branch_opt with
              | None -> (loc_map, [ (entry, exit, Stmt.Assume cond_neg) ])
              | Some (_, f_branch) ->
                  let f_branch_entry = Loc.fresh () in
                  edge_list_of_stmt loc_map f_branch_entry exit ret f_branch
                  |> fun (loc_map, edges) ->
                  (loc_map, (entry, f_branch_entry, Stmt.Assume cond_neg) :: edges)
            in
            cond_intermediate_stmts @ t_branch @ f_branch |> pair loc_map
        | `Labe_stmt (_label, _, s) -> edge_list_of_stmt loc_map entry exit ret s
        | `Local_var_decl (_, _, (v, vs), _) ->
            let decls = v :: List.map ~f:snd vs in
            (* Loc.t * edge list component of return is for intermediate stmts as described in [expr] documentation above *)
            let stmt_of_decl curr_loc : CST.variable_declarator -> Stmt.t * (Loc.t * edge list) =
              function
              | _, None -> (Stmt.Skip, (curr_loc, []))
              | (`Id (_, lhs), None), Some (_, `Exp e) ->
                  let rhs, intermediates = expr curr_loc e in
                  (Stmt.Assign { lhs; rhs }, intermediates)
              | (`Id _, _), _ -> failwith "arrays not yet implemented"
              | (`Choice_open _, _), _ -> failwith "modules not yet implemented"
            in
            let rec edges_of_decls curr_loc = function
              | [] -> failwith "unreachable"
              | [ d ] ->
                  let stmt, (curr_loc, intermediates) = stmt_of_decl curr_loc d in
                  (curr_loc, exit, stmt) :: intermediates
              | d :: ds ->
                  let next_loc = Loc.fresh () in
                  let stmt, (curr_loc, intermediates) = stmt_of_decl curr_loc d in
                  ((curr_loc, next_loc, stmt) :: intermediates) @ edges_of_decls next_loc ds
            in
            edges_of_decls entry decls |> pair loc_map
        | `Ret_stmt (_, e, _) ->
            let stmt, (entry, intermediates) =
              match e with
              | None -> (Stmt.Skip, (entry, []))
              | Some e ->
                  let rhs, (entry, intermediates) = expr entry e in
                  (Stmt.Assign { lhs = Dai.Cfg.retvar; rhs }, (entry, intermediates))
            in
            (entry, ret, stmt) :: intermediates |> pair loc_map
        | `SEMI _ -> (loc_map, [ (entry, exit, Stmt.skip) ])
        | `Switch_exp _ -> failwith "todo: Switch_exp"
        | `Sync_stmt _ -> failwith "todo: Sync_stmt"
        | `Throw_stmt _ -> failwith "todo: Throw_stmt"
        | `Try_stmt _ -> failwith "todo: Try_stmt"
        | `Try_with_resous_stmt _ -> failwith "todo: Try_with_resous_stmt"
        | `While_stmt (_, (_, cond, _), body) ->
            let body_entry = Loc.fresh () in
            let cond, (intermediate_loc, cond_intermediates) = expr entry cond in
            let cond_neg = Expr.Unop { op = Unop.Not; e = cond } in
            let loc_map, body = edge_list_of_stmt loc_map body_entry entry ret body in
            (intermediate_loc, body_entry, Stmt.Assume cond)
            :: (intermediate_loc, exit, Stmt.Assume cond_neg)
            :: body
            |> List.append cond_intermediates |> pair loc_map
        | `Yield_stmt _ -> failwith "todo: Yield_stmt"
      in
      let rec edges_of_stmts loc_map curr_loc = function
        | [] -> (loc_map, [ (curr_loc, exit, Stmt.Skip) ])
        | [ s ] -> edge_list_of_stmt loc_map curr_loc exit ret s
        | s :: ss ->
            let next_loc = Loc.fresh () in
            let loc_map, s_edges = edge_list_of_stmt loc_map curr_loc next_loc ret s in
            let loc_map, ss_edges = edges_of_stmts loc_map next_loc ss in
            (loc_map, s_edges @ ss_edges)
      in
      edges_of_stmts loc_map entry stmts

let parse_class_decl loc_map : CST.class_declaration -> Loc_map.t * (edge list * Dai.Cfg.Fn.t) list
    = function
  | _modifiers, _, class_name, _type_params, _superclass, _superinterfaces, (_, decls, _) ->
      List.fold decls ~init:(loc_map, []) ~f:(fun (loc_map, acc) -> function
        | `Meth_decl
            (_modifiers, (_tparams, _type, (`Id method_name, formals, _), _throws), `Blk block) ->
            let name = snd class_name ^ "#" ^ snd method_name in
            let formals =
              match formals with
              | _open_paren, _rcvr_param, Some (first, rest), _close_paren ->
                  let formal_name = function
                    | `Formal_param (_mods, _type, (`Id (_, id), _dims)) -> id
                    | _ -> failwith "spread params not yet handled"
                  in
                  List.fold rest ~init:[ formal_name first ] ~f:(fun acc curr ->
                      (snd curr |> formal_name) :: acc)
              | _, _, None, _ -> []
            in
            let locals = locals_declared_in_block block in
            let entry = Loc.fresh () in
            let exit = Loc.fresh () in
            let fn : Dai.Cfg.Fn.t = { name; formals; locals; entry; exit } in
            let loc_map, edges = edge_list_of_block name loc_map ~entry ~exit ~ret:exit block in
            (loc_map, (edges, fn) :: acc)
        | d ->
            failwith
              (Format.asprintf "unrecognized class body declaration: %a" Sexp.pp
                 (CST.sexp_of_class_body_declaration d)))

let of_java_cst (cst : Tree.java_cst) =
  let loc_map = Loc_map.empty in
  List.fold cst ~init:(loc_map, []) ~f:(fun (loc_map, acc) -> function
    | `Decl (`Class_decl cd) ->
        parse_class_decl loc_map cd |> fun (loc_map, cfgs) -> (loc_map, cfgs @ acc)
    | `Decl (`Import_decl _) -> (loc_map, acc)
    | stmt ->
        let rec first_atom = function
          | Sexp.Atom a -> a
          | List [] -> "empty-sexp-list"
          | Sexp.List (l :: _) -> first_atom l
        in
        failwith
          (Format.asprintf "unrecognized top-level definition: %s"
             (first_atom (CST.sexp_of_statement stmt))))
  |> fun (loc_map, cfgs) ->
  ( loc_map,
    List.fold cfgs ~init:(Dai.Cfg.empty ()) ~f:(fun cfg (edges, fn) ->
        Dai.Cfg.add_fn ~fn ~edges cfg) )

open Result.Monad_infix

let%test "hello world program" =
  let file = Src_file.of_file ~abspath:false "test_cases/java/HelloWorld.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst |> Result.is_ok
