open Dai.Import
open Tree_sitter_java
open Syntax

type edge = Cfg.Loc.t * Cfg.Loc.t * Ast.Stmt.t

type prgm_parse_result = {
  loc_map : Loc_map.t;
  cfgs : Cfg.t Cfg.Fn.Map.t;
  fields : Declared_fields.t;
  cha : Class_hierarchy.t;
}

let empty_parse_result =
  {
    loc_map = Loc_map.empty;
    cfgs = Cfg.Fn.Map.empty;
    fields = Declared_fields.empty;
    cha = Class_hierarchy.empty;
  }

let tmp_var_counter = ref 0

let diagnostic_mode = ref false

let unimplemented_syntax : int String.Map.t ref = ref String.Map.empty

(* when in [diagnostic_mode], accumulate a count of appearances of unimplemented syntactic forms *)

let unimplemented syntax default_val =
  if !diagnostic_mode then (
    let old_count = Map.find !unimplemented_syntax syntax |> Option.value ~default:0 in
    unimplemented_syntax := Map.set !unimplemented_syntax ~key:syntax ~data:(old_count + 1);
    default_val )
  else failwith ("TODO: unimplemented syntactic form " ^ syntax)

let print_diagnostic_results () =
  Format.(fprintf std_formatter) "DIAGNOSTIC MODE\n";
  Format.(fprintf std_formatter)
    "NB: sub-trees of unimplemented syntax are not explored, so some things may be missed if they \
     are nested within some other unimplemented syntax.\n";
  Format.(fprintf std_formatter) "Unimplemented syntax encountered by Cfg_parser:\n";
  Map.to_alist !unimplemented_syntax
  |> List.sort ~compare:(fun (_, count1) (_, count2) -> Int.compare count2 count1)
  |> List.iter ~f:(uncurry @@ Format.(fprintf std_formatter) "%s (%i occurrences)\n")

let fresh_tmp_var () =
  let v = "__dai_tmp" ^ Int.to_string !tmp_var_counter in
  tmp_var_counter := !tmp_var_counter + 1;
  v

let string_of_simple_type = function
  | `Void_type _ -> "void"
  | `Inte_type (`Byte _) -> "byte"
  | `Inte_type (`Short _) -> "short"
  | `Inte_type (`Int _) -> "int"
  | `Inte_type (`Long _) -> "long"
  | `Inte_type (`Char _) -> "char"
  | `Floa_point_type (`Float _) -> "float"
  | `Floa_point_type (`Double _) -> "double"
  | `Bool_type _ -> "boolean"
  | `Id (_, ident) -> ident
  | `Scoped_type_id _ -> unimplemented "`Scoped_type_id" "scoped_type_id"
  | `Gene_type _ -> unimplemented "`Gene_type" "gene_type"

let rec string_of_unannotated_type = function
  | `Choice_void_type st -> string_of_simple_type st
  | `Array_type (ut, _) -> string_of_unannotated_type ut ^ "[]"

(** Convert an expression concrete syntax tree to an expression in our IR, along with potentially some preceding statements for any function invocations and assignments therein, and a shifted current program location to accomodate those intermediate statements.
    That is, 
      * if `cst` represents a simple expression with no function invocations or assignments, return value is (<that expression in our IR>, (curr_loc,[]))
      * if `cst` contains function invocations f_1 ... f_k and assignments x_1=e_1 ... x_n=e_n, return value is 
          (
             <that expression in our IR>[ tmp_var_i / f_i][x_i / x_i=e_i],
             Some (fresh_loc_k+n, [curr_loc -[tmp_var_1 := f_1]-> fresh_loc_1, ... , fresh_loc_(k-1) -[tmp_var_k := f_k]-> fresh_loc_k] ++ [fresh_loc_k -[x_1=e_1]-> fresh_loc_k+1, ... , fresh_loc_(k+n-1) -[x_n=e_n]-> fresh_loc_(k+n)])

    Optional [exit_loc] param is used to special-case the common statement syntax of [`Exp_stmt (`Assign_exp _)] and avoid generating extraneous locations and [Skip] edges
*)
let rec expr ?exit_loc ~(curr_loc : Cfg.Loc.t) ~(exc : Cfg.Loc.t) (cst : CST.expression) :
    Ast.Expr.t * (Cfg.Loc.t * edge list) =
  let open Ast in
  let placeholder_expr = (Expr.Lit (Lit.String "DIAGNOSTIC MODE PLACEHOLDER"), (curr_loc, [])) in
  match cst with
  | `Assign_exp (lhs, op, rhs) ->
      let rhs_expr, (curr_loc, rhs_intermediates) = expr ~curr_loc ~exc rhs in
      let lhs_expr, (curr_loc, lhs_intermediates) =
        match lhs with
        | `Id (_, ident) -> (Expr.Var ident, (curr_loc, []))
        | `Choice_open _ -> unimplemented "`Choice_open" placeholder_expr
        | `Field_access (rcvr, _, _, field) ->
            let rcvr, aux_info =
              match rcvr with
              | `Super _ -> ("super", (curr_loc, []))
              | `Prim_exp _ as e -> expr_as_var ~curr_loc ~exc e
            in
            let field =
              match field with
              | `Id (_, fld) -> fld
              | `Choice_open _ -> unimplemented "`Choice_open" "DIAGNOSTIC MODE PLACEHOLDER"
              | `This _ -> "this"
            in
            (Expr.Deref { rcvr; field }, aux_info)
        | `Array_access (rcvr, _, idx, _) ->
            let idx, (curr_loc, idx_intermediates) = expr ~curr_loc ~exc idx in
            let rcvr, (curr_loc, rcvr_intermediates) =
              expr_as_var ~curr_loc ~exc (`Prim_exp rcvr)
            in
            ( Expr.Array_access { rcvr = Expr.Var rcvr; idx },
              (curr_loc, idx_intermediates @ rcvr_intermediates) )
      in
      let rhs_expr_with_op =
        match op with
        | `EQ _ -> rhs_expr
        | `PLUSEQ _ -> Expr.binop lhs_expr Binop.Plus rhs_expr
        | `DASHEQ _ -> Expr.binop lhs_expr Binop.Minus rhs_expr
        | `STAREQ _ -> Expr.binop lhs_expr Binop.Times rhs_expr
        | `SLASHEQ _ -> Expr.binop lhs_expr Binop.Divided_by rhs_expr
        | `AMPEQ _ -> unimplemented "`AMPEQ" (fst placeholder_expr)
        | `BAREQ _ -> unimplemented "`BAREQ" (fst placeholder_expr)
        | `HATEQ _ -> unimplemented "`HATEQ" (fst placeholder_expr)
        | `PERCEQ _ -> unimplemented "`PERCEQ" (fst placeholder_expr)
        | `LTLTEQ _ -> unimplemented "`LTLTEQ" (fst placeholder_expr)
        | `GTGTEQ _ -> unimplemented "`GTGTEQ" (fst placeholder_expr)
        | `GTGTGTEQ _ -> unimplemented "`GTGTGTEQ" (fst placeholder_expr)
      in
      let stmt =
        match lhs with
        | `Id (_, lhs) -> Stmt.Assign { lhs; rhs = rhs_expr_with_op }
        | `Choice_open _ -> unimplemented "`Choice_open" Stmt.Skip
        | `Field_access _ ->
            let rcvr, field =
              match lhs_expr with
              | Expr.Deref { rcvr; field } -> (rcvr, field)
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
      let next_loc = Option.value exit_loc ~default:(Cfg.Loc.fresh ()) in
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
        | `Exp_AMP_exp (e1, _, e2) -> unimplemented "`Exp_AMP_exp" (e1, Binop.Eq, e2)
        | `Exp_BAR_exp (e1, _, e2) -> unimplemented "`Exp_BAR_exp" (e1, Binop.Eq, e2)
        | `Exp_HAT_exp (e1, _, e2) -> unimplemented "`Exp_HAT_exp" (e1, Binop.Eq, e2)
        | `Exp_PERC_exp (e1, _, e2) -> (e1, Binop.Mod, e2)
        | `Exp_LTLT_exp (e1, _, e2) -> unimplemented "`Exp_LTLT_exp" (e1, Binop.Eq, e2)
        | `Exp_GTGT_exp (e1, _, e2) -> unimplemented "`Exp_GTGT_exp" (e1, Binop.Eq, e2)
        | `Exp_GTGTGT_exp (e1, _, e2) -> unimplemented "`Exp_GTGTGT_exp" (e1, Binop.Eq, e2)
      in
      let l, (curr_loc, l_intermediates) = expr ~curr_loc ~exc l in
      let r, (curr_loc, r_intermediates) = expr ~curr_loc ~exc r in
      (Expr.Binop { l; op; r }, (curr_loc, l_intermediates @ r_intermediates))
  | `Cast_exp (_, _type, _, _, e) -> expr ~curr_loc ~exc e
  | `Inst_exp _ -> unimplemented "`Inst_exp" placeholder_expr
  | `Lambda_exp _ -> unimplemented "`Lambda_exp" placeholder_expr
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
  | `Prim_exp (`Class_lit _) -> unimplemented "`Class_lit" placeholder_expr
  | `Prim_exp (`This _) -> (Expr.Var "this", (curr_loc, []))
  | `Prim_exp (`Id (_, ident)) -> (Expr.Var ident, (curr_loc, []))
  | `Prim_exp (`Choice_open _) -> unimplemented "`Choice_open" placeholder_expr
  | `Prim_exp (`Paren_exp (_, e, _)) -> expr ?exit_loc ~curr_loc ~exc e
  | `Prim_exp (`Obj_crea_exp (`Unqu_obj_crea_exp unqualified))
  | `Prim_exp (`Obj_crea_exp (`Prim_exp_DOT_unqu_obj_crea_exp (_, _, unqualified))) ->
      (* todo: deal with fully-qualified constructors *)
      let _, _targs, typ, (_, args, _), _initializer = unqualified in
      let ctor_name =
        match typ with
        | `Id (_, typ) | `Gene_type (`Id (_, typ), _) -> typ
        | _ ->
            failwith
              (Format.asprintf "unrecognized constructor simple-type: %a" Sexp.pp
                 (CST.sexp_of_simple_type typ))
      in
      let args = match args with Some (e, es) -> e :: List.map ~f:snd es | None -> [] in
      let actuals, (curr_loc, arg_intermediates) =
        List.fold args
          ~init:([], (curr_loc, []))
          ~f:(fun (acc_exprs, (curr_loc, acc_intermediates)) arg ->
            let arg_expr, (curr_loc, arg_intermediates) = expr ~curr_loc ~exc arg in
            (acc_exprs @ [ arg_expr ], (curr_loc, acc_intermediates @ arg_intermediates)))
      in
      let lhs = fresh_tmp_var () in
      let next_loc = Option.value exit_loc ~default:(Cfg.Loc.fresh ()) in
      let edges =
        ( curr_loc,
          next_loc,
          Stmt.Call
            {
              lhs;
              rcvr = ctor_name;
              meth = "<init>";
              actuals;
              alloc_site = Some (Alloc_site.fresh ());
            } )
        :: (curr_loc, exc, Stmt.Exceptional_call { rcvr = ctor_name; meth = "<init>"; actuals })
        :: arg_intermediates
      in
      (Expr.Var lhs, (next_loc, edges))
  | `Prim_exp (`Field_access (rcvr, _, _, field)) ->
      let rcvr, aux_info =
        match rcvr with
        | `Super _ -> ("super", (curr_loc, []))
        | `Prim_exp _ as e -> expr_as_var ~curr_loc ~exc e
      in
      let field =
        match field with
        | `Id (_, fld) -> fld
        | `Choice_open _ -> unimplemented "`Choice_open" "DIAGNOSTIC MODE PLACEHOLDER"
        | `This _ -> "this"
      in
      (Expr.Deref { rcvr; field }, aux_info)
  | `Prim_exp (`Array_access (rcvr, _, idx, _)) ->
      let idx, (curr_loc, idx_intermediates) = expr ~curr_loc ~exc idx in
      let rcvr, (curr_loc, rcvr_intermediates) = expr_as_var ~curr_loc ~exc (`Prim_exp rcvr) in
      ( Expr.Array_access { rcvr = Expr.Var rcvr; idx },
        (curr_loc, idx_intermediates @ rcvr_intermediates) )
  | `Prim_exp (`Meth_invo (rcvr_and_meth, (_, args, _))) ->
      let rcvr, meth, (curr_loc, rcvr_intermediates) =
        match rcvr_and_meth with
        | `Choice_id (`Id (_, meth)) -> ("this", meth, (curr_loc, []))
        | `Choice_id (`Choice_open _) ->
            unimplemented "`Choice_open" ("PLACEHOLDER", "PLACEHOLDER", (curr_loc, []))
        | `Choice_prim_exp_DOT_opt_super_DOT_opt_type_args_choice_id
            (rcvr, _dot, super, _typeargs, meth) -> (
            let rcvr, aux_info =
              match rcvr with
              | `Prim_exp _ as e when Option.is_none super -> expr_as_var ~curr_loc ~exc e
              | _ -> unimplemented "`Choice_prim_exp_DOT_super" ("PLACEHOLDER", (curr_loc, []))
            in
            match meth with
            | `Id (_, meth_name) -> (rcvr, meth_name, aux_info)
            | `Choice_open _ ->
                unimplemented "`Choice_open" ("PLACEHOLDER", "PLACEHOLDER", (curr_loc, [])) )
      in
      let args = match args with Some (e, es) -> e :: List.map ~f:snd es | None -> [] in
      let actuals, (curr_loc, arg_intermediates) =
        List.fold args
          ~init:([], (curr_loc, []))
          ~f:(fun (acc_exprs, (curr_loc, acc_intermediates)) arg ->
            let arg_expr, (curr_loc, arg_intermediates) = expr ~curr_loc ~exc arg in
            (acc_exprs @ [ arg_expr ], (curr_loc, acc_intermediates @ arg_intermediates)))
      in
      let lhs = fresh_tmp_var () in
      let next_loc = Option.value exit_loc ~default:(Cfg.Loc.fresh ()) in
      let call = (curr_loc, next_loc, Stmt.Call { lhs; rcvr; meth; actuals; alloc_site = None }) in
      let exc_call = (curr_loc, exc, Stmt.Exceptional_call { rcvr; meth; actuals }) in
      (Expr.Var lhs, (next_loc, (call :: exc_call :: rcvr_intermediates) @ arg_intermediates))
  | `Prim_exp (`Meth_ref _) -> unimplemented "`Meth_ref" placeholder_expr
  | `Prim_exp (`Array_crea_exp (_new, simple_type, initial_array)) -> (
      match initial_array with
      | `Dimens_array_init (_, ai) -> array_lit ~curr_loc ~exc ai
      | `Rep1_dimens_expr_opt_dimens (dim_exprs, _) ->
          let elt_type = string_of_simple_type simple_type in
          let alloc_site = Alloc_site.fresh () in
          let _, _, outermost_dim_expr, _ = List.hd_exn dim_exprs in
          let outermost_dim_size, (curr_loc, intermediate_stmts) =
            expr ~curr_loc ~exc outermost_dim_expr
          in
          let e = Ast.Expr.Array_create { elt_type; size = outermost_dim_size; alloc_site } in
          (e, (curr_loc, intermediate_stmts)) )
  | `Switch_exp _ -> unimplemented "`Switch_exp" placeholder_expr
  | `Tern_exp (if_exp, _, then_exp, _, else_exp) ->
      let tmp = fresh_tmp_var () in
      let if_exp, (curr_loc, cond_intermediates) = expr ~curr_loc ~exc if_exp in
      let then_branch_head = Cfg.Loc.fresh () in
      let else_branch_head = Cfg.Loc.fresh () in
      let next_loc = Option.value exit_loc ~default:(Cfg.Loc.fresh ()) in
      let then_exp, (then_branch_tail, then_intermediates) =
        expr ~curr_loc:then_branch_head ~exc then_exp
      in
      let else_exp, (else_branch_tail, else_intermediates) =
        expr ~curr_loc:else_branch_head ~exc else_exp
      in
      let stmts =
        (curr_loc, then_branch_head, Stmt.Assume if_exp)
        :: (curr_loc, else_branch_head, Stmt.Assume (Expr.unop Unop.Not if_exp))
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
      let e, (curr_loc, intermediates) = expr ~curr_loc ~exc e in
      (Expr.unop op e, (curr_loc, intermediates))
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
      let e, (curr_loc, intermediates) = expr ~curr_loc ~exc e in
      match e with
      | Expr.Var v as var ->
          let next_loc = Option.value exit_loc ~default:(Cfg.Loc.fresh ()) in
          let update_edge =
            ( curr_loc,
              next_loc,
              Stmt.Assign { lhs = v; rhs = Expr.binop var op (Expr.Lit (Lit.Int 1)) } )
          in
          if is_pre then (var, (next_loc, update_edge :: intermediates))
          else
            let inverse_op =
              match op with
              | Binop.Plus -> Binop.Minus
              | Binop.Minus -> Binop.Plus
              | _ -> failwith "unreachable"
            in
            ( Expr.binop var inverse_op (Expr.Lit (Lit.Int 1)),
              (next_loc, update_edge :: intermediates) )
      | Expr.Deref _ | Expr.Array_access _ ->
          unimplemented "`Unary_increment_or_decrement_on_heap" placeholder_expr
      | _ ->
          if is_pre then (Expr.binop e op (Expr.Lit (Lit.Int 1)), (curr_loc, intermediates))
          else (e, (curr_loc, intermediates)) )

and expr_as_var ~(curr_loc : Cfg.Loc.t) ~(exc : Cfg.Loc.t) (cst : CST.expression) :
    string * (Cfg.Loc.t * edge list) =
  let open Ast in
  let e, (curr_loc, intermediates) = expr ~curr_loc ~exc cst in
  match e with
  | Expr.Var v -> (v, (curr_loc, intermediates))
  | e ->
      let tmp = fresh_tmp_var () in
      let next_loc = Cfg.Loc.fresh () in
      (tmp, (next_loc, intermediates @ [ (curr_loc, next_loc, Stmt.Assign { lhs = tmp; rhs = e }) ]))

and array_lit ~(curr_loc : Cfg.Loc.t) ~(exc : Cfg.Loc.t) (lit : CST.array_initializer) :
    Ast.Expr.t * (Cfg.Loc.t * edge list) =
  let expr_of_var_initializer curr_loc (vi : CST.variable_initializer) =
    match vi with `Exp e -> expr ~curr_loc ~exc e | `Array_init ai -> array_lit ~curr_loc ~exc ai
  in
  match lit with
  | _, Some (first, rest), _, _ ->
      let first_elt, aux = expr_of_var_initializer curr_loc first in
      let elts, (curr_loc, intermediates) =
        List.fold rest
          ~init:([ first_elt ], aux)
          ~f:(fun (elts, (curr_loc, intermediate_stmts)) curr ->
            let elt, (curr_loc, curr_intermediates) = expr_of_var_initializer curr_loc (snd curr) in
            (elt :: elts, (curr_loc, curr_intermediates @ intermediate_stmts)))
      in
      (Ast.Expr.Array_literal { elts; alloc_site = Alloc_site.fresh () }, (curr_loc, intermediates))
  | _, None, _, _ ->
      (Ast.Expr.Array_literal { elts = []; alloc_site = Alloc_site.fresh () }, (curr_loc, []))

(** lift [expr] to non-empty lists of expressions, according to Java semantics: i.e. throwing away value of all but last *)
let rec expr_of_nonempty_list ~(curr_loc : Cfg.Loc.t) ~(exc : Cfg.Loc.t) :
    CST.expression list -> Ast.Expr.t * (Cfg.Loc.t * edge list) = function
  | [] -> failwith "only non-empty lists of expressions supported"
  | [ e ] -> expr ~curr_loc ~exc e
  | e :: es ->
      let _, (intermediate_loc, intermediate_stmts) = expr ~curr_loc ~exc e in
      expr_of_nonempty_list ~curr_loc:intermediate_loc ~exc es |> fun (e, (l, stmts)) ->
      (e, (l, intermediate_stmts @ stmts))

let ident_of_var_declarator : CST.variable_declarator -> string = function
  | (`Id (_, ident), _), _initializer -> ident
  | _ -> unimplemented "variable_declarator" "DIAGNOSTIC MODE PLACEHOLDER"

let rec declarations stmts : string list =
  let rec local_decls_of_stmt : CST.statement -> string list = function
    | `Assert_stmt _ -> []
    | `Blk (_, stmts, _) -> declarations stmts
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
        ident_of_var_declarator v :: List.map vs ~f:(snd >> ident_of_var_declarator)
    | `Ret_stmt _ -> []
    | `SEMI _ -> []
    | `Switch_exp _ -> []
    | `Sync_stmt (_, _, (_, stmts, _)) -> declarations stmts
    | `Throw_stmt _ -> []
    | `Try_stmt (_, (_, stmts, _), _) -> declarations stmts
    | `Try_with_resous_stmt _ -> []
    | `While_stmt (_, _, s) -> local_decls_of_stmt s
    | `Yield_stmt _ -> []
  in
  List.bind stmts ~f:local_decls_of_stmt

let rec edge_list_of_stmt method_id loc_map entry exit ret exc stmt : Loc_map.t * edge list =
  let open Ast in
  let loc_map =
    match Loc_map.add method_id stmt { entry; exit; ret; exc } loc_map with
    | `Ok lm -> lm
    | `Collision ->
        (* handle this case if it comes up in evaluation.  Perhaps by enumerating the occurrences topologically to disambiguate, storing a sequence of loc_ctx's for the statement and scanning parse trees for the order as needed?*)
        Format.(fprintf std_formatter)
          "WARNING: two syntactically-identical statements in method %s.%s; this is problematic \
           only if there's an edit there.\n"
          method_id.class_name method_id.method_name;
        loc_map
  in
  match stmt with
  | `Assert_stmt assertion ->
      let expr, (entry, intermediate_stmts) =
        match assertion with
        | `Assert_exp_SEMI (_, e, _) | `Assert_exp_COLON_exp_SEMI (_, e, _, _, _) ->
            expr ~curr_loc:entry ~exc e
      in
      (loc_map, (entry, exit, Stmt.Assume expr) :: intermediate_stmts)
  | `Blk (_, stmts, _) -> edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret ~exc stmts
  | `Brk_stmt _ -> unimplemented "`Brk_stmt" (loc_map, [])
  | `Cont_stmt _ -> unimplemented "`Cont_stmt" (loc_map, [])
  | `Decl _ -> unimplemented "`Decl" (loc_map, [])
  | `Do_stmt (_, body, _, (_, cond, _), _) ->
      let body_exit = Cfg.Loc.fresh () in
      let cond, (cond_exit, cond_intermediate_stmts) = expr ~curr_loc:body_exit ~exc cond in
      let cond_neg = Expr.unop Unop.Not cond in
      let loc_map, body = edge_list_of_stmt method_id loc_map entry body_exit ret exc body in
      (cond_exit, entry, Stmt.Assume cond) :: (cond_exit, exit, Stmt.Assume cond_neg) :: body
      |> List.append cond_intermediate_stmts
      |> pair loc_map
  | `Enha_for_stmt _ -> unimplemented "`Enha_for_stmt" (loc_map, [])
  | `Exp_stmt (e, _) ->
      let _value_of_e, (intermediate_loc, intermediate_stmts) =
        expr ~exit_loc:exit ~curr_loc:entry ~exc e
      in
      if Cfg.Loc.equal intermediate_loc exit then (loc_map, intermediate_stmts)
      else (loc_map, (intermediate_loc, exit, Stmt.Skip) :: intermediate_stmts)
  | `For_stmt ((_, _, _, _, _, _, _, body) as f) ->
      let body_entry = Cfg.Loc.fresh () in
      let body_exit = Cfg.Loc.fresh () in
      let loc_map, header, _ =
        for_loop_header method_id ~body_entry ~body_exit ~entry ~exit ~ret ~exc loc_map f
      in
      let loc_map, body = edge_list_of_stmt method_id loc_map body_entry body_exit ret exc body in
      (loc_map, header @ body)
  | `If_stmt (_, (_, cond, _), t_branch, f_branch_opt) ->
      let t_branch_entry = Cfg.Loc.fresh () in
      let cond, (entry, cond_intermediate_stmts) = expr ~curr_loc:entry ~exc cond in
      let cond_neg = Expr.unop Unop.Not cond in
      let loc_map, t_branch =
        edge_list_of_stmt method_id loc_map t_branch_entry exit ret exc t_branch
        |> fun (loc_map, edges) -> (loc_map, (entry, t_branch_entry, Stmt.Assume cond) :: edges)
      in
      let loc_map, f_branch =
        match f_branch_opt with
        | None -> (loc_map, [ (entry, exit, Stmt.Assume cond_neg) ])
        | Some (_, f_branch) ->
            let f_branch_entry = Cfg.Loc.fresh () in
            edge_list_of_stmt method_id loc_map f_branch_entry exit ret exc f_branch
            |> fun (loc_map, edges) ->
            (loc_map, (entry, f_branch_entry, Stmt.Assume cond_neg) :: edges)
      in
      cond_intermediate_stmts @ t_branch @ f_branch |> pair loc_map
  | `Labe_stmt (_label, _, s) -> edge_list_of_stmt method_id loc_map entry exit ret exc s
  | `Local_var_decl (_, _, (v, vs), _) ->
      let decls = v :: List.map ~f:snd vs in
      (* Loc.t * edge list component of return is for intermediate stmts as described in [expr] documentation above *)
      let stmt_of_decl curr_loc : CST.variable_declarator -> Stmt.t * (Cfg.Loc.t * edge list) =
        function
        | _, None -> (Stmt.Skip, (curr_loc, []))
        | (`Id (_, lhs), _), Some (_, `Exp e) ->
            let rhs, intermediate_stmts = expr ~curr_loc ~exc e in
            (Stmt.Assign { lhs; rhs }, intermediate_stmts)
        | (`Id (_, lhs), _), Some (_, `Array_init lit) ->
            let rhs, intermediate_stmts = array_lit ~curr_loc ~exc lit in
            (Stmt.Assign { lhs; rhs }, intermediate_stmts)
        | (`Choice_open _, _), _ -> failwith "modules not yet implemented"
      in
      let rec edges_of_decls curr_loc = function
        | [] -> failwith "unreachable"
        | [ d ] ->
            let stmt, (curr_loc, intermediates) = stmt_of_decl curr_loc d in
            (curr_loc, exit, stmt) :: intermediates
        | d :: ds ->
            let next_loc = Cfg.Loc.fresh () in
            let stmt, (curr_loc, intermediates) = stmt_of_decl curr_loc d in
            ((curr_loc, next_loc, stmt) :: intermediates) @ edges_of_decls next_loc ds
      in
      edges_of_decls entry decls |> pair loc_map
  | `Ret_stmt (_, e, _) ->
      let stmt, (entry, intermediates) =
        match e with
        | None -> (Stmt.Skip, (entry, []))
        | Some e ->
            let rhs, (entry, intermediates) = expr ~curr_loc:entry ~exc e in
            (Stmt.Assign { lhs = Cfg.retvar; rhs }, (entry, intermediates))
      in
      (entry, ret, stmt) :: intermediates |> pair loc_map
  | `SEMI _ -> (loc_map, [ (entry, exit, Stmt.skip) ])
  | `Switch_exp _ -> unimplemented "`Switch_exp" (loc_map, [])
  | `Sync_stmt _ -> unimplemented "`Sync_stmt" (loc_map, [])
  | `Throw_stmt (_, e, _) ->
      let thrown_expr, (intermediate_loc, intermediate_stmts) = expr ~curr_loc:entry ~exc e in
      let throw_edge =
        (intermediate_loc, exc, Stmt.Assign { lhs = Cfg.exc_retvar; rhs = thrown_expr })
      in
      (loc_map, throw_edge :: intermediate_stmts)
  | `Try_stmt (_, (_, try_block, _), `Rep1_catch_clause catch_clauses) ->
      let loc_map, catch_loc, catch_edges =
        build_catch_cfg catch_clauses loc_map method_id ~exit ~ret ~exc
      in
      let loc_map, try_edges =
        edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret ~exc:catch_loc try_block
      in
      (loc_map, try_edges @ catch_edges)
  | `Try_stmt
      ( _,
        (_, try_block, _),
        `Rep_catch_clause_fina_clause (catch_clauses, (_, (_, finally_block, _))) ) ->
      let finally_entry_loc = Cfg.Loc.fresh () in
      let finally_exit_loc = Cfg.Loc.fresh () in
      let loc_map, catch_loc, catch_edges =
        build_catch_cfg catch_clauses loc_map method_id ~exit:finally_entry_loc ~ret
          ~exc:finally_entry_loc
      in
      let loc_map, try_edges =
        edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret ~exc:catch_loc try_block
      in
      let loc_map, finally_edges =
        edge_list_of_stmt_list method_id loc_map ~entry:finally_entry_loc ~exit:finally_exit_loc
          ~ret ~exc finally_block
      in
      ( loc_map,
        (* control flow can go to either normal or exceptional exit from finally block*)
        ((finally_exit_loc, exit, Stmt.Skip) :: (finally_exit_loc, exc, Stmt.Skip) :: try_edges)
        @ catch_edges @ finally_edges )
  | `Try_with_resous_stmt _ -> unimplemented "`Try_with_resous_stmt" (loc_map, [])
  | `While_stmt (_, (_, cond, _), body) ->
      let body_entry = Cfg.Loc.fresh () in
      let cond, (intermediate_loc, cond_intermediates) = expr ~curr_loc:entry ~exc cond in
      let cond_neg = Expr.unop Unop.Not cond in
      let loc_map, body = edge_list_of_stmt method_id loc_map body_entry entry ret exc body in
      (intermediate_loc, body_entry, Stmt.Assume cond)
      :: (intermediate_loc, exit, Stmt.Assume cond_neg)
      :: body
      |> List.append cond_intermediates |> pair loc_map
  | `Yield_stmt _ -> unimplemented "`Yield_stmt" (loc_map, [])

and build_catch_cfg catch_clauses loc_map method_id ~exit ~ret ~exc :
    Loc_map.t * Cfg.Loc.t * edge list =
  let open Ast in
  let condition_of_catch_clause = function
    | _, _, (_, (ct, cts), _), _, _ ->
        let caught_types =
          string_of_unannotated_type ct :: List.map cts ~f:(snd >> string_of_unannotated_type)
        in
        let exc_retvar_has_type t =
          Expr.binop
            (Expr.unop Unop.Typeof (Expr.Var Cfg.exc_retvar))
            Binop.Eq (Expr.Lit (Lit.String t))
        in
        List.map caught_types ~f:exc_retvar_has_type
        |> List.reduce_exn ~f:(flip Expr.binop Binop.Or)
  in
  let exn_binding_of_catch_clause : CST.catch_clause -> Stmt.t = function
    | _, _, (_, _, (`Id (_, ident), _)), _, _ ->
        Stmt.Assign { lhs = ident; rhs = Expr.Var Cfg.exc_retvar }
    | _ -> failwith "unimplemented case: catch_clause with non-identifier formal parameter"
  in
  let body_of_catch_clause : CST.catch_clause -> CST.program = function
    | _, _, _, _, (_lcurly, body, _rcurly) -> body
  in
  if List.is_empty catch_clauses then (loc_map, exc, [])
  else
    let catch_loc = Cfg.Loc.fresh () in
    let rec build_catch_cfg_impl curr_loc loc_map = function
      | [] ->
          (* only reachable when there are 0 catch_clauses, precluded by preceding [List.is_empty] check *)
          failwith "unreachable"
      | cc :: ccs ->
          let block_entry_loc = Cfg.Loc.fresh () in
          let cond = condition_of_catch_clause cc in
          let cond_is_match = Stmt.Assume cond in
          let cond_is_not_match = Stmt.Assume (Expr.unop Unop.Not cond) in
          let exn_binding_loc = Cfg.Loc.fresh () in
          let loc_map, block_edges =
            edge_list_of_stmt_list method_id loc_map (body_of_catch_clause cc)
              ~entry:block_entry_loc ~exit ~ret ~exc
          in
          let next_loc, (loc_map, ccs_edges) =
            match ccs with
            | [] ->
                (* this is the last catch clause, so wire the non-matching case out to the exceptional exit and make no recursive call *)
                (exc, (loc_map, []))
            | _ ->
                (* this is _not_ the last catch clause, so wire the non-matching case out to a new location and keep building catch blocks from there *)
                let next_loc = Cfg.Loc.fresh () in
                (next_loc, build_catch_cfg_impl next_loc loc_map ccs)
          in
          (* 3 explicit edges:
             1. from [curr_loc] into the catch-block, guarded by the specified exception types
             2. from [curr_loc] to the next catch clause (or the exceptional exit if this is the last catch clause), guarded by the negation of the specified exception types
             3. preceding the catch-block, binding the exception value to the formal parameter
             in addition to:
             [block_edges]: the body of this catch-block
             [ccs_edges]: the recursively-constructed CFG edges of all remaining catch clauses [ccs]
          *)
          (curr_loc, exn_binding_loc, cond_is_match)
          :: (curr_loc, next_loc, cond_is_not_match)
          :: (exn_binding_loc, block_entry_loc, exn_binding_of_catch_clause cc)
          :: (block_edges @ ccs_edges)
          |> pair loc_map
    in
    build_catch_cfg_impl catch_loc loc_map catch_clauses |> fun (loc_map, edges) ->
    (loc_map, catch_loc, edges)

and edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret ~exc stmts : Loc_map.t * edge list =
  let rec edges_of_stmts loc_map curr_loc = function
    | [] -> (loc_map, [ (curr_loc, exit, Ast.Stmt.Skip) ])
    | [ s ] -> edge_list_of_stmt method_id loc_map curr_loc exit ret exc s
    | s :: ss ->
        let next_loc = Cfg.Loc.fresh () in
        let loc_map, s_edges = edge_list_of_stmt method_id loc_map curr_loc next_loc ret exc s in
        let loc_map, ss_edges = edges_of_stmts loc_map next_loc ss in
        (loc_map, s_edges @ ss_edges)
  in
  edges_of_stmts loc_map entry stmts

and for_loop_header method_id ~body_entry ~body_exit ~entry ~exit ~ret ~exc loc_map :
    CST.for_statement -> Loc_map.t * edge list * edge = function
  | _, _, init, cond, _, update, _, _body ->
      let loc_map, (init_intermediate_loc, init_intermediate_stmts) =
        match init with
        | `Local_var_decl _ as decl ->
            let l = Cfg.Loc.fresh () in
            let loc_map, es = edge_list_of_stmt method_id loc_map entry l ret exc decl in
            (loc_map, (l, es))
        | `Opt_exp_rep_COMMA_exp_SEMI (None, _) -> (loc_map, (entry, []))
        | `Opt_exp_rep_COMMA_exp_SEMI (Some (e, es), _) ->
            let exprs = e :: List.map ~f:snd es in
            (loc_map, expr_of_nonempty_list ~curr_loc:entry ~exc exprs |> snd)
        (* discard value of initializer expression, as in Java semantics *)
      in
      let cond, (cond_intermediate_loc, cond_intermediate_stmts) =
        match cond with
        | None -> Ast.(Expr.Lit (Lit.Bool true), (init_intermediate_loc, []))
        | Some cond -> expr ~curr_loc:init_intermediate_loc ~exc cond
      in
      let cond_neg = Ast.(Expr.unop Unop.Not cond) in
      let update_intermediate_loc, update_intermediate_stmts =
        match update with
        | None -> (body_exit, [])
        | Some (e, es) ->
            let exprs = e :: List.map ~f:snd es in
            expr_of_nonempty_list ~curr_loc:body_exit ~exc exprs |> snd
        (* discard value of update expression, as in Java semantics *)
      in
      let back_edge = (update_intermediate_loc, init_intermediate_loc, Ast.Stmt.Skip) in
      (cond_intermediate_loc, body_entry, Ast.Stmt.Assume cond)
      :: (cond_intermediate_loc, exit, Ast.Stmt.Assume cond_neg)
      :: back_edge
      :: (init_intermediate_stmts @ update_intermediate_stmts @ cond_intermediate_stmts)
      |> fun edges -> (loc_map, edges, back_edge)

let parse_formals = function
  | _open_paren, _rcvr_param, Some (first, rest), _close_paren ->
      let formal_name = function
        | `Formal_param (_mods, _type, (`Id (_, id), _dims)) -> id
        | _ -> failwith "spread params not yet handled"
      in
      List.fold rest ~init:[ formal_name first ] ~f:(fun acc curr ->
          (snd curr |> formal_name) :: acc)
  | _, _, None, _ -> []

let types_of_formals = function
  | _open_paren, _rcvr_param, Some (first, rest), _close_paren ->
      let type_of_formal = function
        | `Spread_param _ -> failwith "spread params not yet handled"
        | `Formal_param (_mods, t, _) -> string_of_unannotated_type t
      in
      List.fold rest ~init:[ type_of_formal first ] ~f:(fun acc curr ->
          (snd curr |> type_of_formal) :: acc)
  | _, _, None, _ -> []

let of_method_decl loc_map ?(package = []) ~class_name (md : CST.method_declaration) =
  match md with
  | modifiers, (_tparams, _type, (`Id (_, method_name), formals, _), _throws), `Blk (_, stmts, _) ->
      let entry = Cfg.Loc.fresh () in
      let exit = Cfg.Loc.fresh () in
      let exc_exit = Cfg.Loc.fresh () in
      let arg_types = types_of_formals formals in
      let formals = parse_formals formals in
      let locals = declarations stmts in
      let static =
        Option.exists modifiers ~f:(List.exists ~f:(function `Static _ -> true | _ -> false))
      in
      let method_id : Method_id.t = { package; class_name; method_name; static; arg_types } in
      let fn : Cfg.Fn.t = { method_id; formals; locals; entry; exit; exc_exit } in
      let loc_map, edges =
        edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret:exit ~exc:exc_exit stmts
      in
      Some (loc_map, edges, fn)
  | _, (_, _, (`Choice_open _, _, _), _), _ -> None
  | _, _, `SEMI _ -> None

let of_constructor_decl loc_map ?(package = []) ~class_name (cd : CST.constructor_declarator)
    (body : CST.constructor_body) =
  match (cd, body) with
  | (_tparams, _, formals), (_, explicit_constructor_invo, stmts, _) ->
      let entry = Cfg.Loc.fresh () in
      let exit = Cfg.Loc.fresh () in
      let exc_exit = Cfg.Loc.fresh () in
      let arg_types = types_of_formals formals in
      let formals = parse_formals formals in
      let locals = declarations stmts in
      let method_id : Method_id.t =
        { package; class_name; method_name = "<init>"; static = false; arg_types }
      in
      let fn : Cfg.Fn.t = { method_id; formals; locals; entry; exit; exc_exit } in
      let loc_map, edges =
        match explicit_constructor_invo with
        | None ->
            edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret:exit ~exc:exc_exit stmts
        | Some (`Opt_type_args_choice_this (_typargs, `This _), args, _) ->
            let args = match args with _, Some (e, es), _ -> e :: List.map ~f:snd es | _ -> [] in
            let actuals, (pre_invocation_loc, pre_invocation_edges) =
              List.fold args
                ~init:([], (entry, []))
                ~f:(fun (acc_exprs, (curr_loc, acc_intermediates)) arg ->
                  let arg_expr, (curr_loc, arg_intermediates) = expr ~curr_loc ~exc:exc_exit arg in
                  (acc_exprs @ [ arg_expr ], (curr_loc, acc_intermediates @ arg_intermediates)))
            in
            let post_invocation_loc = Cfg.Loc.fresh () in
            let loc_map, body_edges =
              edge_list_of_stmt_list method_id loc_map ~entry:post_invocation_loc ~exit ~ret:exit
                ~exc:exc_exit stmts
            in
            let invocation =
              ( pre_invocation_loc,
                post_invocation_loc,
                Ast.Stmt.Call
                  {
                    lhs = "this";
                    rcvr = class_name;
                    meth = "<init>";
                    actuals;
                    alloc_site = Some (Alloc_site.fresh ());
                  } )
            in
            let exc_invocation =
              ( pre_invocation_loc,
                exc_exit,
                Ast.Stmt.Exceptional_call { rcvr = class_name; meth = "<init>"; actuals } )
            in
            (loc_map, invocation :: exc_invocation :: (pre_invocation_edges @ body_edges))
        | Some (`Choice_prim_exp_DOT_opt_type_args_super _, _, _) ->
            unimplemented "`Choice_prim_exp_DOT_opt_type_args_super" (loc_map, [])
        | Some (`Opt_type_args_choice_this _, _, _) ->
            unimplemented "`Opt_type_args_choice_this" (loc_map, [])
      in

      Some (loc_map, edges, fn)

let rec parse_class_decl ?(package = []) ?(containing_class_name = None) ~imports
    ~(acc : prgm_parse_result) : CST.class_declaration -> prgm_parse_result = function
  | _modifiers, _, (_, class_name), _type_params, superclass, _superinterfaces, (_, decls, _) ->
      let class_name =
        match containing_class_name with Some n -> n ^ "$" ^ class_name | None -> class_name
      in
      let cha =
        match superclass with
        | None -> acc.cha
        | Some (_, `Unan_type ut) | Some (_, `Anno_type (_, ut)) -> (
            let superclass_name = string_of_unannotated_type ut in
            match Map.find imports superclass_name with
            | Some super_package ->
                Class_hierarchy.add ~package ~class_name ~super_package ~superclass_name acc.cha
            | None -> acc.cha )
      in
      let fields =
        let declared_fields =
          List.(
            decls >>= function
            | `Field_decl (mods, _, (v, vs), _) ->
                let is_static =
                  Option.exists mods ~f:(exists ~f:(function `Static _ -> true | _ -> false))
                in
                if is_static then [] else v :: map ~f:snd vs |> map ~f:ident_of_var_declarator
            | _ -> [])
          |> String.Set.of_list
        in
        Declared_fields.add ~package ~class_name ~fields:declared_fields acc.fields
      in
      List.fold decls ~init:{ cfgs = acc.cfgs; fields; cha; loc_map = acc.loc_map } ~f:(fun acc ->
        function
        | `Meth_decl md -> (
            match of_method_decl acc.loc_map ~package ~class_name md with
            | Some (loc_map, edges, fn) ->
                {
                  cfgs = Cfg.add_fn fn ~edges acc.cfgs;
                  loc_map;
                  fields = acc.fields;
                  cha = acc.cha;
                }
            | None -> acc )
        | `Class_decl cd ->
            parse_class_decl cd ~package ~containing_class_name:(Some class_name) ~imports ~acc
        | `Cons_decl (_, cd, _, body) -> (
            match of_constructor_decl acc.loc_map ~package ~class_name cd body with
            | Some (loc_map, edges, fn) ->
                {
                  cfgs = Cfg.add_fn fn ~edges acc.cfgs;
                  loc_map;
                  fields = acc.fields;
                  cha = acc.cha;
                }
            | None -> acc )
        | `Field_decl _ ->
            acc (* skip over field declarations, as they are handled outside of this List.fold *)
        | `Inte_decl _ -> acc (* skip over interface declarations -- no code to analyze! *)
        | `Anno_type_decl _ -> unimplemented "`Anno_type_decl" acc
        | `SEMI _ -> unimplemented "`SEMI" acc
        | `Blk _ -> unimplemented "`Blk" acc
        | `Enum_decl _ -> unimplemented "`Enum_decl" acc
        | `Static_init _ -> unimplemented "`Static_init" acc
        | `Record_decl _ -> unimplemented "`Record_decl" acc)

(*        | d ->
            failwith
              (Format.asprintf "unrecognized class body declaration: %a" Sexp.pp
                 (CST.sexp_of_class_body_declaration d)))*)

let of_java_cst ?(diagnostic = false) ?(acc = empty_parse_result) (cst : CST.program) :
    prgm_parse_result =
  diagnostic_mode := diagnostic;
  let package =
    List.find_map cst ~f:(function `Decl (`Pack_decl (_, _, name, _)) -> Some name | _ -> None)
    |> function
    | None -> []
    | Some n ->
        let rec list_of_name = function
          | `Id (_, ident) -> [ ident ]
          | `Choice_open _ -> unimplemented "`Choice_open" []
          | `Scoped_id (nm, _dot, (_, ident)) -> list_of_name nm @ [ ident ]
        in
        list_of_name n
  in
  (* best-effort local name resolution:
   * For each "import foo.bar.Baz;", [imports] maps "Baz" to ["foo" ; "bar"]
   * For each "class Foo { ... }"  in this file, also map "Foo" to its [package] declaration
   *)
  let imports : string list String.Map.t =
    String.Map.of_alist_exn
    @@ List.filter_map cst ~f:(function
         | `Decl (`Import_decl (_, _, nm, None, _)) -> (
             let rec quals = function
               | `Id (_, ident) -> [ ident ]
               | `Scoped_id (prefix, _, (_, ident)) -> ident :: quals prefix
               | `Choice_open _ -> unimplemented "`Choice_open" []
             in
             match nm with
             | `Id (_, ident) -> Some (ident, [])
             | `Scoped_id (q, _, (_, ident)) -> Some (ident, List.rev (quals q))
             | `Choice_open _ -> unimplemented "`Choice_open" None )
         | `Decl (`Class_decl (_, _, (_, class_name), _, _, _, _)) -> Some (class_name, package)
         | _ -> None)
  in
  List.fold cst ~init:acc ~f:(fun acc -> function
    | `Decl (`Class_decl cd) -> parse_class_decl cd ~package ~acc ~imports
    | `Decl (`Enum_decl _) | `Decl (`Import_decl _) | `Decl (`Inte_decl _) | `Decl (`Pack_decl _) ->
        acc
    | `Decl (`Module_decl _) -> unimplemented "`Module_decl" acc
    | `Decl (`Anno_type_decl _) -> unimplemented "`Anno_type_decl" acc
    | stmt ->
        let rec first_atom = function
          | Sexp.Atom a -> a
          | List [] -> "empty-sexp-list"
          | Sexp.List (l :: _) -> first_atom l
        in
        unimplemented (first_atom (CST.sexp_of_statement stmt) ^ "-at-top-level") acc)

open Result.Monad_infix

let of_file_exn ?(acc = empty_parse_result) filename =
  let file = Src_file.of_file filename in
  let tree = Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file in
  match tree with
  | Ok tree -> of_java_cst ~acc tree
  | Error _e -> failwith @@ "parse error in " ^ filename

let of_files ~files =
  List.fold files ~init:empty_parse_result ~f:(fun acc file -> of_file_exn ~acc file)

let%test "hello world program" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/HelloWorld.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst |> Result.is_ok

let%test "nested classes" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/NestedClasses.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst |> Result.is_ok

let%test "nested loops" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/NestedLoops.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (fun res ->
       match res with
       | Error _ -> ()
       | Ok { cfgs; _ } -> Cfg.dump_dot_interproc ~filename:"nested_loops.dot" cfgs)
  |> Result.is_ok

let%test "constructors" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Constructors.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (fun res ->
       match res with
       | Error _ -> ()
       | Ok { cfgs; _ } ->
           Cfg.dump_dot_interproc ~filename:(abs_of_rel_path "constructors.dot") cfgs)
  |> Result.is_ok

let%test "Cibai example" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/CibaiExample/MiniBag.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (fun res ->
       match res with
       | Error _ -> ()
       | Ok { cfgs; _ } ->
           Cfg.dump_dot_interproc ~filename:(abs_of_rel_path "cibai_example.dot") cfgs)
  |> Result.is_ok

let%test "exceptions, try, catch, finally" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Exceptions.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (fun res ->
       match res with
       | Error _ -> ()
       | Ok { cfgs; _ } -> Cfg.dump_dot_interproc ~filename:(abs_of_rel_path "exceptions.dot") cfgs)
  |> Result.is_ok