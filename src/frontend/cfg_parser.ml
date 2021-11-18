open Dai.Import
open Tree_sitter_java
open Syntax
open Cfg

type edge = Loc.t * Loc.t * Ast.Stmt.t

type prgm_parse_result = {
  loc_map : Loc_map.t;
  cfgs : t Fn.Map.t;
  fields : Declared_fields.t;
  cha : Class_hierarchy.t;
}

let set_parse_result ?loc_map ?cfgs ?fields ?cha acc =
  let loc_map = Option.value loc_map ~default:acc.loc_map in
  let cfgs = Option.value cfgs ~default:acc.cfgs in
  let fields = Option.value fields ~default:acc.fields in
  let cha = Option.value cha ~default:acc.cha in
  { loc_map; cfgs; fields; cha }

let empty_parse_result =
  {
    loc_map = Loc_map.empty;
    cfgs = Fn.Map.empty;
    fields = Declared_fields.empty;
    cha = Class_hierarchy.empty;
  }

let tmp_var_counter = ref 0

let diagnostic_mode = ref false

let set_diagnostic b = diagnostic_mode := b

let unimplemented_syntax : int String.Map.t ref = ref String.Map.empty

(* when in [diagnostic_mode], accumulate a count of appearances of unimplemented syntactic forms *)

let unimplemented syntax default_val =
  if !diagnostic_mode then (
    let old_count = Map.find !unimplemented_syntax syntax |> Option.value ~default:0 in
    unimplemented_syntax := Map.set !unimplemented_syntax ~key:syntax ~data:(old_count + 1);
    default_val)
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

let rec string_of_simple_type = function
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
  | `Scoped_type_id (((`Id _ | `Gene_type _ | `Scoped_type_id _) as base_typ), _, _, (_, typ)) ->
      string_of_simple_type base_typ ^ "$" ^ typ
  | `Gene_type (((`Id _ | `Scoped_type_id _) as typ), _) -> string_of_simple_type typ

let rec string_of_unannotated_type = function
  | `Choice_void_type st -> string_of_simple_type st
  | `Array_type (ut, _) -> string_of_unannotated_type ut ^ "[]"

let string_of_type = function `Unan_type ut | `Anno_type (_, ut) -> string_of_unannotated_type ut

(** Convert an expression concrete syntax tree to an expression in our IR, along with potentially some preceding statements for any function invocations and assignments therein, and a shifted current program location to accomodate those intermediate statements.
    That is, 
      * if `cst` represents a simple expression with no function invocations or assignments, return value is (<that expression in our IR>, (curr_loc,[]))
      * if `cst` contains function invocations f_1 ... f_k and assignments x_1=e_1 ... x_n=e_n, return value is 
          (
             <that expression in our IR>[ tmp_var_i / f_i][x_i / x_i=e_i],
             Some (fresh_loc_k+n, [curr_loc -[tmp_var_1 := f_1]-> fresh_loc_1, ... , fresh_loc_(k-1) -[tmp_var_k := f_k]-> fresh_loc_k] ++ [fresh_loc_k -[x_1=e_1]-> fresh_loc_k+1, ... , fresh_loc_(k+n-1) -[x_n=e_n]-> fresh_loc_(k+n)])

    Optional [exit_loc] param is used to special-case the common statement syntax of [`Exp_stmt (`Assign_exp _)] and avoid generating extraneous locations and [Skip] edges
*)
let rec expr ?exit_loc ~(curr_loc : Loc.t) ~(exc : Loc.t) (cst : CST.expression) :
    Ast.Expr.t * (Loc.t * edge list) =
  let open Ast in
  let placeholder_expr = (Expr.Lit (Lit.String "DIAGNOSTIC MODE PLACEHOLDER"), (curr_loc, [])) in
  match cst with
  | `Assign_exp (lhs, op, rhs) ->
      let rhs_expr, (curr_loc, rhs_intermediates) = expr ~curr_loc ~exc rhs in
      let lhs_expr, (curr_loc, lhs_intermediates) =
        match lhs with
        | `Id (_, ident) -> (Expr.Var ident, (curr_loc, []))
        | `Choice_open (`Open _) -> (Expr.Var "open", (curr_loc, []))
        | `Choice_open (`Module _) -> (Expr.Var "module", (curr_loc, []))
        | `Field_access (rcvr, _, _, field) ->
            let rcvr, aux_info =
              match rcvr with
              | `Super _ -> ("super", (curr_loc, []))
              | `Prim_exp _ as e -> expr_as_var ~curr_loc ~exc e
            in
            let field =
              match field with
              | `Id (_, fld) -> fld
              | `Choice_open (`Module _) -> "module"
              | `Choice_open _ ->
                  unimplemented "\"open\" as field name" "DIAGNOSTIC MODE PLACEHOLDER"
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
        | `AMPEQ _ -> Expr.binop lhs_expr Binop.BAnd rhs_expr
        | `BAREQ _ -> Expr.binop lhs_expr Binop.BOr rhs_expr
        | `HATEQ _ -> Expr.binop lhs_expr Binop.BXor rhs_expr
        | `PERCEQ _ -> Expr.binop lhs_expr Binop.Mod rhs_expr
        | `LTLTEQ _ -> Expr.binop lhs_expr Binop.LShift rhs_expr
        | `GTGTEQ _ -> Expr.binop lhs_expr Binop.RShift rhs_expr
        | `GTGTGTEQ _ -> Expr.binop lhs_expr Binop.URShift rhs_expr
      in
      let stmt =
        match lhs with
        | `Id (_, lhs) -> Stmt.Assign { lhs; rhs = rhs_expr_with_op }
        | `Choice_open (`Open _) -> Stmt.Assign { lhs = "open"; rhs = rhs_expr_with_op }
        | `Choice_open (`Module _) -> Stmt.Assign { lhs = "module"; rhs = rhs_expr_with_op }
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
      let next_loc = Option.value exit_loc ~default:(Loc.fresh ()) in
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
        | `Exp_AMP_exp (e1, _, e2) -> (e1, Binop.BAnd, e2)
        | `Exp_BAR_exp (e1, _, e2) -> (e1, Binop.BOr, e2)
        | `Exp_HAT_exp (e1, _, e2) -> (e1, Binop.BXor, e2)
        | `Exp_PERC_exp (e1, _, e2) -> (e1, Binop.Mod, e2)
        | `Exp_LTLT_exp (e1, _, e2) -> (e1, Binop.LShift, e2)
        | `Exp_GTGT_exp (e1, _, e2) -> (e1, Binop.RShift, e2)
        | `Exp_GTGTGT_exp (e1, _, e2) -> (e1, Binop.URShift, e2)
      in
      let l, (curr_loc, l_intermediates) = expr ~curr_loc ~exc l in
      let r, (curr_loc, r_intermediates) = expr ~curr_loc ~exc r in
      (Expr.Binop { l; op; r }, (curr_loc, l_intermediates @ r_intermediates))
  | `Cast_exp (_, _type, _, _, e) -> expr ~curr_loc ~exc e
  | `Inst_exp (e, _instanceof, `Unan_type ty) | `Inst_exp (e, _instanceof, `Anno_type (_, ty)) ->
      let l, aux = expr ~curr_loc ~exc e in
      let r = Expr.Lit (Lit.String (string_of_unannotated_type ty)) in
      let op = Binop.Instanceof in
      (Expr.Binop { l; op; r }, aux)
  | `Lambda_exp _ -> unimplemented "`Lambda_exp" placeholder_expr
  | `Prim_exp (`Lit l) ->
      let e =
        Expr.Lit
          (match l with
          | `Deci_int_lit (_, raw) | `Hex_int_lit (_, raw) | `Bin_int_lit (_, raw) ->
              raw
              |> String.chop_suffix_if_exists ~suffix:"l"
              |> String.chop_suffix_if_exists ~suffix:"L"
              |> Int64.of_string |> Lit.of_int
          | `Octal_int_lit (_, raw) ->
              (* add an "0o" prefix to force octal decoding *)
              raw
              |> String.chop_suffix_if_exists ~suffix:"l"
              |> String.chop_suffix_if_exists ~suffix:"L"
              |> (fun s -> if String.is_prefix ~prefix:"0o" s then s else "0o" ^ s)
              |> Int64.of_string |> Lit.of_int
          | `Deci_floa_point_lit (_, raw) | `Hex_floa_point_lit (_, raw) ->
              raw
              |> String.chop_suffix_if_exists ~suffix:"f"
              |> String.chop_suffix_if_exists ~suffix:"F"
              |> String.chop_suffix_if_exists ~suffix:"d"
              |> String.chop_suffix_if_exists ~suffix:"D"
              |> Float.of_string |> Lit.of_float
          | `True _ -> Lit.Bool true
          | `False _ -> Lit.Bool false
          | `Char_lit (_, raw) ->
              raw
              |> String.chop_prefix_if_exists ~prefix:"\'"
              |> String.chop_suffix_if_exists ~suffix:"\'"
              |> (fun s -> try Scanf.unescaped s with _ -> "DAI_UNESCAPED_FAILED_" ^ s)
              |> Lit.char_of_string
          | `Str_lit (_, raw) ->
              raw
              |> String.chop_prefix_if_exists ~prefix:"\""
              |> String.chop_suffix_if_exists ~suffix:"\""
              |> (fun s -> try Scanf.unescaped s with _ -> "DAI_UNESCAPED_FAILED_" ^ s)
              |> Lit.of_string
          | `Null_lit _ -> Lit.Null)
      in
      (e, (curr_loc, []))
  | `Prim_exp (`Class_lit (typ, _, _)) ->
      (Expr.Class_lit { name = string_of_unannotated_type typ }, (curr_loc, []))
  | `Prim_exp (`This _) -> (Expr.Var "this", (curr_loc, []))
  | `Prim_exp (`Id (_, ident)) -> (Expr.Var ident, (curr_loc, []))
  | `Prim_exp (`Choice_open (`Open _)) -> (Expr.Var "open", (curr_loc, []))
  | `Prim_exp (`Choice_open (`Module _)) -> (Expr.Var "module", (curr_loc, []))
  | `Prim_exp (`Paren_exp (_, e, _)) -> expr ?exit_loc ~curr_loc ~exc e
  | `Prim_exp (`Obj_crea_exp (`Unqu_obj_crea_exp unqualified))
  | `Prim_exp (`Obj_crea_exp (`Prim_exp_DOT_unqu_obj_crea_exp (_, _, unqualified))) ->
      (* todo: deal with fully-qualified constructors *)
      let _, _targs, typ, (_, args, _), _initializer = unqualified in
      let ctor_name = string_of_simple_type typ in
      let args = match args with Some (e, es) -> e :: List.map ~f:snd es | None -> [] in
      let actuals, (curr_loc, arg_intermediates) =
        List.fold args
          ~init:([], (curr_loc, []))
          ~f:(fun (acc_exprs, (curr_loc, acc_intermediates)) arg ->
            let arg_expr, (curr_loc, arg_intermediates) = expr ~curr_loc ~exc arg in
            (acc_exprs @ [ arg_expr ], (curr_loc, acc_intermediates @ arg_intermediates)))
      in
      let lhs = fresh_tmp_var () in
      let next_loc = Option.value exit_loc ~default:(Loc.fresh ()) in
      let edges =
        ( curr_loc,
          next_loc,
          Stmt.Call
            {
              lhs = Some lhs;
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
        | `Choice_open (`Open _) -> "open"
        | `Choice_open (`Module _) -> "module"
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
        | `Choice_id (`Choice_open (`Open _)) -> ("this", "open", (curr_loc, []))
        | `Choice_id (`Choice_open (`Module _)) -> ("this", "module", (curr_loc, []))
        | `Choice_prim_exp_DOT_opt_super_DOT_opt_type_args_choice_id
            (rcvr, _dot, super, _typeargs, meth) -> (
            let rcvr, aux_info =
              match rcvr with
              | `Prim_exp _ as e when Option.is_none super -> expr_as_var ~curr_loc ~exc e
              | `Prim_exp _ ->
                  unimplemented "`Choice_prim_exp_DOT_super" ("PLACEHOLDER", (curr_loc, []))
              | `Super _ when Option.is_none super -> ("super", (curr_loc, []))
              | `Super _ -> unimplemented "`Choice_super_DOT_super" ("PLACEHOLDER", (curr_loc, []))
            in
            match meth with
            | `Id (_, meth_name) -> (rcvr, meth_name, aux_info)
            | `Choice_open (`Open _) -> (rcvr, "open", aux_info)
            | `Choice_open (`Module _) -> (rcvr, "module", aux_info))
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
      let next_loc = Option.value exit_loc ~default:(Loc.fresh ()) in
      let call =
        (curr_loc, next_loc, Stmt.Call { lhs = Some lhs; rcvr; meth; actuals; alloc_site = None })
      in
      let exc_call = (curr_loc, exc, Stmt.Exceptional_call { rcvr; meth; actuals }) in
      (Expr.Var lhs, (next_loc, (call :: exc_call :: rcvr_intermediates) @ arg_intermediates))
  | `Prim_exp (`Meth_ref (klass, _, _, meth)) -> (
      let meth = match meth with `Id (_, id) -> id | `New _ -> "<init>" in
      match klass with
      | `Type ty -> (Expr.Method_ref { rcvr = string_of_type ty; meth }, (curr_loc, []))
      | `Prim_exp _ as e ->
          let rcvr, aux = expr_as_var ~curr_loc ~exc e in
          (Expr.Method_ref { rcvr; meth }, aux)
      | `Super _ -> (Expr.Method_ref { rcvr = "super"; meth }, (curr_loc, [])))
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
          let tmp = fresh_tmp_var () in
          let next_loc = match exit_loc with Some l -> l | None -> Loc.fresh () in
          let array = Ast.Expr.Array_create { elt_type; size = outermost_dim_size; alloc_site } in
          let stmt = (curr_loc, next_loc, Stmt.Assign { lhs = tmp; rhs = array }) in
          (Expr.Var tmp, (next_loc, stmt :: intermediate_stmts)))
  | `Switch_exp (_, (_, _matching_exp, _), (_, cases_block, _)) -> (
      match cases_block with
      | `Rep_switch_blk_stmt_group _cases ->
          unimplemented "`Rep_switch_blk_stmt_group`" placeholder_expr
      | `Rep_switch_rule _cases -> unimplemented "`Rep_switch_rule`" placeholder_expr)
  | `Tern_exp (if_exp, _, then_exp, _, else_exp) ->
      let tmp = fresh_tmp_var () in
      let if_exp, (curr_loc, cond_intermediates) = expr ~curr_loc ~exc if_exp in
      let then_branch_head = Loc.fresh () in
      let else_branch_head = Loc.fresh () in
      let next_loc = Option.value exit_loc ~default:(Loc.fresh ()) in
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
  | `Un_exp (`DASH_exp (_, `Prim_exp (`Lit (`Deci_int_lit (_, "9223372036854775808")))))
  | `Un_exp (`DASH_exp (_, `Prim_exp (`Lit (`Deci_int_lit (_, "9223372036854775808l")))))
  | `Un_exp (`DASH_exp (_, `Prim_exp (`Lit (`Deci_int_lit (_, "9223372036854775808L"))))) ->
      (* special-case this because otherwise it gets parsed as the negation of an int literal one larger than Int64.max_value, causing a crash*)
      (Expr.Lit (Lit.Int Int64.min_value), (curr_loc, []))
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
          let next_loc = Option.value exit_loc ~default:(Loc.fresh ()) in
          let update_edge =
            ( curr_loc,
              next_loc,
              Stmt.Assign { lhs = v; rhs = Expr.binop var op (Expr.Lit (Lit.Int 1L)) } )
          in
          if is_pre then (var, (next_loc, update_edge :: intermediates))
          else
            let inverse_op =
              match op with
              | Binop.Plus -> Binop.Minus
              | Binop.Minus -> Binop.Plus
              | _ -> failwith "unreachable"
            in
            ( Expr.binop var inverse_op (Expr.Lit (Lit.Int 1L)),
              (next_loc, update_edge :: intermediates) )
      | (Expr.Deref _ | Expr.Array_access _) as heap_loc ->
          let next_loc = Loc.fresh () in
          let end_loc = Option.value exit_loc ~default:(Loc.fresh ()) in
          let result = fresh_tmp_var () in
          let update_edges =
            match heap_loc with
            | Expr.Deref { rcvr; field } ->
                [
                  ( curr_loc,
                    next_loc,
                    Stmt.Assign
                      { lhs = result; rhs = Expr.binop heap_loc op (Expr.Lit (Lit.Int 1L)) } );
                  (next_loc, end_loc, Stmt.Write { rcvr; field; rhs = Expr.Var result });
                ]
            | Expr.Array_access { rcvr = Expr.Var rcvr; idx = Expr.Var idx } ->
                [
                  ( curr_loc,
                    next_loc,
                    Stmt.Assign
                      { lhs = result; rhs = Expr.binop heap_loc op (Expr.Lit (Lit.Int 1L)) } );
                  ( next_loc,
                    end_loc,
                    Stmt.Array_write { rcvr; idx = Expr.Var idx; rhs = Expr.Var result } );
                ]
            | Expr.Array_access _ ->
                unimplemented "Unary_increment_or_decrement_of_nonvar_array_or_idx" []
            | _ -> failwith "unreachable"
          in
          if is_pre then (Expr.Var result, (end_loc, update_edges @ intermediates))
          else
            let inverse_op =
              match op with
              | Binop.Plus -> Binop.Minus
              | Binop.Minus -> Binop.Plus
              | _ -> failwith "unreachable"
            in
            ( Expr.binop (Expr.Var result) inverse_op (Expr.Lit (Lit.Int 1L)),
              (end_loc, update_edges @ intermediates) )
      | _ ->
          if is_pre then (Expr.binop e op (Expr.Lit (Lit.Int 1L)), (curr_loc, intermediates))
          else (e, (curr_loc, intermediates)))

and expr_as_var ~(curr_loc : Loc.t) ~(exc : Loc.t) (cst : CST.expression) :
    string * (Loc.t * edge list) =
  let open Ast in
  let e, (curr_loc, intermediates) = expr ~curr_loc ~exc cst in
  match e with
  | Expr.Var v -> (v, (curr_loc, intermediates))
  | e ->
      let tmp = fresh_tmp_var () in
      let next_loc = Loc.fresh () in
      (tmp, (next_loc, (curr_loc, next_loc, Stmt.Assign { lhs = tmp; rhs = e }) :: intermediates))

and array_lit ~(curr_loc : Loc.t) ~(exc : Loc.t) (lit : CST.array_initializer) :
    Ast.Expr.t * (Loc.t * edge list) =
  let expr_of_var_initializer curr_loc (vi : CST.variable_initializer) =
    match vi with `Exp e -> expr ~curr_loc ~exc e | `Array_init ai -> array_lit ~curr_loc ~exc ai
  in
  let tmp = fresh_tmp_var () in
  let next_loc = Loc.fresh () in
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
      let lit = Ast.Expr.Array_literal { elts; alloc_site = Alloc_site.fresh () } in
      let stmt = (curr_loc, next_loc, Ast.Stmt.Assign { lhs = tmp; rhs = lit }) in
      (Ast.Expr.Var tmp, (next_loc, stmt :: intermediates))
  | _, None, _, _ ->
      let lit = Ast.Expr.Array_literal { elts = []; alloc_site = Alloc_site.fresh () } in
      let stmt = (curr_loc, next_loc, Ast.Stmt.Assign { lhs = tmp; rhs = lit }) in
      (Ast.Expr.Var tmp, (next_loc, [ stmt ]))

(** lift [expr] to non-empty lists of expressions, according to Java semantics: i.e. throwing away value of all but last *)
let rec expr_of_nonempty_list ~(curr_loc : Loc.t) ~(exc : Loc.t) :
    CST.expression list -> Ast.Expr.t * (Loc.t * edge list) = function
  | [] -> failwith "only non-empty lists of expressions supported"
  | [ e ] -> expr ~curr_loc ~exc e
  | e :: es ->
      let _, (intermediate_loc, intermediate_stmts) = expr ~curr_loc ~exc e in
      expr_of_nonempty_list ~curr_loc:intermediate_loc ~exc es |> fun (e, (l, stmts)) ->
      (e, (l, intermediate_stmts @ stmts))

let ident_of_var_declarator_id = function
  | `Id (_, ident), _ -> ident
  | `Choice_open (`Open _), _ -> "open"
  | `Choice_open (`Module _), _ -> "module"

let ident_of_var_declarator = fst >> ident_of_var_declarator_id

let rec declarations stmts : string list =
  let open List.Monad_infix in
  let decls_of_var_decl_id : CST.variable_declarator_id -> string list =
    fst >> function `Id (_, x) -> [ x ] | _ -> []
  in
  let decls_of_catch_clause_list : CST.catch_clause list -> string list =
    List.bind ~f:(fun (_, _, (_, _, var_decl_id), _, (_, blk, _)) ->
        decls_of_var_decl_id var_decl_id @ declarations blk)
  in
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
        | _ -> local_decls_of_stmt s)
    | `Exp_stmt _ -> []
    | `For_stmt (_, _, binding, _, _, _, _, s) -> (
        match binding with
        | `Local_var_decl (_, _, _, _) as l -> local_decls_of_stmt l @ local_decls_of_stmt s
        | _ -> local_decls_of_stmt s)
    | `If_stmt (_, _, t_stmt, f_branch) -> (
        match f_branch with
        | None -> local_decls_of_stmt t_stmt
        | Some (_, f_stmt) -> local_decls_of_stmt t_stmt @ local_decls_of_stmt f_stmt)
    | `Labe_stmt (_label, _, s) -> local_decls_of_stmt s
    | `Local_var_decl (_, _, (v, vs), _) ->
        ident_of_var_declarator v :: (vs >>| (snd >> ident_of_var_declarator))
    | `Ret_stmt _ -> []
    | `SEMI _ -> []
    | `Switch_exp (_, _, (_, cases_block, _)) -> (
        match cases_block with
        | `Rep_switch_blk_stmt_group cases -> cases >>= (snd >> declarations)
        | `Rep_switch_rule cases -> (
            cases >>= function
            | _, _, ((`Exp_stmt _ | `Throw_stmt _) as s) -> declarations [ s ]
            | _, _, `Blk (_, block_stmts, _) -> declarations block_stmts))
    | `Sync_stmt (_, _, (_, stmts, _)) -> declarations stmts
    | `Throw_stmt _ -> []
    | `Try_stmt (_, (_, stmts, _), catches) ->
        let catch_and_finally_decls =
          match catches with
          | `Rep1_catch_clause ccs -> decls_of_catch_clause_list ccs
          | `Rep_catch_clause_fina_clause (ccs, (_, (_, fin_blk, _))) ->
              decls_of_catch_clause_list ccs @ declarations fin_blk
        in
        catch_and_finally_decls @ declarations stmts
    | `Try_with_resous_stmt (_, resource_spec, (_, blk, _), catches, opt_finally) ->
        let rs = match resource_spec with _, r, rs, _, _ -> r :: List.map ~f:snd rs in
        List.bind rs ~f:(function
          | `Opt_modifs_unan_type_var_decl_id_EQ_exp (_, _, vdi, _, _) -> decls_of_var_decl_id vdi
          | `Id (_, x) -> [ x ]
          | _ -> [])
        @ Option.fold opt_finally ~init:[] ~f:(fun _ (_, (_, fin_blk, _)) -> declarations fin_blk)
        @ decls_of_catch_clause_list catches
        @ declarations blk
    | `While_stmt (_, _, s) -> local_decls_of_stmt s
    | `Yield_stmt _ -> []
  in
  stmts >>= local_decls_of_stmt

let rec edge_list_of_stmt method_id loc_map entry exit ret exc ?(brk = (None, String.Map.empty))
    ?(cont = (None, String.Map.empty)) ?cont_label stmt : Loc_map.t * edge list =
  let open Ast in
  let loc_map =
    match Loc_map.add method_id stmt { entry; exit; ret; exc } loc_map with
    | `Ok lm -> lm
    | `Collision ->
        (* handle this case if it comes up in evaluation.  Perhaps by enumerating the occurrences topologically to disambiguate, storing a sequence of loc_ctx's for the statement and scanning parse trees for the order as needed?*)
        (* Format.(fprintf std_formatter)
           "WARNING: two syntactically-identical statements in method %s.%s; this is problematic \
            only if there's an edit there.\n"
           method_id.class_name method_id.method_name;*)
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
  | `Blk (_, stmts, _) ->
      edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret ~exc ~brk ~cont stmts
  | `Brk_stmt (_, None, _) -> (
      match fst brk with
      | Some brk_target -> (loc_map, [ (entry, brk_target, Stmt.Skip) ])
      | None -> unimplemented "`Brk_stmt inside try catch with finally" (loc_map, []))
  | `Brk_stmt (_, Some (_, label), _) -> (
      match String.Map.find (snd brk) label with
      | Some cont_target -> (loc_map, [ (entry, cont_target, Stmt.Skip) ])
      | None -> unimplemented "`Brk_stmt with label inside try catch with finally" (loc_map, []))
  | `Cont_stmt (_, None, _) -> (
      match fst cont with
      | Some cont_target -> (loc_map, [ (entry, cont_target, Stmt.Skip) ])
      | None -> unimplemented "`Cont_stmt inside try catch with finally" (loc_map, []))
  | `Cont_stmt (_, Some (_, label), _) -> (
      match String.Map.find (snd cont) label with
      | Some cont_target -> (loc_map, [ (entry, cont_target, Stmt.Skip) ])
      | None -> unimplemented "`Cont_stmt with label inside try catch with finally" (loc_map, []))
  | `Decl (`Module_decl _) -> unimplemented "`Module_decl in edge_list_of_stmt" (loc_map, [])
  | `Decl (`Pack_decl _) -> unimplemented "`Pack_decl in edge_list_of_stmt" (loc_map, [])
  | `Decl (`Import_decl _) -> unimplemented "`Import_decl in edge_list_of_stmt" (loc_map, [])
  | `Decl (`Class_decl _) -> unimplemented "`Class_decl in edge_list_of_stmt" (loc_map, [])
  | `Decl (`Inte_decl _) -> unimplemented "`Inte_decl in edge_list_of_stmt" (loc_map, [])
  | `Decl (`Anno_type_decl _) -> unimplemented "`Anno_type_decl in edge_list_of_stmt" (loc_map, [])
  | `Decl (`Enum_decl _) -> unimplemented "`Enum_decl in edge_list_of_stmt" (loc_map, [])
  | `Do_stmt (_, body, _, (_, cond, _), _) ->
      let body_exit = Loc.fresh () in
      let cond, (cond_exit, cond_intermediate_stmts) = expr ~curr_loc:body_exit ~exc cond in
      let cond_neg = Expr.unop Unop.Not cond in
      let cont =
        ( Some body_exit,
          match cont_label with
          | None -> snd cont
          | Some label -> String.Map.set (snd cont) ~key:label ~data:body_exit )
      in
      let loc_map, body =
        edge_list_of_stmt method_id loc_map entry body_exit ret exc
          ~brk:(Some exit, snd brk)
          ~cont body
      in
      (cond_exit, entry, Stmt.Assume cond) :: (cond_exit, exit, Stmt.Assume cond_neg) :: body
      |> List.append cond_intermediate_stmts
      |> pair loc_map
  | `Enha_for_stmt
      (_, _, _mods, _type, ((`Id (_, var) | `Choice_open (`Module (_, var))), None), _, exp, _, body)
    ->
      (* Technically the way this reduces depends on whether the expression is an iterable or an array.
       * because apparently arrays aren't iterables.
       * starting with the iterable implementation, since that seems to be the most prevelant version *)
      let iter = fresh_tmp_var () in
      let cond_result = fresh_tmp_var () in
      let cond_entry = Loc.fresh () in
      let cond_exit = Loc.fresh () in
      let update_entry = Loc.fresh () in
      let body_entry = Loc.fresh () in
      let expr, (expr_exit, expr_intermediate_stmts) = expr_as_var ~curr_loc:entry ~exc exp in
      let for_logic_stmts =
        [
          ( expr_exit,
            cond_entry,
            Ast.Stmt.Call
              { lhs = Some iter; rcvr = expr; meth = "iterator"; actuals = []; alloc_site = None }
          );
          ( expr_exit,
            exc,
            Ast.Stmt.Exceptional_call { rcvr = expr; meth = "iterator"; actuals = [] } );
          ( cond_entry,
            cond_exit,
            Ast.Stmt.Call
              {
                lhs = Some cond_result;
                rcvr = iter;
                meth = "hasNext";
                actuals = [];
                alloc_site = None;
              } );
          ( cond_entry,
            exc,
            Ast.Stmt.Exceptional_call { rcvr = iter; meth = "hasNext"; actuals = [] } );
          (cond_exit, update_entry, Stmt.Assume (Expr.Var cond_result));
          (cond_exit, exit, Stmt.Assume (Expr.Unop { op = Unop.Not; e = Expr.Var cond_result }));
          ( update_entry,
            body_entry,
            Ast.Stmt.Call
              { lhs = Some var; rcvr = iter; meth = "next"; actuals = []; alloc_site = None } );
          (update_entry, exc, Ast.Stmt.Exceptional_call { rcvr = iter; meth = "next"; actuals = [] });
        ]
      in
      let cont =
        ( Some cond_entry,
          match cont_label with
          | None -> snd cont
          | Some label -> String.Map.set (snd cont) ~key:label ~data:cond_entry )
      in
      let loc_map', body_stmts =
        edge_list_of_stmt method_id loc_map body_entry cond_entry ret exc
          ~brk:(Some exit, snd brk)
          ~cont body
      in
      (* add a dummy location and skip edge if there are multiple back edges to cond_entry *)
      let body_stmts =
        if List.count body_stmts ~f:(snd3 >> Loc.equal cond_entry) > 1 then
          let pre_exit_loc = Loc.fresh () in
          (pre_exit_loc, cond_entry, Stmt.Skip)
          :: List.map body_stmts ~f:(fun (src, dst, lbl) ->
                 (src, (if Loc.equal dst cond_entry then pre_exit_loc else dst), lbl))
        else body_stmts
      in

      (loc_map', for_logic_stmts @ expr_intermediate_stmts @ body_stmts)
  | `Enha_for_stmt (_, _, _mods, _type, (`Id (_, _var), Some _dims), _, _exp, _, _body) ->
      unimplemented "`Enha_for_stmt var with dims" (loc_map, [])
  | `Enha_for_stmt _ -> unimplemented "`Enha_for_stmt alt form" (loc_map, [])
  | `Exp_stmt (e, _) ->
      let _value_of_e, (intermediate_loc, intermediate_stmts) =
        expr ~exit_loc:exit ~curr_loc:entry ~exc e
      in
      if Loc.equal intermediate_loc exit then (loc_map, intermediate_stmts)
      else (loc_map, (intermediate_loc, exit, Stmt.Skip) :: intermediate_stmts)
  | `For_stmt ((_, _, _, _, _, _, _, body) as f) ->
      let body_entry = Loc.fresh () in
      let body_exit = Loc.fresh () in
      let loc_map, header, _ =
        for_loop_header method_id ~body_entry ~body_exit ~entry ~exit ~ret ~exc loc_map f
      in
      let cont =
        ( Some body_exit,
          match cont_label with
          | None -> snd cont
          | Some label -> String.Map.set (snd cont) ~key:label ~data:body_exit )
      in
      let loc_map, body =
        edge_list_of_stmt method_id loc_map body_entry body_exit ret exc
          ~brk:(Some exit, snd brk)
          ~cont body
      in
      (* add an assume-false edge to body exit if there's no other control flow, to avoid malformed result *)
      let body =
        match List.count body ~f:(snd3 >> Loc.equal body_exit) with
        | 0 -> (body_entry, body_exit, Stmt.Assume (Expr.Lit (Lit.Bool false))) :: body
        | _ -> body
      in
      (* add a dummy location and skip edge if there are multiple back edges to cond_entry *)
      let body =
        if List.count body ~f:(snd3 >> Loc.equal body_exit) > 1 then
          let pre_exit_loc = Loc.fresh () in
          (pre_exit_loc, body_exit, Stmt.Skip)
          :: List.map body ~f:(fun (src, dst, lbl) ->
                 (src, (if Loc.equal dst body_exit then pre_exit_loc else dst), lbl))
        else body
      in

      (loc_map, header @ body)
  | `If_stmt (_, (_, cond, _), t_branch, f_branch_opt) ->
      let t_branch_entry = Loc.fresh () in
      let cond, (entry, cond_intermediate_stmts) = expr ~curr_loc:entry ~exc cond in
      let cond_neg = Expr.unop Unop.Not cond in
      let loc_map, t_branch =
        edge_list_of_stmt method_id loc_map t_branch_entry exit ret exc ~brk ~cont t_branch
        |> fun (loc_map, edges) -> (loc_map, (entry, t_branch_entry, Stmt.Assume cond) :: edges)
      in
      let loc_map, f_branch =
        match f_branch_opt with
        | None -> (loc_map, [ (entry, exit, Stmt.Assume cond_neg) ])
        | Some (_, f_branch) ->
            let f_branch_entry = Loc.fresh () in
            edge_list_of_stmt method_id loc_map f_branch_entry exit ret exc ~brk ~cont f_branch
            |> fun (loc_map, edges) ->
            (loc_map, (entry, f_branch_entry, Stmt.Assume cond_neg) :: edges)
      in
      cond_intermediate_stmts @ t_branch @ f_branch |> pair loc_map
  | `Labe_stmt ((_, label), _, s) ->
      edge_list_of_stmt method_id loc_map entry exit ret exc
        ~brk:(fst brk, String.Map.set (snd brk) ~key:label ~data:exit)
        ~cont ~cont_label:label s
  | `Local_var_decl (_, _, (v, vs), _) ->
      let decls = v :: List.map ~f:snd vs in
      (* Loc.t * edge list component of return is for intermediate stmts as described in [expr] documentation above *)
      let stmt_of_decl curr_loc : CST.variable_declarator -> Stmt.t * (Loc.t * edge list) = function
        | _, None -> (Stmt.Skip, (curr_loc, []))
        | (`Id (_, lhs), _), Some (_, `Exp e) ->
            let rhs, intermediate_stmts = expr ~curr_loc ~exc e in
            (Stmt.Assign { lhs; rhs }, intermediate_stmts)
        | (`Id (_, lhs), _), Some (_, `Array_init lit) ->
            let rhs, intermediate_stmts = array_lit ~curr_loc ~exc lit in
            (Stmt.Assign { lhs; rhs }, intermediate_stmts)
        | (`Choice_open _, _), _ -> (Stmt.Skip, (curr_loc, []))
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
            let rhs, (entry, intermediates) = expr ~curr_loc:entry ~exc e in
            (Stmt.Assign { lhs = retvar; rhs }, (entry, intermediates))
      in
      (entry, ret, stmt) :: intermediates |> pair loc_map
  | `SEMI _ -> (loc_map, [ (entry, exit, Stmt.skip) ])
  | `Switch_exp (_, (_, matching_exp, _), (_, cases_block, _)) -> (
      match cases_block with
      | `Rep_switch_blk_stmt_group cases ->
          let match_var, (switch_head, intermediate_stmts) =
            expr_as_var ~curr_loc:entry ~exc matching_exp
          in
          let first_block_head = Loc.fresh () in
          (* Strings are checked for equality using .equals, everything else is checked using == *)
          (* Currently assuming that all expressions are *NOT* strings. *)
          (* TODO: handle strings properly *)
          let check_expr exp = Expr.Binop { l = Expr.Var match_var; op = Binop.Eq; r = exp } in
          let get_non_default_label_expressions labels =
            List.bind labels
              ~f:
                (fst >> function
                 | `Case_exp_rep_COMMA_exp (_, exp, extra_exps) -> exp :: List.map extra_exps ~f:snd
                 | `Defa _ -> [])
          in
          let matches_case_expr entry_loc label_exprs =
            let ir_exprs, exit_loc, intermediate_stmts =
              List.fold label_exprs ~init:([], entry_loc, [])
                ~f:(fun (exprs, entry, intermediate_stmts) java_expr ->
                  let ir_expr, (exit, new_stmts) = expr ~curr_loc:entry ~exc java_expr in
                  (check_expr ir_expr :: exprs, exit, new_stmts @ intermediate_stmts))
            in
            let expr_opt =
              List.reduce ir_exprs ~f:(fun e1 e2 -> Expr.Binop { l = e1; op = Binop.Or; r = e2 })
            in
            ( Option.value expr_opt ~default:(Expr.Lit (Lit.Bool false)),
              exit_loc,
              intermediate_stmts )
          in
          let matches_default_expr entry_loc =
            let matches_some_case_expr, exit_loc, intermediate_stmts =
              List.bind cases ~f:(fst >> get_non_default_label_expressions)
              |> matches_case_expr entry_loc
            in
            (Expr.Unop { op = Unop.Not; e = matches_some_case_expr }, exit_loc, intermediate_stmts)
          in
          let create_labels_expr labels =
            let matches_some_case_expr, exit_loc, intermediate_stmts =
              matches_case_expr switch_head @@ get_non_default_label_expressions labels
            in
            match
              List.find_map labels ~f:(function
                | `Defa _, _ -> Some (matches_default_expr exit_loc)
                | _ -> None)
            with
            | None -> (matches_some_case_expr, exit_loc, intermediate_stmts)
            | Some (matches_default_expr, exit_loc', intermediate_stmts') ->
                ( Expr.Binop { l = matches_some_case_expr; op = Binop.Or; r = matches_default_expr },
                  exit_loc',
                  intermediate_stmts @ intermediate_stmts' )
          in
          let rec edge_list_of_cases block_head lmap cases =
            match cases with
            | [] -> (lmap, [])
            | [ (labels, block) ] ->
                let match_expr, case_tail, intermediate_stmts = create_labels_expr labels in
                let lmap', edges' =
                  edge_list_of_stmt_list method_id lmap ~entry:block_head ~exit ~ret ~exc
                    ~brk:(Some exit, snd brk)
                    ~cont block
                in
                ( lmap',
                  ((case_tail, block_head, Stmt.Assume match_expr) :: intermediate_stmts) @ edges'
                )
            | (labels, block) :: cases ->
                let block_tail = Loc.fresh () in
                let match_expr, case_tail, intermediate_stmts = create_labels_expr labels in
                let lmap', edges' =
                  edge_list_of_stmt_list method_id lmap ~entry:block_head ~exit:block_tail ~ret ~exc
                    ~brk:(Some exit, snd brk)
                    ~cont block
                in
                let lmap'', edges'' = edge_list_of_cases block_tail lmap' cases in
                ( lmap'',
                  ((case_tail, block_head, Stmt.Assume match_expr) :: intermediate_stmts)
                  @ edges' @ edges'' )
          in
          let loc_map, body_edges = edge_list_of_cases first_block_head loc_map cases in
          if List.exists (List.bind cases ~f:fst) ~f:(function `Defa _, _ -> true | _ -> false)
          then (loc_map, intermediate_stmts @ body_edges)
          else
            let matches_default_expr, exit_loc, intermediate_stmts' =
              matches_default_expr switch_head
            in
            ( loc_map,
              ((exit_loc, exit, Stmt.Assume matches_default_expr) :: intermediate_stmts')
              @ intermediate_stmts @ body_edges )
      | `Rep_switch_rule _cases -> unimplemented "`Rep_switch_rule`" (loc_map, []))
  | `Sync_stmt (_, _, (_, body, _)) ->
      edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret ~exc ~brk ~cont body
  | `Throw_stmt (_, e, _) ->
      let thrown_expr, (intermediate_loc, intermediate_stmts) = expr ~curr_loc:entry ~exc e in
      let throw_edge =
        (intermediate_loc, exc, Stmt.Assign { lhs = exc_retvar; rhs = thrown_expr })
      in
      (loc_map, throw_edge :: intermediate_stmts)
      (* try statements with finally disrupt the flow of continues and breaks *)
  | `Try_stmt (_, (_, try_block, _), `Rep1_catch_clause catch_clauses) ->
      let loc_map, catch_loc, catch_edges =
        build_catch_cfg catch_clauses loc_map method_id ~exit ~ret ~exc ~brk ~cont
      in
      let loc_map, try_edges =
        edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret ~exc:catch_loc ~brk ~cont
          try_block
      in
      (loc_map, try_edges @ catch_edges)
  | `Try_stmt
      (_, (_, try_block, _), `Rep_catch_clause_fina_clause (catch_clauses, (_, (_, f_blk, _)))) ->
      let f_blk_entry = Loc.fresh () in
      let f_blk_exit = Loc.fresh () in
      let loc_map, catch_loc, catch_edges =
        build_catch_cfg catch_clauses loc_map method_id ~exit:f_blk_entry ~ret ~exc:f_blk_entry
          ~brk:(None, String.Map.empty) ~cont:(None, String.Map.empty)
      in
      let has_catch_return, catch_edges =
        let catch_return_edges, catch_edges =
          List.partition_tf catch_edges ~f:(snd3 >> Loc.equal ret)
        in
        match catch_return_edges with
        | [] -> (false, catch_edges)
        | c_r_edges ->
            ( true,
              List.map c_r_edges ~f:(fun (src, _, lbl) -> (src, f_blk_entry, lbl)) @ catch_edges )
      in
      let loc_map, try_edges =
        edge_list_of_stmt_list method_id loc_map ~entry ~exit:f_blk_entry ~ret ~exc:catch_loc
          try_block
      in
      let has_try_return, try_edges =
        let try_return_edges, try_edges = List.partition_tf try_edges ~f:(snd3 >> Loc.equal ret) in
        match try_return_edges with
        | [] -> (false, try_edges)
        | t_r_edges ->
            (true, List.map t_r_edges ~f:(fun (src, _, lbl) -> (src, f_blk_entry, lbl)) @ try_edges)
      in
      let loc_map, finally_edges =
        edge_list_of_stmt_list method_id loc_map ~entry:f_blk_entry ~exit:f_blk_exit ~ret ~exc ~brk
          ~cont f_blk
      in
      let f_blk_egress_edges =
        let has_uncaught_exc : edge list -> bool =
          List.exists ~f:(function
            | _, dst, Stmt.Assign { lhs; rhs = _ } -> Loc.equal dst f_blk_entry && lhs = exc_retvar
            | _ -> false)
        in
        ( [ (f_blk_exit, exit, Stmt.Skip) ] |> fun es ->
          if has_uncaught_exc try_edges || has_uncaught_exc catch_edges then
            (f_blk_exit, exc, Stmt.Skip) :: es
          else es )
        |> fun es ->
        if has_try_return || has_catch_return then (f_blk_exit, ret, Stmt.Skip) :: es else es
      in
      (loc_map, List.concat [ f_blk_egress_edges; try_edges; catch_edges; finally_edges ])
  | `Try_with_resous_stmt (_, resource_spec, (_, blk, _), catches, opt_finally) ->
      let rs = match resource_spec with _, r, rs, _, _ -> r :: List.map ~f:snd rs in
      let try_entry_loc, resource_decl_edges, resource_vars =
        (* NOTE: resource_exprs are in reverse order here, processed in reverse below *)
        List.fold rs ~init:(entry, [], []) ~f:(fun (curr_loc, edges, vars) -> function
          | `Field_access _ as fa ->
              let var, (curr_loc, new_edges) = expr_as_var ~curr_loc ~exc (`Prim_exp fa) in
              (curr_loc, new_edges @ edges, var :: vars)
          | `Id (_, ident) -> (curr_loc, edges, ident :: vars)
          | `Opt_modifs_unan_type_var_decl_id_EQ_exp (_, _, vdi, _, rhs) ->
              let lhs = ident_of_var_declarator_id vdi in
              let rhs, (curr_loc, rhs_edges) = expr ~curr_loc ~exc rhs in
              let next_loc = Loc.fresh () in
              let binding = (curr_loc, next_loc, Stmt.Assign { lhs; rhs }) in
              (next_loc, (binding :: rhs_edges) @ edges, lhs :: vars))
      in
      let close_entry = Loc.fresh () in
      let f_blk_entry, f_blk_exit, (loc_map, finally_edges) =
        (match opt_finally with
        | Some (_, (_, f_blk, _)) ->
            let f_blk_entry = Loc.fresh () in
            let f_blk_exit = Loc.fresh () in
            ( f_blk_entry,
              f_blk_exit,
              edge_list_of_stmt_list method_id loc_map f_blk ~entry:f_blk_entry ~exit:f_blk_exit
                ~ret ~exc ~brk ~cont )
        | None -> (exit, exit, (loc_map, [])))
        |> fun init ->
        List.fold resource_vars ~init ~f:(fun (entry, exit, (lm, edges)) resource ->
            let new_entry = Loc.fresh () in
            let close_resource_edge =
              ( new_entry,
                entry,
                Stmt.Call
                  { lhs = None; rcvr = resource; meth = "close"; actuals = []; alloc_site = None }
              )
            in
            (new_entry, exit, (lm, close_resource_edge :: edges)))
      in
      let loc_map, catch_loc, catch_edges =
        let brk, cont =
          if Option.is_some opt_finally then ((None, String.Map.empty), (None, String.Map.empty))
          else (brk, cont)
        in
        build_catch_cfg catches loc_map method_id ~exit:f_blk_entry ~ret ~exc:f_blk_entry ~brk ~cont
      in
      let loc_map, try_edges =
        edge_list_of_stmt_list method_id loc_map ~entry:try_entry_loc ~exit:close_entry
          ~exc:catch_loc ~ret:close_entry blk
      in
      let f_blk_egress_edges =
        let has_uncaught_exc : edge list -> bool =
          List.exists ~f:(function
            | _, dst, Stmt.Assign { lhs; rhs = _ } -> Loc.equal dst f_blk_entry && lhs = exc_retvar
            | _ -> false)
        in
        if Option.is_some opt_finally && (has_uncaught_exc try_edges || has_uncaught_exc catch_edges)
        then [ (f_blk_exit, exit, Stmt.Skip); (f_blk_exit, exc, Stmt.Skip) ]
        else if Option.is_some opt_finally then [ (f_blk_exit, exit, Stmt.Skip) ]
        else []
      in
      ( loc_map,
        List.concat
          [ resource_decl_edges; try_edges; catch_edges; finally_edges; f_blk_egress_edges ] )
  | `While_stmt (_, (_, cond, _), body) ->
      let body_entry = Loc.fresh () in
      let cond, (intermediate_loc, cond_intermediates) = expr ~curr_loc:entry ~exc cond in
      let cond_neg = Expr.unop Unop.Not cond in
      let cont =
        ( Some entry,
          match cont_label with
          | None -> snd cont
          | Some label -> String.Map.set (snd cont) ~key:label ~data:entry )
      in
      let loc_map, body =
        edge_list_of_stmt method_id loc_map body_entry entry ret exc
          ~brk:(Some exit, snd brk)
          ~cont body
      in
      let body =
        if List.count body ~f:(snd3 >> Loc.equal entry) > 1 then
          let pre_exit_loc = Loc.fresh () in
          (pre_exit_loc, entry, Stmt.Skip)
          :: List.map body ~f:(fun (src, dst, lbl) ->
                 (src, (if Loc.equal dst entry then pre_exit_loc else dst), lbl))
        else body
      in
      (intermediate_loc, body_entry, Stmt.Assume cond)
      :: (intermediate_loc, exit, Stmt.Assume cond_neg)
      :: body
      |> List.append cond_intermediates |> pair loc_map
  | `Yield_stmt _ -> unimplemented "`Yield_stmt" (loc_map, [])

and build_catch_cfg catch_clauses loc_map method_id ~exit ~ret ~exc ~brk ~cont :
    Loc_map.t * Loc.t * edge list =
  let open Ast in
  let condition_of_catch_clause = function
    | _, _, (_, (ct, cts), _), _, _ ->
        let caught_types =
          string_of_unannotated_type ct :: List.map cts ~f:(snd >> string_of_unannotated_type)
        in
        let exc_retvar_has_type t =
          Expr.binop
            (Expr.unop Unop.Typeof (Expr.Var exc_retvar))
            Binop.Eq (Expr.Lit (Lit.String t))
        in
        List.map caught_types ~f:exc_retvar_has_type
        |> List.reduce_exn ~f:(flip Expr.binop Binop.Or)
  in
  let exn_binding_of_catch_clause : CST.catch_clause -> Stmt.t = function
    | _, _, (_, _, (`Id (_, ident), _)), _, _ ->
        Stmt.Assign { lhs = ident; rhs = Expr.Var exc_retvar }
    | _ -> unimplemented "catch-clause-with-non-ident-formal-param" Stmt.Skip
  in
  let body_of_catch_clause : CST.catch_clause -> CST.program = function
    | _, _, _, _, (_lcurly, body, _rcurly) -> body
  in
  if List.is_empty catch_clauses then (loc_map, exc, [])
  else
    let catch_loc = Loc.fresh () in
    let rec build_catch_cfg_impl curr_loc loc_map = function
      | [] ->
          (* only reachable when there are 0 catch_clauses, precluded by preceding [List.is_empty] check *)
          failwith "unreachable"
      | cc :: ccs ->
          let block_entry_loc = Loc.fresh () in
          let cond = condition_of_catch_clause cc in
          let cond_is_match = Stmt.Assume cond in
          let cond_is_not_match = Stmt.Assume (Expr.unop Unop.Not cond) in
          let exn_binding_loc = Loc.fresh () in
          let loc_map, block_edges =
            edge_list_of_stmt_list method_id loc_map (body_of_catch_clause cc)
              ~entry:block_entry_loc ~exit ~ret ~exc ~brk ~cont
          in
          let next_loc, (loc_map, ccs_edges) =
            match ccs with
            | [] ->
                (* this is the last catch clause, so wire the non-matching case out to the exceptional exit and make no recursive call *)
                (exc, (loc_map, []))
            | _ ->
                (* this is _not_ the last catch clause, so wire the non-matching case out to a new location and keep building catch blocks from there *)
                let next_loc = Loc.fresh () in
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

and edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret ~exc
    ?(brk = (None, String.Map.empty)) ?(cont = (None, String.Map.empty)) stmts :
    Loc_map.t * edge list =
  let rec edges_of_stmts loc_map curr_loc = function
    | [] -> (loc_map, [ (curr_loc, exit, Ast.Stmt.Skip) ])
    | [ s ] -> edge_list_of_stmt method_id loc_map curr_loc exit ret exc ~brk ~cont s
    | s :: ss ->
        let next_loc = Loc.fresh () in
        let loc_map, s_edges =
          edge_list_of_stmt method_id loc_map curr_loc next_loc ret exc ~brk ~cont s
        in
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
            let l = Loc.fresh () in
            (* Can you break/continue in a loop header? and if so, where should that go? the end of the for loop or the pre-existing break target? *)
            (* TODO: update the None below, and possibly the signiture of this function as well *)
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
        | `Formal_param (_, _, var_decl_id) ->
            let var_decl = (var_decl_id, None) in
            ident_of_var_declarator var_decl
        | `Spread_param (_, _, _, var_decl) -> ident_of_var_declarator var_decl
      in
      formal_name first :: List.map rest ~f:(snd >> formal_name)
  | _, _, None, _ -> []

let types_of_formals = function
  | _open_paren, _rcvr_param, Some (first, rest), _close_paren ->
      let type_of_formal = function
        | `Formal_param (_, t, _) -> string_of_unannotated_type t
        | `Spread_param (_, t, _, _) -> string_of_unannotated_type t ^ "[]"
      in
      type_of_formal first :: List.map rest ~f:(snd >> type_of_formal)
  | _, _, None, _ -> []

let of_method_decl loc_map ?(package = []) ~class_name (md : CST.method_declaration) =
  match md with
  | modifiers, (_tparams, _type, (`Id (_, method_name), formals, _), _throws), `Blk (_, stmts, _) ->
      let entry = Loc.fresh () in
      let exit = Loc.fresh () in
      let exc_exit = Loc.fresh () in
      let arg_types = types_of_formals formals in
      let static =
        Option.exists modifiers ~f:(List.exists ~f:(function `Static _ -> true | _ -> false))
      in
      let formals = parse_formals formals in
      let locals = declarations stmts in
      let method_id : Method_id.t = { package; class_name; method_name; static; arg_types } in
      let fn : Fn.t = { method_id; formals; locals; entry; exit; exc_exit } in
      let loc_map, edges =
        edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret:exit ~exc:exc_exit stmts
      in
      Some (loc_map, edges, fn)
  | _, (_, _, (`Choice_open _, _, _), _), _ -> None
  | _, _, `SEMI _ -> None

let of_constructor_decl loc_map ?(package = []) ~class_name ~instance_init ~cha cd body =
  match (cd, body) with
  | (_tparams, _, formals), (_, explicit_constructor_invo, stmts, _) ->
      let entry = Loc.fresh () in
      let exit = Loc.fresh () in
      let exc_exit = Loc.fresh () in
      let arg_types = types_of_formals formals in
      let formals = parse_formals formals in
      let locals = declarations stmts in
      let method_id : Method_id.t =
        { package; class_name; method_name = "<init>"; static = false; arg_types }
      in
      let fn : Fn.t = { method_id; formals; locals; entry; exit; exc_exit } in
      let loc_map, edges =
        match explicit_constructor_invo with
        | None ->
            (* prepend instance initializer block to constructor body, if one is given *)
            let stmts = Option.fold instance_init ~init:stmts ~f:(flip ( @ )) in
            edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret:exit ~exc:exc_exit stmts
        | Some (`Opt_type_args_choice_this (_typargs, rcvr), args, _semi) ->
            let args = match args with _, Some (e, es), _ -> e :: List.map ~f:snd es | _ -> [] in
            let actuals, (pre_invocation_loc, pre_invocation_edges) =
              List.fold args
                ~init:([], (entry, []))
                ~f:(fun (acc_exprs, (curr_loc, acc_intermediates)) arg ->
                  let arg_expr, (curr_loc, arg_intermediates) = expr ~curr_loc ~exc:exc_exit arg in
                  (acc_exprs @ [ arg_expr ], (curr_loc, acc_intermediates @ arg_intermediates)))
            in
            let post_invocation_loc = Loc.fresh () in
            let loc_map, body_edges =
              edge_list_of_stmt_list method_id loc_map ~entry:post_invocation_loc ~exit ~ret:exit
                ~exc:exc_exit stmts
            in
            let rcvr =
              match rcvr with
              | `This _ -> class_name
              | `Super _ -> (
                  match Class_hierarchy.get_superclass_name ~package ~class_name cha with
                  | Some s -> s
                  | None -> "super")
              (* if no superclass name exists in [cha] then we don't have code for the superclass' constructor, so just emit this placeholder *)
            in
            let invocation =
              ( pre_invocation_loc,
                post_invocation_loc,
                Ast.Stmt.Call
                  {
                    lhs = Some "this";
                    rcvr;
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
      in
      (loc_map, edges, fn)

let of_static_init loc_map ?(package = []) ~class_name (_, block, _) =
  let entry = Loc.fresh () in
  let exit = Loc.fresh () in
  let exc_exit = Loc.fresh () in
  let method_id : Method_id.t =
    { package; class_name; method_name = "<staticinit>"; static = true; arg_types = [] }
  in
  let fn : Fn.t = { method_id; formals = []; locals = declarations block; entry; exit; exc_exit } in
  let loc_map, edges =
    edge_list_of_stmt_list method_id loc_map ~entry ~exit ~ret:exit ~exc:exc_exit block
  in
  (loc_map, edges, fn)

let rec parse_class_decl ?(package = []) ?(containing_class_name = None) ~imports
    ~(acc : prgm_parse_result) : CST.class_declaration -> prgm_parse_result = function
  | _modifiers, _, (_, class_name), _, _, _, (_, decls, _) ->
      let class_name =
        match containing_class_name with Some n -> n ^ "$" ^ class_name | None -> class_name
      in
      let instance_init =
        List.find_map decls ~f:(function `Blk (_, b, _) -> Some b | _ -> None)
      in
      List.fold decls ~init:acc ~f:(fun acc -> function
        | `Meth_decl md -> (
            match of_method_decl acc.loc_map ~package ~class_name md with
            | Some (loc_map, edges, fn) ->
                set_parse_result ~cfgs:(add_fn fn ~edges acc.cfgs) ~loc_map acc
            | None -> acc)
        | `Class_decl cd ->
            parse_class_decl cd ~package ~containing_class_name:(Some class_name) ~imports ~acc
        | `Cons_decl (_, cd, _, body) ->
            let loc_map, edges, fn =
              of_constructor_decl acc.loc_map ~package ~class_name ~instance_init ~cha:acc.cha cd
                body
            in
            set_parse_result ~cfgs:(add_fn fn ~edges acc.cfgs) ~loc_map acc
        | `Field_decl _ | `Inte_decl _ | `Anno_type_decl _ | `SEMI _ | `Blk _ | `Enum_decl _ -> acc
        | `Static_init (_, block) ->
            let loc_map, edges, fn = of_static_init acc.loc_map ~package ~class_name block in
            set_parse_result ~cfgs:(add_fn fn ~edges acc.cfgs) ~loc_map acc
        | `Record_decl _ -> unimplemented "`Record_decl" acc)

let package_of_cst cst =
  List.find_map cst ~f:(function `Decl (`Pack_decl (_, _, name, _)) -> Some name | _ -> None)
  |> function
  | None -> []
  | Some n ->
      let rec list_of_name = function
        | `Id (_, ident) -> [ ident ]
        | `Choice_open _ -> unimplemented "`Choice_open in package decl" []
        | `Scoped_id (nm, _dot, (_, ident)) -> list_of_name nm @ [ ident ]
      in
      list_of_name n

let imports_of_cst ?package cst : string list String.Map.t =
  let package = match package with None -> package_of_cst cst | Some p -> p in
  List.fold ~init:String.Map.empty cst ~f:(fun acc -> function
    | `Decl (`Import_decl (_, _, nm, None, _)) -> (
        let rec quals = function
          | `Id (_, ident) -> [ ident ]
          | `Scoped_id (prefix, _, (_, ident)) -> ident :: quals prefix
          | `Choice_open _ -> []
        in
        match nm with
        | `Id (_, ident) -> Map.set acc ~key:ident ~data:[]
        | `Scoped_id (q, _, (_, ident)) -> Map.set acc ~key:ident ~data:(List.rev (quals q))
        | `Choice_open _ -> acc)
    | `Decl (`Class_decl (_, _, (_, class_name), _, _, _, _)) ->
        Map.set acc ~key:class_name ~data:package
    | _ -> acc)

let cha_and_fields_of_cst ?(acc_cha = Class_hierarchy.empty) ?(acc_fields = Declared_fields.empty)
    ?(containing_class = None) cst =
  let package = package_of_cst cst in
  let imports = imports_of_cst ~package cst in
  let rec impl init containing_class cst =
    List.fold cst ~init ~f:(fun (acc_cha, acc_fields) -> function
      | `Decl (`Class_decl (_, _, (_, name), _, superclass, _, (_, body, _))) ->
          let class_name = match containing_class with Some n -> n ^ "$" ^ name | None -> name in
          let cha =
            match superclass with
            | None -> acc_cha
            | Some (_, typ) -> (
                let superclass_name = string_of_type typ in
                match Map.find imports superclass_name with
                | Some super_package ->
                    Class_hierarchy.add ~package ~class_name ~super_package ~superclass_name acc_cha
                | None -> acc_cha)
          in
          let fields =
            let static, instance =
              List.fold body ~init:([], []) ~f:(fun (static, instance) -> function
                | `Field_decl (mods, _, (v, vs), _) ->
                    let decls = List.(map ~f:ident_of_var_declarator (v :: map ~f:snd vs)) in
                    let is_static =
                      Option.exists mods
                        ~f:(List.exists ~f:(function `Static _ -> true | _ -> false))
                    in
                    if is_static then (decls @ static, instance) else (static, decls @ instance)
                | _ -> (static, instance))
              |> fun (s, i) -> String.Set.(of_list s, of_list i)
            in
            Declared_fields.add ~package ~class_name ~fields:{ static; instance } acc_fields
          in
          List.fold body ~init:(cha, fields) ~f:(fun acc -> function
            | `Class_decl _ as cd -> impl acc (Some class_name) [ `Decl cd ] | _ -> acc)
      | _ -> (acc_cha, acc_fields))
  in
  impl (acc_cha, acc_fields) containing_class cst |> fun (cha, fields) ->
  (cha, Class_hierarchy.compute_closure ~cha ~fields)

let of_java_cst ?(acc = empty_parse_result) (cst : CST.program) : prgm_parse_result =
  let package = package_of_cst cst in
  let imports = imports_of_cst ~package cst in
  let cha, fields = cha_and_fields_of_cst ~acc_cha:acc.cha ~acc_fields:acc.fields cst in
  let acc = set_parse_result ~cha ~fields acc in
  List.fold cst ~init:acc ~f:(fun acc -> function
    | `Decl (`Class_decl cd) -> parse_class_decl cd ~package ~acc ~imports
    | `Decl (`Anno_type_decl _)
    | `SEMI _
    | `Decl (`Enum_decl _)
    | `Decl (`Import_decl _)
    | `Decl (`Inte_decl _)
    | `Decl (`Pack_decl _) ->
        acc
    | `Decl (`Module_decl _) -> unimplemented "`Module_decl" acc
    | stmt ->
        let rec first_atom = function
          | Sexp.Atom a -> a
          | List [] -> "empty-sexp-list"
          | Sexp.List (l :: _) -> first_atom l
        in
        unimplemented (first_atom (CST.sexp_of_statement stmt) ^ "-at-top-level") acc)

let parse_file_exn ?(acc = empty_parse_result) filename =
  let file = Src_file.of_file filename in
  let tree = Result.bind (Tree.parse ~old_tree:None ~file) ~f:(Tree.as_java_cst file) in
  match tree with
  | Ok tree -> of_java_cst ~acc tree
  | Error _e -> failwith @@ "parse error in " ^ filename

let parse_tree_exn ?(acc = empty_parse_result) filename tree =
  let file = Src_file.of_file filename in
  let cst =
    match Tree.as_java_cst file tree with
    | Ok cst -> cst
    | _ -> failwith ("parse error in " ^ filename)
  in
  of_java_cst ~acc cst

let parse_trees_exn ~trees =
  List.fold trees ~init:empty_parse_result ~f:(fun acc (file, tree) ->
      parse_tree_exn ~acc file tree)

open Result.Monad_infix

let parse_files_exn ~files =
  List.fold files ~init:empty_parse_result ~f:(fun acc file -> parse_file_exn ~acc file)

let%test "hello world program" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/HelloWorld.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst |> Result.is_ok

let%test "nested classes" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/NestedClasses.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst |> Result.is_ok

let%test "nested loops" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/NestedLoops.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> () | Ok { cfgs; _ } -> dump_dot_interproc ~filename:"nested_loops.dot" cfgs)
  |> Result.is_ok

let%test "constructors" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Constructors.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } -> dump_dot_interproc ~filename:(abs_of_rel_path "constructors.dot") cfgs)
  |> Result.is_ok

let%test "Cibai example" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/CibaiExample/MiniBag.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } -> dump_dot_interproc ~filename:(abs_of_rel_path "cibai_example.dot") cfgs)
  |> Result.is_ok

let%test "exceptions, try, catch, finally" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Exceptions.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } -> dump_dot_interproc ~filename:(abs_of_rel_path "exceptions.dot") cfgs)
  |> Result.is_ok

let%test "Literals: various syntactic forms / types" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Literals.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } -> dump_dot_interproc ~filename:(abs_of_rel_path "literals.dot") cfgs)
  |> Result.is_ok

let%test "switch block statements" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Switch.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } -> dump_dot_interproc ~filename:(abs_of_rel_path "switch.dot") cfgs)
  |> Result.is_ok

let%test "Enhanced For loops" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/ForEach.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } -> dump_dot_interproc ~filename:(abs_of_rel_path "for_each.dot") cfgs)
  |> Result.is_ok

let%test "super method call" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/SuperMethodInvocation.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } -> dump_dot_interproc ~filename:(abs_of_rel_path "supercall.dot") cfgs)
  |> Result.is_ok

let%test "instance initializers" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/InstanceInitializer.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } -> dump_dot_interproc ~filename:(abs_of_rel_path "instance_init.dot") cfgs)
  |> Result.is_ok

let%test "labeled breaks" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/Break.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } -> dump_dot_interproc ~filename:(abs_of_rel_path "break.dot") cfgs)
  |> Result.is_ok

let%test "try-with-resources" =
  let file = Src_file.of_file @@ abs_of_rel_path "test_cases/java/TryWithResources.java" in
  Tree.parse ~old_tree:None ~file >>= Tree.as_java_cst file >>| of_java_cst
  $> (function
       | Error _ -> ()
       | Ok { cfgs; _ } ->
           dump_dot_interproc ~filename:(abs_of_rel_path "trywithresources.dot") cfgs)
  |> Result.is_ok
