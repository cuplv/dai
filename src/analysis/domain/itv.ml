(* Copyright (c) Benno Stein, 2020
 * me@bennostein.org
 * 
 * This source code is derived in part from the Interval domain of
 * Sledge (github.com/facebook/infer ./sledge directory), which is MIT Licensed.
 * As such, this source code is licensed under the same conditions:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.

 *)

open Dai
open Import
open Apron
open Option.Monad_infix
open Syntax

type t = Box.t Abstract1.t

let man = lazy (Box.manager_alloc ())

let get_man () = Lazy.force man

let combine_envs x y =
  let man = get_man () in
  let x = Abstract1.minimize_environment man x in
  let y = Abstract1.minimize_environment man y in
  let new_env = Environment.lce (Abstract1.env x) (Abstract1.env y) in
  pair
    (Abstract1.change_environment man x new_env false)
    (Abstract1.change_environment man y new_env false)

(* Do not eta-reduce!  Will break lazy manager allocation *)
let join l r =
  let l, r = combine_envs l r in
  Abstract1.join (get_man ()) l r

(* Do not eta-reduce!  Will break lazy manager allocation *)
let meet l r =
  let l, r = combine_envs l r in
  Abstract1.meet (get_man ()) l r

(* Do not eta-reduce! Will break lazy manager allocation *)
let widen l r =
  let l, r = combine_envs l r in
  let man = get_man () in
  (* check that [r] is included in [l] before widening (just returning [l] if not), since APRON's widening assumes that to be the case *)
  if Abstract1.is_leq man r l then l else Abstract1.widening man l r

(* Do not eta-reduce!  Will break lazy manager allocation *)
let equal l r =
  let l, r = combine_envs l r in
  Abstract1.is_eq (get_man ()) l r

(* Do not eta-reduce!  Will break lazy manager allocation *)
let is_bot itv = Abstract1.is_bottom (get_man ()) itv

(* Do not eta-reduce!  Will break lazy manager allocation *)
let implies l r =
  let l, r = combine_envs l r in
  Abstract1.is_leq (get_man ()) l r

let ( <= ) = implies

let bindings (itv : t) =
  let man = get_man () in
  let itv = Abstract1.minimize_environment man itv in
  let box = Abstract1.to_box man itv in
  let vars = Environment.vars box.box1_env |> uncurry Array.append in
  Array.zip_exn vars box.interval_array

let pp_interval fs (interval : Interval.t) =
  let truncate scalar =
    (* Mpqf.to_float/Mpfrf.to_float segfault on +/- infinity;
       use default [Scalar.to_string] in that case, yielding "-1/0" or "1/0"
       otherwise, convert to float and use [Float.to_string_hum] (i.e. to-human-readable-string)
    *)
    let is_infty = not @@ Int.equal 0 @@ Scalar.is_infty scalar in
    if is_infty then Scalar.to_string scalar
    else
      let as_float =
        match scalar with
        | Scalar.Float f -> f
        | Scalar.Mpqf m -> Mpqf.to_float m
        | Scalar.Mpfrf m -> Mpfrf.to_float m
      in
      Float.to_string_hum ~decimals:3 ~strip_zero:true as_float
  in
  Format.fprintf fs "[%s, %s]" (truncate interval.inf) (truncate interval.sup)

let pp fs itv =
  if is_bot itv then Format.fprintf fs "bottom"
  else
    bindings itv
    |>
    let pp_binding fs (v, i) = Format.fprintf fs "%a -> %a" Var.print v pp_interval i in
    Format.fprintf fs "{%a}" (Array.pp "; " pp_binding)

let sexp_of_t (itv : t) =
  let sexps =
    Array.fold (bindings itv) ~init:[] ~f:(fun acc (v, { inf; sup }) ->
        Sexp.List
          [
            Sexp.Atom (Var.to_string v);
            Sexp.Atom (Scalar.to_string inf);
            Sexp.Atom (Scalar.to_string sup);
          ]
        :: acc)
  in
  Sexp.List sexps

let t_of_sexp = function
  | Sexp.List sexps ->
      let constraint_of_sexp = function
        | Sexp.List [ Sexp.Atom v; Sexp.Atom inf; Sexp.Atom sup ] ->
            ( Var.of_string v,
              (Scalar.Float (Float.of_string inf), Scalar.Float (Float.of_string sup)) )
        | _ -> failwith "malformed interval sexp contents"
      in
      let vars, itvs =
        List.fold sexps ~init:([], []) ~f:(fun (v_acc, i_acc) sexp ->
            let v, (inf, sup) = constraint_of_sexp sexp in
            (v :: v_acc, Interval.of_infsup inf sup :: i_acc))
      in
      let vars = Array.of_list vars in
      let itvs = Array.of_list itvs in
      let env = Environment.make [||] vars in
      Abstract1.of_box (get_man ()) env vars itvs
  | _ -> failwith "malformed interval sexp"

let init () = Abstract1.top (get_man ()) (Environment.make [||] [||])

let bottom () = Abstract1.bottom (get_man ()) (Environment.make [||] [||])

(* given a boolean operation : [Tcons0.typ] and two operands, construct a Tcons1 encoding the constraint *)
let mk_tcons env op l r =
  (* add tiny constant to right hand side to avoid float comparison wonkiness*)
  let r =
    if op = Tcons0.SUP then
      Texpr1.Binop (Texpr1.Add, r, Texpr1.Cst (Coeff.s_of_float 1e-10), Texpr1.Double, Texpr1.Zero)
    else r
  in
  let l_minus_r = Texpr1.Binop (Texpr1.Sub, l, r, Texpr1.Double, Texpr1.Zero) in
  Tcons1.make (Texpr1.of_expr env l_minus_r) op

(* abstractly evaluate boolean binary operation [l op r] at interval [itv] by translating it to [(l - r) op 0]
   (since apron can only solve booleran constraints of that form), and intersecting the result with [itv].
   If that intersection is  ...
     ... bottom then expression is false
     ... equal to [itv] then expression is true
     ... anything else then the expression may be true or false
  Return that result as an apron interval constant: [0,0], [1,1], or [0,1] respectively.
*)
let mk_bool_binop itv op l r =
  let env = Abstract1.env itv in
  let tcons = mk_tcons env op l r in
  let tcons_array = Tcons1.array_make env 1 $> fun a -> Tcons1.array_set a 0 tcons in
  let intersection = Abstract1.meet_tcons_array (get_man ()) itv tcons_array in
  if is_bot intersection then Texpr1.Cst (Coeff.s_of_float 0.)
  else if equal intersection itv then Texpr1.Cst (Coeff.s_of_float 1.)
  else Texpr1.Cst (Coeff.i_of_float 0. 1.)

(* Convert a native AST expression into an APRON tree expression.
   [fallback] supports mutually recursive extensions, e.g. in [array_bounds.ml]
*)
let rec texpr_of_expr ?(fallback = fun _ _ -> None) itv =
  let open Ast in
  let mk_arith_binop op l r = Some Texpr1.(Binop (op, l, r, Double, Zero)) in
  let mk_bool_binop i op l r = Some (mk_bool_binop i op l r) in
  function
  | Expr.Var v -> Some (Texpr1.Var (Var.of_string v))
  | Expr.Lit (Int i) -> Some (Texpr1.Cst (Coeff.s_of_float (Float.of_int64 i)))
  | Expr.Lit (Float f) -> Some (Texpr1.Cst (Coeff.s_of_float f))
  | Expr.Lit (Bool b) -> Some (Texpr1.Cst (Coeff.s_of_float (if b then 1. else 0.)))
  | Expr.Lit _ -> None
  | Expr.Binop { l; op; r } -> (
      texpr_of_expr ~fallback itv l >>= fun l ->
      texpr_of_expr ~fallback itv r >>= fun r ->
      match op with
      | Plus -> mk_arith_binop Texpr1.Add l r
      | Minus -> mk_arith_binop Texpr1.Sub l r
      | Times -> mk_arith_binop Texpr1.Mul l r
      | Divided_by -> mk_arith_binop Texpr1.Div l r
      | Mod -> mk_arith_binop Texpr1.Mod l r
      | Eq -> mk_bool_binop itv Tcons0.EQ l r
      | NEq -> mk_bool_binop itv Tcons0.DISEQ l r
      | Gt -> mk_bool_binop itv Tcons0.SUP l r
      | Ge -> mk_bool_binop itv Tcons0.SUPEQ l r
      | Lt -> mk_bool_binop itv Tcons0.SUP r l
      | Le -> mk_bool_binop itv Tcons0.SUPEQ r l
      | BAnd | BOr | BXor ->
          (* sending bitwise arihmetic to top because APRON does not support it *)
          Some (Texpr1.Cst (Coeff.Interval Interval.top))
      | LShift ->
          (* IR expression `l >> r` becomes APRON expression `l * (2^r)` *)
          let two_to_the_r = Texpr1.(Binop (Pow, Cst (Coeff.s_of_int 2), r, Double, Zero)) in
          Some Texpr1.(Binop (Mul, l, two_to_the_r, Double, Zero))
      | URShift | RShift ->
          (* IR expression `l >> r` becomes APRON expression `l / (2^r)` *)
          (* todo: mess with sign for unsigned shift --- this isn't quite right for negative values of [l] *)
          let two_to_the_r = Texpr1.(Binop (Pow, Cst (Coeff.s_of_int 2), r, Double, Zero)) in
          Some Texpr1.(Binop (Div, l, two_to_the_r, Double, Zero))
      | Instanceof ->
          (* result of instanceof is a bool; from interval's perspective, either a 0 or 1 *)
          Some (Texpr1.Cst (Coeff.Interval (Interval.of_int 0 1)))
      | _ ->
          Format.fprintf Format.err_formatter "Binary op %a has no APRON equivalent\n" Binop.pp op;
          None )
  (* cancel out double-negations, arithmetic and boolean *)
  | Expr.Unop { op = Unop.Neg; e = Expr.Unop { op = Unop.Neg; e } }
  | Expr.Unop { op = Unop.Not; e = Expr.Unop { op = Unop.Not; e } } ->
      texpr_of_expr ~fallback itv e
  | Expr.Unop { op; e } -> (
      (* Translate to equivalent expressions for unops with no APRON equivalent (i.e. everything but [Neg]):
       * !e -> e==0
       * +e -> e
       * e++ -> e+1
       * e-- -> e-1
       *)
      texpr_of_expr ~fallback itv e
      >>= fun e ->
      match op with
      | Unop.Neg -> Some Texpr1.(Unop (Neg, e, Double, Zero))
      | Unop.Not -> mk_bool_binop itv Tcons0.EQ e (Texpr1.Cst (Coeff.s_of_float 0.))
      | Unop.Plus -> Some e
      | Unop.Incr -> mk_arith_binop Texpr1.Add e (Texpr1.Cst (Coeff.s_of_float 1.))
      | Unop.Decr -> mk_arith_binop Texpr1.Sub e (Texpr1.Cst (Coeff.s_of_float 1.))
      | Unop.Typeof | Unop.BNot -> None )
  | expr -> fallback itv expr

let rec meet_with_constraint ?(fallback = fun _ _ -> None) itv =
  let open Ast.Expr in
  let man = get_man () in
  let meet_with_op itv op l r =
    texpr_of_expr ~fallback itv l
    >>= (fun l ->
          texpr_of_expr ~fallback itv r >>| fun r ->
          let tcons = mk_tcons (Abstract1.env itv) op l r in
          let tcons_array =
            Tcons1.array_make (Abstract1.env itv) 1 $> fun a -> Tcons1.array_set a 0 tcons
          in
          Abstract1.meet_tcons_array man itv tcons_array)
    |> Option.value ~default:itv
  in
  let flip_binop =
    let open Ast.Binop in
    function
    | And -> Some Or
    | Or -> Some And
    | Eq -> Some NEq
    | NEq -> Some Eq
    | Gt -> Some Le
    | Lt -> Some Ge
    | Ge -> Some Lt
    | Le -> Some Gt
    | _ -> None
  in
  function
  | Unop { op = Not; e = Binop { l; op; r } } when Option.is_some (flip_binop op) ->
      (* flip equalities/disequalities/inequalities, and apply demorgan's where possible *)
      let flipped_op = flip_binop op |> fun x -> Option.value_exn x in
      let new_l = if op = And || op = Or then Unop { op = Not; e = l } else l in
      let new_r = if op = And || op = Or then Unop { op = Not; e = r } else r in
      meet_with_constraint ~fallback itv (Binop { l = new_l; op = flipped_op; r = new_r })
  | Binop { l; op = And; r } ->
      let l = meet_with_constraint ~fallback itv l in
      let r = meet_with_constraint ~fallback itv r in
      Abstract1.meet man l r
  | Binop { l; op = Or; r } ->
      let l = meet_with_constraint ~fallback itv l in
      let r = meet_with_constraint ~fallback itv r in
      Abstract1.join man l r
  | Binop { l; op = Eq; r } -> meet_with_op itv Tcons0.EQ l r
  | Binop { l; op = NEq; r } -> meet_with_op itv Tcons0.DISEQ l r
  | Binop { l; op = Gt; r } -> meet_with_op itv Tcons0.SUP l r
  | Binop { l; op = Ge; r } -> meet_with_op itv Tcons0.SUPEQ l r
  | Binop { l; op = Lt; r } -> meet_with_op itv Tcons0.SUP r l
  | Binop { l; op = Le; r } -> meet_with_op itv Tcons0.SUPEQ r l
  | Unop { op = Not; e } -> meet_with_op itv Tcons0.EQ e (Lit (Int 0L))
  | _ -> itv

let eval_texpr itv =
  (fun e ->
    try Texpr1.of_expr (Abstract1.env itv) e
    with _ ->
      failwith (Format.asprintf "error in Texpr1.of_expr; expr = [%a]\n" Texpr1.print_expr e))
  >> Abstract1.bound_texpr (get_man ()) itv

let extend_env_by_uses stmt itv =
  let env = Abstract1.env itv in
  let man = get_man () in
  Ast.Stmt.uses stmt
  |> Set.filter ~f:(Var.of_string >> Environment.mem_var env >> not)
  |> Set.to_array |> Array.map ~f:Var.of_string
  |> Environment.add env [||]
  |> fun new_env -> Abstract1.change_environment man itv new_env false

let filter_env (itv : t) ~(f : string -> bool) =
  let env = Abstract1.env itv in
  let _, fp_vars = Environment.vars env in
  let removed_vars = Array.filter fp_vars ~f:(Var.to_string >> f >> not) in
  let new_env = Environment.remove env removed_vars in
  Abstract1.change_environment (get_man ()) itv new_env false

let forget vars itv =
  let new_env = Environment.remove (Abstract1.env itv) vars in
  Abstract1.change_environment (get_man ()) itv new_env false

let assign itv var texpr =
  let man = get_man () in
  let env =
    Abstract1.env itv |> fun env ->
    Environment.(if mem_var env var then env else add env [||] [| var |])
  in
  let itv = Abstract1.change_environment man itv env false in
  Abstract1.assign_texpr man itv var Texpr1.(of_expr env texpr) None

let lookup itv var =
  let man = get_man () in
  if Environment.mem_var (Abstract1.env itv) var then Abstract1.bound_variable man itv var
  else Interval.top

let interpret stmt itv =
  let man = get_man () in
  let itv = extend_env_by_uses stmt itv in
  match stmt with
  | Array_write _ | Exceptional_call _ -> failwith "todo: Itv#interpret"
  | Write _ | Skip | Expr _ | Call _ -> itv
  | Assume e -> meet_with_constraint itv e
  | Assign { lhs; rhs } -> (
      let lhs = Var.of_string lhs in
      match texpr_of_expr itv rhs with
      | Some rhs_texpr -> assign itv lhs rhs_texpr
      | None ->
          if Environment.mem_var (Abstract1.env itv) lhs then
            (* lhs was constrained, quantify that out *)
            Abstract1.forget_array man itv [| lhs |] false
          else (* lhs was unconstrained, treat as a `skip`*) itv )

let sanitize itv = itv

let show itv =
  pp Format.str_formatter itv;
  Format.flush_str_formatter ()

let hash seed itv = seeded_hash seed @@ Abstract1.hash (get_man ()) itv

let compare (l : t) (r : t) =
  let open Abstract1 in
  let man = get_man () in
  try if is_eq man l r then 0 else if is_leq man l r then -1 else 1
  with Apron.Manager.Error _ -> failwith "Apron.Manager.Error in Itv#compare"

let hash_fold_t h itv = Ppx_hash_lib.Std.Hash.fold_int h (hash 0 itv)

let call ~(callee : Cfg.Fn.t) ~callsite ~caller_state ~fields:_ =
  match callsite with
  | Ast.Stmt.Call { actuals; _ } | Ast.Stmt.Exceptional_call { actuals; _ } ->
      if is_bot caller_state then caller_state
      else
        let caller_state = extend_env_by_uses callsite caller_state in
        let formal_bindings =
          List.filter_map (List.zip_exn callee.formals actuals) ~f:(fun (f, a) ->
              texpr_of_expr caller_state a >>| eval_texpr caller_state >>| pair (Var.of_string f))
        in
        if List.is_empty formal_bindings then
          Abstract1.top (get_man ()) (Environment.make [||] [||])
        else
          let formals = List.map ~f:fst formal_bindings |> Array.of_list in
          let bounds = List.map ~f:snd formal_bindings |> Array.of_list in
          Abstract1.of_box (get_man ()) (Environment.make [||] formals) formals bounds
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)

let return ~callee:_ ~caller:_ ~callsite ~caller_state ~return_state ~fields:_ =
  match callsite with
  | Ast.Stmt.Call { lhs; _ } ->
      let man = get_man () in
      let return_val =
        let retvar = Var.of_string Cfg.retvar in
        if Environment.mem_var (Abstract1.env return_state) retvar then
          Abstract1.bound_variable man return_state retvar
        else Interval.top
      in
      assign caller_state (Var.of_string lhs) Texpr1.(Cst (Coeff.Interval return_val))
  | Ast.Stmt.Exceptional_call _ -> failwith "todo: exceptional Itv#return"
  | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)

let approximate_missing_callee ~caller_state:_ ~callsite:_ = failwith "todo"
