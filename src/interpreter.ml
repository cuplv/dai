let seeded_hash = Hashtbl.seeded_hash

open Import
open Ast
open Adapton
module Engine = Engine.Make (Engine.Default_params)
module ArtLib = Engine.ArtLib

type truthiness = [ `Neither | `T | `F | `Either ]

module type Domain = sig
  include Data.S

  include Articulated.S

  val init : t

  val join : t -> t -> t

  val exec_stmt : t -> Stmt.t -> t option
end

module type Abstract_value = sig
  include Adapton.Data.S

  val pp : t pp

  val join : t -> t -> t

  val eval_binop : t -> Binop.t -> t -> t

  val of_lit : Lit.t -> t

  val eval_unop : Unop.t -> t -> t

  val truthiness : t -> truthiness
end

module Set_of_concrete : Abstract_value = struct
  type t = Set.M(Lit).t [@@deriving compare, equal]

  let pp = Set.pp Lit.pp

  let show = Format.asprintf "%a" pp

  let sanitize x = x

  let hash = seeded_hash

  let join = Set.union

  let bottom = Set.empty (module Lit)

  let of_lit = Set.singleton (Set.comparator_s bottom)

  let eval_binop l op r =
    let concrete_op l r =
      match (op, l, r) with
      | Binop.Plus, Lit.Int l, Lit.Int r -> Lit.Int (l + r)
      | Binop.Plus, Lit.Float l, Lit.Float r -> Lit.Float (l +. r)
      | Binop.Plus, Lit.String l, Lit.String r -> Lit.String (l ^ r)
      | Binop.Minus, Lit.Int l, Lit.Int r -> Lit.Int (l - r)
      | Binop.Minus, Lit.Float l, Lit.Float r -> Lit.Float (l -. r)
      | _ ->
          failwith
            (Format.asprintf "Unimplemented binary operation: \"%a %a %a\""
               Lit.pp l Binop.pp op Lit.pp r)
    in
    Set.fold l ~init:bottom ~f:(fun acc curr_l ->
        Set.fold r ~init:acc ~f:(fun acc curr_r ->
            Set.add acc (concrete_op curr_l curr_r)))

  let eval_unop op v =
    let concrete_op v =
      match (op, v) with
      | Unop.Not, Lit.Bool b -> Lit.Bool (not b)
      | Unop.Neg, Lit.Float f -> Lit.Float (Float.neg f)
      | Unop.Neg, Lit.Int i -> Lit.Int (Int.neg i)
      | _ ->
          failwith
            (Format.asprintf "Unimplemented unary operation: \"%a %a\"" Unop.pp
               op Lit.pp v)
    in
    Set.fold v ~init:bottom ~f:(fun acc curr -> Set.add acc (concrete_op curr))

  let truthiness =
    Set.fold ~init:`Neither ~f:(fun acc curr ->
        let curr =
          match curr with
          | Lit.Bool b -> b
          | Lit.Int i -> i <> 0
          | Lit.Float f -> not @@ Float.equal f 0.0
          | Lit.String _ -> true
          | Lit.Null | Lit.Undefined -> false
        in
        match (acc, curr) with
        | `Neither, true | `T, true -> `T
        | `Neither, false | `F, false -> `F
        | `T, false | `F, true | `Either, _ -> `Either)
end

module EnvNonInc (Val : Abstract_value) : Domain = struct
  module Env = Trie.Map.MakeNonInc (Name) (Engine.ArtLib) (Types.String) (Val)
  include Env

  let pp fs (env : t) =
    let pp_string_color color str =
      Format.fprintf fs "%s%s%s" color str Colors.reset
    in
    pp_string_color Colors.cyan "{";
    Format.open_hovbox 0;
    Format.print_space ();
    List.iter (to_list env) ~f:(fun (k, v) ->
        Format.fprintf fs "%s%s%s -> %a %s;%s@ " Colors.cyan k Colors.reset
          Val.pp v Colors.red Colors.reset);
    Format.close_box ();
    pp_string_color Colors.cyan "}"

  let init = of_list []

  let join =
    fold (fun acc k v ->
        match find acc k with
        | Some acc_v -> add acc k (Val.join v acc_v)
        | None -> add acc k v)

  (*    let mfn =
      Art.mk_mfn
        (Name.of_string "interpreter#env#join")
        (module Types.Tuple2 (Env) (Env))
        (fun _mfn (l, r) ->
          Env.fold
            (fun acc k v ->
              match Env.find acc k with
              | Some acc_v -> failwith "todo"
              | None -> 
            l r)
    in
    fun l r -> mfn.mfn_data (l, r)*)

  let rec eval_expr env = function
    | Expr.Var v -> (
        match find env v with
        | Some value -> value
        | None -> Val.of_lit Lit.Undefined )
    | Expr.Lit l -> Val.of_lit l
    | Expr.Binop { l; op; r } ->
        Val.eval_binop (eval_expr env l) op (eval_expr env r)
    | Expr.Unop { op; e } -> Val.eval_unop op (eval_expr env e)

  (*    | Call of { fn : t; actuals : t list }*)
  (*    | MemberAccess { rcvr=_ ; prop=_ } -> 
          | Array _xs -> failwith "todo"*)

  let rec exec_stmt env stmt =
    let open Stmt in
    match stmt with
    | Seq (l, r) -> exec_stmt env l >>= flip exec_stmt r
    | If { cond; then_body; else_body } -> (
        match Val.truthiness @@ eval_expr env cond with
        | `T -> exec_stmt env then_body
        | `F -> exec_stmt env else_body
        | `Either ->
            let then_res = exec_stmt env then_body in
            let else_res = exec_stmt env else_body in
            Option.merge then_res else_res join
        | `Neither -> None )
    | Assign { lhs; rhs } ->
        let rhs = eval_expr env rhs in
        Some (add env lhs rhs)
    | Throw { exn = _ } -> None
    | Expr _ | Skip -> Some env
end

module ENI = EnvNonInc (Set_of_concrete)

let prgm0 = Parser.(stmt_list arith0)

let prgm1 = Parser.(stmt_list arith1)

let prgm2 = Parser.(stmt_list arith2)

let%test "interpret" =
  Option.iter (ENI.exec_stmt ENI.init prgm0) ~f:(Format.printf "\n\n%a" ENI.pp);
  false

let%test "interpret" =
  Option.iter (ENI.exec_stmt ENI.init prgm1) ~f:(Format.printf "\n\n%a" ENI.pp);
  false

let%test "interpret" =
  Option.iter (ENI.exec_stmt ENI.init prgm2) ~f:(Format.printf "\n\n%a" ENI.pp);
  false
