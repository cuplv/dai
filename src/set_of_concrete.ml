open Import
open Ast

module Val : Abstract.Val = struct
  type t = Set.M(Lit).t [@@deriving compare, equal, hash]

  let pp = Set.pp Lit.pp

  let show = Format.asprintf "%a" pp

  let sanitize x = x

  let hash = seeded_hash

  let join = Set.union

  let widen = Set.union (* not a real widen *)

  let implies l r = Set.for_all l ~f:(Set.mem r)

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
            (Format.asprintf "Unimplemented binary operation: \"%a %a %a\"" Lit.pp l Binop.pp op
               Lit.pp r)
    in
    Set.fold l ~init:bottom ~f:(fun acc curr_l ->
        Set.fold r ~init:acc ~f:(fun acc curr_r -> Set.add acc (concrete_op curr_l curr_r)))

  let eval_unop op v =
    let concrete_op v =
      match (op, v) with
      | Unop.Not, Lit.Bool b -> Lit.Bool (not b)
      | Unop.Neg, Lit.Float f -> Lit.Float (Float.neg f)
      | Unop.Neg, Lit.Int i -> Lit.Int (Int.neg i)
      | _ ->
          failwith (Format.asprintf "Unimplemented unary operation: \"%a %a\"" Unop.pp op Lit.pp v)
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

  let sexp_of_t v = Sexp.List (List.map (Set.to_list v) ~f:Lit.sexp_of_t)

  let t_of_sexp = function
    | Sexp.List l -> List.fold l ~init:bottom ~f:(fun a c -> Set.add a (Lit.t_of_sexp c))
    | _ -> failwith "malformed set_of_concrete sexp"
end

module Env : Abstract.Dom = Env.Make (Val)
