open Dai
open Import
open Syntax

(* use Make_env_with_heap to get a domain *)
type t = Top | Null | NotNull | Bot [@@deriving hash, equal, sexp, compare]

let not_null = NotNull

let top = Top

let join = function
  | Top -> fun _ -> Top
  | Bot -> fun n2 -> n2
  | Null -> ( function Top -> Top | Null -> Null | NotNull -> Top | Bot -> Null)
  | NotNull -> ( function Top -> Top | Null -> Top | NotNull -> NotNull | Bot -> NotNull)

let widen = join

let implies n1 n2 =
  match (n1, n2) with
  | Bot, _ -> true
  | _, Top -> true
  | NotNull, NotNull -> true
  | Null, Null -> true
  | _, _ -> false

let ( <= ) = implies

(* Because after the operation, we get a nonnull value, or an exception, which is an error *)
let eval_unop _uop _n = NotNull

let eval_binop _n1 _binop _n2 = NotNull

let of_lit = function Ast.Lit.Null -> Null | _ -> NotNull

let models nullness lit =
  match nullness with
  | Bot -> false
  | Top -> true
  | Null -> phys_equal lit Ast.Lit.Null
  | NotNull -> not (phys_equal lit Ast.Lit.Null)

let truthiness = function Null | Bot -> `Neither | _ -> `Either

(* boilerplate *)
let sanitize = Fn.id

let pp fs = function
  | Top -> Format.pp_print_string fs "Top"
  | Bot -> Format.pp_print_string fs "Bot"
  | Null -> Format.pp_print_string fs "Null"
  | NotNull -> Format.pp_print_string fs "NotNull"

let show x =
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

let hash = seeded_hash
