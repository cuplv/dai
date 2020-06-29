open Import

type ident = string [@@deriving equal, hash, compare, sexp_of]

module Lit = struct
  module T = struct
    type t = Bool of bool | Int of int | Float of float | Null | String of string | Undefined
    [@@deriving equal, compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  let pp fs = function
    | Bool b -> Format.pp_print_bool fs b
    | Int i -> Format.pp_print_int fs i
    | Float f -> Format.pp_print_float fs f
    | Null -> Format.pp_print_string fs "null"
    | String s -> Format.pp_print_string fs ("\\\"" ^ s ^ "\\\"")
    | Undefined -> Format.pp_print_string fs "undefined"
end

module Binop = struct
  type t =
    | Lt
    | Le
    | Gt
    | Ge
    | Eq
    | NEq
    | SEq
    | Plus
    | Minus
    | Times
    | Divided_by
    | Mod
    | Pow
    | Or
    | And
    | BOr
    | BAnd
    | BXor
    | LShift
    | RShift
    | URShift
    | Instanceof
  [@@deriving equal, hash, compare, sexp_of]

  let pp fs =
    let ps = Format.pp_print_string fs in
    function
    | Lt -> ps "<"
    | Le -> ps "<="
    | Gt -> ps ">"
    | Ge -> ps ">="
    | Eq -> ps "=="
    | NEq -> ps "!="
    | SEq -> ps "==="
    | Plus -> ps "+"
    | Minus -> ps "-"
    | Times -> ps "*"
    | Divided_by -> ps "/"
    | Mod -> ps "%"
    | Pow -> ps "**"
    | Or -> ps "||"
    | And -> ps "&&"
    | BOr -> ps "|"
    | BAnd -> ps "&"
    | BXor -> ps "^"
    | LShift -> ps "<<"
    | RShift -> ps ">>"
    | URShift -> ps ">>>"
    | Instanceof -> ps "instanceof"
end

module Unop = struct
  type t = Plus | Neg | Not | BNot | Incr | Decr | Typeof
  [@@deriving compare, equal, hash, sexp_of]

  let pp fs =
    let ps = Format.pp_print_string fs in
    function
    | Plus -> ps "+"
    | Neg -> ps "-"
    | Not -> ps "!"
    | BNot -> ps "~"
    | Incr -> ps "++"
    | Decr -> ps "--"
    | Typeof -> ps "typeof "
end

module Expr = struct
  type t =
    | Var of ident
    | Lit of Lit.t
    | Binop of { l : t; op : Binop.t; r : t }
    | Unop of { op : Unop.t; e : t }
    | Deref of { rcvr : t; field : t }
    | Array of { elts : t list; alloc_site : int * int }
  [@@deriving equal, compare, hash, sexp_of]

  let rec pp fs e =
    match e with
    | Var v -> Format.pp_print_string fs v
    | Lit l -> Lit.pp fs l
    (*    | Call { fn; actuals } ->
        Format.fprintf fs "%a(%a)" pp fn (List.pp ",@ " pp) actuals*)
    | Binop { l; op; r } -> Format.fprintf fs "%a %a %a" pp l Binop.pp op pp r
    | Unop { op; e } -> Format.fprintf fs "%a%a" Unop.pp op pp e
    | Deref { rcvr; field } -> Format.fprintf fs "%a[%a]" pp rcvr pp field
    | Array { elts; alloc_site = line, col } ->
        (List.pp ", " ~pre:"[" ~suf:"]" pp) fs elts;
        Format.fprintf fs "%@(%i,%i)" line col

  (** fold hash as int, rather than as Ppx_hash_lib.Std.Hash.state *)
  let hash_fold_int acc curr =
    let open Ppx_hash_lib.Std in
    hash_fold_t (Hash.fold_int (Hash.alloc ()) acc) curr |> Hash.get_hash_value
end

module Stmt = struct
  type t =
    | Assign of { lhs : string; rhs : Expr.t }
    | Write of { rcvr : string; field : Expr.t; rhs : Expr.t }
    | Throw of { exn : Expr.t }
    | Expr of Expr.t
    | Assume of Expr.t
    | Skip
  [@@deriving compare, equal, sexp_of]

  let pp fs stmt =
    match stmt with
    | Assign { lhs; rhs } -> Format.fprintf fs "%s := %a" lhs Expr.pp rhs
    | Write { rcvr; field; rhs } -> Format.fprintf fs "%s[%a] := %a" rcvr Expr.pp field Expr.pp rhs
    | Throw { exn } -> Format.fprintf fs "throw %a" Expr.pp exn
    | Expr e -> Expr.pp fs e
    | Assume e -> Format.fprintf fs "assume %a" Expr.pp e
    | Skip -> Format.pp_print_string fs "skip"

  let to_string stmt : string =
    Format.fprintf Format.str_formatter "%a" pp stmt;
    Format.flush_str_formatter ()

  let sanitize x = x

  let show x =
    pp Format.str_formatter x;
    Format.flush_str_formatter ()

  let hash = seeded_hash
end
