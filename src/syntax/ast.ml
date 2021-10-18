open Dai
open Import

type ident = string [@@deriving equal, hash, compare, sexp_of]

module Lit = struct
  module T = struct
    type t =
      | Bool of bool
      | Int of int
      | Float of float
      | Null
      | String of string
      | Char of Uchar.t
    [@@deriving equal, compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  let pp fs = function
    | Bool b -> Bool.pp fs b
    | Int i -> Int.pp fs i
    | Float f -> Float.pp fs f
    | Null -> Format.pp_print_string fs "null"
    | String s -> Format.pp_print_string fs ("\\\"" ^ s ^ "\\\"")
    | Char c -> Uchar.pp fs c
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
    | Deref of { rcvr : ident; field : ident }
    | Array_access of { rcvr : t; idx : t }
    | Array_literal of { elts : t list; alloc_site : Alloc_site.t }
    | Array_create of { elt_type : string; size : t; alloc_site : Alloc_site.t }
  [@@deriving equal, compare, hash, sexp_of]

  let rec pp fs e =
    match e with
    | Var v -> Format.pp_print_string fs v
    | Lit l -> Lit.pp fs l
    | Binop { l; op; r } -> Format.fprintf fs "%a %a %a" pp l Binop.pp op pp r
    | Unop { op; e } -> Format.fprintf fs "%a%a" Unop.pp op pp e
    | Deref { rcvr; field } -> Format.fprintf fs "%s.%s" rcvr field
    | Array_access { rcvr; idx } -> Format.fprintf fs "%a[%a]" pp rcvr pp idx
    | Array_literal { elts; alloc_site } ->
        Format.fprintf fs "%a%@%a" (List.pp ", " ~pre:"{" ~suf:"}" pp) elts Alloc_site.pp alloc_site
    | Array_create { elt_type; size; alloc_site } ->
        Format.fprintf fs "new@%a %a[%a]" Alloc_site.pp alloc_site String.pp elt_type pp size

  let rec uses =
    let uses_in_list exprs =
      List.fold exprs ~init:String.Set.empty ~f:(fun a c -> Set.union a (uses c))
    in
    function
    | Var v -> String.Set.singleton v
    | Binop { l; op = _; r } -> Set.union (uses l) (uses r)
    | Unop { op = _; e } -> uses e
    | Deref { rcvr; field = _ } -> String.Set.singleton rcvr
    | Array_literal { elts; alloc_site = _ } -> uses_in_list elts
    | Array_access { rcvr; idx } -> Set.union (uses rcvr) (uses idx)
    | Array_create { elt_type = _; size; alloc_site = _ } -> uses size
    | _ -> String.Set.empty

  (** fold hash as int, rather than as Ppx_hash_lib.Std.Hash.state *)
  let hash_fold_int acc curr =
    let open Ppx_hash_lib.Std in
    hash_fold_t (Hash.fold_int (Hash.alloc ()) acc) curr |> Hash.get_hash_value

  let unop op e = Unop { op; e }

  let binop l op r = Binop { l; op; r }
end

module Stmt = struct
  type t =
    | Array_write of { rcvr : string; idx : Expr.t; rhs : Expr.t }
    | Assign of { lhs : string; rhs : Expr.t }
    | Assume of Expr.t
    | Call of {
        lhs : string;
        rcvr : string;
        meth : string;
        actuals : Expr.t list;
        alloc_site : Alloc_site.t option;
      }
    | Exceptional_call of { rcvr : string; meth : string; actuals : Expr.t list }
    | Expr of Expr.t
    | Skip
    | Write of { rcvr : string; field : string; rhs : Expr.t }
  [@@deriving compare, equal, hash, sexp_of]

  let pp fs stmt =
    match stmt with
    | Array_write { rcvr; idx; rhs } ->
        Format.fprintf fs "%s[%a] := %a" rcvr Expr.pp idx Expr.pp rhs
    | Assign { lhs; rhs } -> Format.fprintf fs "%s := %a" lhs Expr.pp rhs
    | Assume e -> Format.fprintf fs "assume %a" Expr.pp e
    | Call { lhs; rcvr; meth; actuals; alloc_site = _ } ->
        Format.fprintf fs "%s := %s.%s(%a)" lhs rcvr meth (List.pp ", " Expr.pp) actuals
    | Exceptional_call { rcvr; meth; actuals } ->
        Format.fprintf fs "exc-return %s.%s(%a)" rcvr meth (List.pp ", " Expr.pp) actuals
    | Expr e -> Expr.pp fs e
    | Skip -> Format.pp_print_string fs "skip"
    | Write { rcvr; field; rhs } -> Format.fprintf fs "%s.%s := %a" rcvr field Expr.pp rhs

  let uses = function
    | Array_write { rcvr = _; idx; rhs } -> Set.union (Expr.uses idx) (Expr.uses rhs)
    | Assign { lhs = _; rhs } -> Expr.uses rhs
    | Assume e -> Expr.uses e
    | Call { lhs = _; rcvr; meth = _; actuals; alloc_site = _ }
    | Exceptional_call { rcvr; meth = _; actuals } ->
        List.fold actuals ~init:(String.Set.singleton rcvr) ~f:(fun a c ->
            Set.union a (Expr.uses c))
    | Expr e -> Expr.uses e
    | Skip -> String.Set.empty
    | Write { rcvr; field = _; rhs } -> String.Set.add (Expr.uses rhs) rcvr

  let def = function Assign { lhs; _ } | Call { lhs; _ } -> Some lhs | _ -> None

  let to_string stmt : string =
    Format.fprintf Format.str_formatter "%a" pp stmt;
    Format.flush_str_formatter ()

  let sanitize x = x

  let show x =
    pp Format.str_formatter x;
    Format.flush_str_formatter ()

  let hash = seeded_hash

  let skip = Skip

  let is_exc = function Exceptional_call _ -> true | _ -> false
end
