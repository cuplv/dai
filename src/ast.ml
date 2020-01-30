open Import

type ident = string [@@deriving equal, hash, compare]

module Lit = struct
  module T = struct
    type t =
      | Bool of bool
      | Int of int
      | Float of float
      | Null
      | String of string
      | Undefined
    [@@deriving equal, compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  (*  module Set = struct include (
                 Set : module type of Set with type ('elt, 'cmp) t := ('elt, 'cmp) Set.t)
                      type t = Set.M(T).t [@@deriving compare, equal, sexp, hash]
                      let empty = Set.empty (module T)
               end*)

  let pp fs = function
    | Bool b -> Format.pp_print_bool fs b
    | Int i -> Format.pp_print_int fs i
    | Float f -> Format.pp_print_float fs f
    | Null -> Format.pp_print_string fs "null"
    | String s -> Format.pp_print_string fs ("\"" ^ s ^ "\"")
    | Undefined -> Format.pp_print_string fs "undefined"
end

module Binop = struct
  type t =
    | Lt
    | Le
    | Gt
    | Ge
    | Eq
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
  [@@deriving equal, hash, compare]

  let pp fs =
    let ps = Format.pp_print_string fs in
    function
    | Lt -> ps "<"
    | Le -> ps "<="
    | Gt -> ps ">"
    | Ge -> ps ">="
    | Eq -> ps "=="
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
  type t = UPlus | UNeg | Not | BNot | Incr | Decr | Typeof
  [@@deriving compare, equal, hash]

  let pp fs =
    let ps = Format.pp_print_string fs in
    function
    | UPlus -> ps "+"
    | UNeg -> ps "-"
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
    (*    | Call of { fn : t; actuals : t list }*)
    | Binop of { l : t; op : Binop.t; r : t }
    | Unop of { op : Unop.t; e : t }
    | MemberAccess of { rcvr : t; prop : t }
    | Array of t list
  [@@deriving equal, compare, hash]

  let rec pp fs e =
    match e with
    | Var v -> Format.pp_print_string fs v
    | Lit l -> Lit.pp fs l
    (*    | Call { fn; actuals } ->
        Format.fprintf fs "%a(%a)" pp fn (List.pp ",@ " pp) actuals*)
    | Binop { l; op; r } ->
        Format.fprintf fs "@[%a@ %a@ %a@]" pp l Binop.pp op pp r
    | Unop { op; e } -> Format.fprintf fs "%a%a" Unop.pp op pp e
    | MemberAccess { rcvr; prop } -> Format.fprintf fs "%a[%a]" pp rcvr pp prop
    | Array contents -> (List.pp ~pre:"[" ~suf:"]" ",@ " pp) fs contents

  (** fold hash as int, rather than as Ppx_hash_lib.Std.Hash.state *)
  let hash_fold_int acc curr =
    let open Ppx_hash_lib.Std in
    hash_fold_t (Hash.fold_int (Hash.alloc ()) acc) curr |> Hash.get_hash_value
end

module Stmt = struct
  type t =
    | Seq of t * t
    | If of { cond : Expr.t; then_body : t; else_body : t }
    | Assign of { lhs : string; rhs : Expr.t }
    | Throw of { exn : Expr.t }
    | Expr of Expr.t
    | Skip
  [@@deriving equal]

  let seq fst snd =
    match (fst, snd) with Skip, _ -> snd | _, Skip -> fst | _ -> Seq (fst, snd)

  let rec pp fs stmt =
    match stmt with
    | Seq (l, r) -> Format.fprintf fs "@[<v>%a;@ %a@]" pp l pp r
    | If { cond; then_body; else_body } ->
        Format.fprintf fs
          "@[<v>if(%a){@   @[<v 2>%a@]@ } else {@   @[<v 2>%a@]@ }@]" Expr.pp
          cond pp then_body pp else_body
    | Assign { lhs; rhs } -> Format.fprintf fs "@[%s@ =@ %a@]" lhs Expr.pp rhs
    | Throw { exn } -> Format.fprintf fs "throw %a" Expr.pp exn
    | Expr e -> Expr.pp fs e
    | Skip -> Format.pp_print_string fs "skip"
end

(*type t = Stmt.t list*)

let seqify_list = List.fold ~init:Stmt.Skip ~f:Stmt.seq
