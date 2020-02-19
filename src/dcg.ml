open Adapton

let seeded_hash = Hashtbl.seeded_hash (* Core shadows Hashtbl via Import *)

open Import

module Make (ArtLib : ArtLib.S) = struct
  include Ast

  module Stmt = struct
    type 'a t =
      | Seq of 'a t * 'a t
      | If of { cond : Ast.Expr.t; then_body : 'a t; else_body : 'a t }
      | Assign of { lhs : ident; rhs : Expr.t }
      | Throw of { exn : Expr.t }
      | Expr of Expr.t
      | Skip
      | Art of 'a (* Adapton articulation point *)
    (*      | Name of { name : Name.t; stmt : 'a t } (* Adapton nominal cache point*)*)
    [@@deriving equal, compare]

    let rec sanitize san = function
      | Seq (l, r) -> Seq (sanitize san l, sanitize san r)
      | If { cond; then_body; else_body } ->
          If
            {
              cond;
              then_body = sanitize san then_body;
              else_body = sanitize san else_body;
            }
      | Assign { lhs; rhs } -> Assign { lhs; rhs }
      | Throw { exn } -> Throw { exn }
      | Expr e -> Expr e
      | Skip -> Skip
      | Art x -> Art (san x)

    (*      | Name { name; stmt } ->
          Name { name = Name.sanitize name; stmt = sanitize san stmt }*)

    let rec hash h seed = function
      | Seq (l, r) -> hash h (hash h seed l) r
      | If { cond; then_body; else_body } ->
          let cond_hash = Expr.hash_fold_int seed cond in
          hash h (hash h cond_hash then_body) else_body
      | Assign { lhs; rhs } -> seeded_hash (Expr.hash_fold_int seed rhs) lhs
      | Skip -> seeded_hash seed Skip
      | Throw { exn } -> Expr.hash_fold_int seed exn
      | Expr e -> Expr.hash_fold_int seed e
      | Art x -> h seed x

    (*      | Name { name; stmt } -> hash h (Name.hash seed name) stmt*)

    let rec pp pp_a fs stmt =
      match stmt with
      | Seq (l, r) -> Format.fprintf fs "@[<v>%a;@ %a@]" (pp pp_a) l (pp pp_a) r
      | If { cond; then_body; else_body } ->
          Format.fprintf fs
            "@[<v>if(%a){@   @[<v 2>%a@]@ } else {@   @[<v 2>%a@]@ }@]" Expr.pp
            cond (pp pp_a) then_body (pp pp_a) else_body
      | Assign { lhs; rhs } -> Format.fprintf fs "@[%s@ =@ %a@]" lhs Expr.pp rhs
      | Throw { exn } -> Format.fprintf fs "throw %a" Expr.pp exn
      | Expr e -> Expr.pp fs e
      | Skip -> Format.pp_print_string fs "skip"
      | Art a -> Format.fprintf fs "[| %a |]" pp_a a

    (*      | Name { name; stmt } ->
          Format.fprintf fs "%s#%a(%s%a%s)%s" Colors.green Name.pp name
            Colors.reset (pp pp_a) stmt Colors.green Colors.reset*)

    let show pp_a = Format.asprintf "%a" (pp pp_a)
  end

  module AStmt : sig
    include Articulated.Fix(ArtLib)(Name)(Stmt).S

    val hash : int -> t -> int

    val seq : t -> t -> t

    val cell : name -> t -> t

    val set : t -> t -> unit

    val force : Art.t -> t
  end = struct
    include Articulated.Fix (ArtLib) (Name) (Stmt)

    let hash_leaf = hash

    let rec hash seed = function
      | Stmt.Seq (l, r) -> hash (hash seed l) r
      | Stmt.If { cond; then_body; else_body } ->
          let cond_hash = Expr.hash_fold_int seed cond in
          hash (hash cond_hash then_body) else_body
      | Stmt.Art a -> hash seed (A.force a)
      (*      | Stmt.Name { name; stmt } -> hash (Name.hash seed name) stmt*)
      | stmt -> hash_leaf seed stmt

    let seq x y =
      match (x, y) with
      | Stmt.Skip, y -> y
      | x, Stmt.Skip -> x
      | x, y -> Stmt.Seq (x, y)

    let cell name stmt = Stmt.Art (A.cell name stmt)

    let set = function
      | Stmt.Art a -> A.set a
      | _ -> failwith "Can't set a non-art AST node"

    let force = A.force

    let rec pp fs stmt = Stmt.pp (fun fs a -> pp fs (A.force a)) fs stmt
  end
end

module With_default_articulation = Make (DefaultArtLib)
