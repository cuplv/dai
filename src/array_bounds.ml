open Import

(*open Apron
open Option.Monad_infix*)

module Addr_map = struct
  type t = Addr.Abstract.t Map.M(String).t [@@deriving compare, sexp]

  let join =
    Map.merge ~f:(fun ~key:_ ->
      function `Both (a1, a2) -> Some (Addr.Abstract.union a1 a2) | `Left a | `Right a -> Some a)

  let implies l r =
    List.for_all (Map.to_alist l) ~f:(fun (k, lv) ->
        match Map.find r k with Some rv -> Addr.Abstract.is_subset lv ~of_:rv | None -> false)

  let equal = Map.equal Addr.Abstract.equal

  let pp fs =
    Map.to_alist >> (List.pp ", " ~pre:"{" ~suf:"}" (pp_pair String.pp Addr.Abstract.pp)) fs
end

(* Keep a map of variable names to array abstract addresses, along with an APRON interval. None indicates bottom. *)
type t = (Addr_map.t * Itv.t) option [@@deriving compare]

let is_bot = Option.fold ~init:true ~f:(fun _ (_, itv) -> Itv.is_bot itv)

let mk_binary_op a_op i_op l r =
  match (l, r) with
  | Some (al, il), Some (ar, ir) -> Some (a_op al ar, i_op il ir)
  | Some (a, i), _ | _, Some (a, i) -> Some (a, i)
  | _ -> None

let widen = mk_binary_op Addr_map.join Itv.widen

let join = mk_binary_op Addr_map.join Itv.join

let implies l r =
  match (l, r) with
  | None, _ -> true
  | _, None -> false
  | Some (al, il), Some (ar, ir) -> Addr_map.implies al ar && Itv.implies il ir

let interpret = failwith "todo"

let init () = pair (Map.empty (module String)) (Itv.init ()) |> Option.some

let pp = Option.pp (pp_pair Addr_map.pp Itv.pp)

let sanitize x = x

let show x =
  pp Format.str_formatter x;
  Format.flush_str_formatter ()

let hash = seeded_hash

let equal l r =
  match (l, r) with
  | None, None -> true
  | Some (al, il), Some (ar, ir) -> Addr_map.equal al ar && Itv.equal il ir
  | _ -> false

let sexp_of_t = function
  | Some (a, i) -> Sexp.List [ Addr_map.sexp_of_t a; Itv.sexp_of_t i ]
  | _ -> Sexp.Atom "bottom"

let t_of_sexp = function
  | Sexp.Atom "bottom" -> None
  | Sexp.List [ a_sexp; i_sexp ] -> Some (Addr_map.t_of_sexp a_sexp, Itv.t_of_sexp i_sexp)
  | _ -> failwith "malformed abstract state s-expression"

let hash_fold_t seed = function
  | Some (a, i) ->
      let i_hash = Itv.hash_fold_t seed i in
      Ppx_hash_lib.Std.Hash.fold_int i_hash (Addr.Abstract.hash 13 a)
  | None -> seed
