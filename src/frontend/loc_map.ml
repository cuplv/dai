open Dai.Import
open Tree_sitter_java
open Syntax
open Cfg

type loc_ctx = { entry : Loc.t; exit : Loc.t; ret : Loc.t; exc : Loc.t }

let pp_loc_ctx fs { entry; exit; ret; exc } =
  Format.fprintf fs "{%a -> %a; ret=%a; exc=%a}" Loc.pp entry Loc.pp exit Loc.pp ret Loc.pp exc

type t = loc_ctx Int.Map.t Method_id.Map.t

let empty = Map.empty (module Method_id)

type 'a or_collision = [ `Ok of 'a | `Collision ]

let add method_id stmt loc_ctx lmap =
  let stmt_hash = CST.sexp_of_statement stmt |> Sexp.hash in
  match Method_id.Map.find lmap method_id with
  | None ->
      `Ok (Method_id.Map.add_exn lmap ~key:method_id ~data:(Int.Map.singleton stmt_hash loc_ctx))
  | Some stmt_hash_map -> (
      match Int.Map.add stmt_hash_map ~key:stmt_hash ~data:loc_ctx with
      | `Duplicate -> `Collision
      | `Ok data -> `Ok (Method_id.Map.set lmap ~key:method_id ~data) )

let remove method_id stmt lmap =
  let stmt_hash = CST.sexp_of_statement stmt |> Sexp.hash in
  let method_lmap = Method_id.Map.find_exn lmap method_id |> flip Int.Map.remove stmt_hash in
  Method_id.Map.set lmap ~key:method_id ~data:method_lmap

let remove_fn = Method_id.Map.remove

let remove_region method_id (region : Loc.Set.t) lmap =
  let new_method_lmap =
    Int.Map.filter (Method_id.Map.find_exn lmap method_id)
      ~f:(fun { entry; exit; ret = _; exc = _ } -> Loc.Set.(mem region entry && mem region exit))
  in
  Method_id.Map.set lmap ~key:method_id ~data:new_method_lmap

let get method_id stmt lmap =
  let stmt_hash = CST.sexp_of_statement stmt |> Sexp.hash in
  Method_id.Map.find_exn lmap method_id |> flip Int.Map.find_exn stmt_hash

let union l r =
  Method_id.Map.merge l r ~f:(fun ~key:_ -> function
    | `Both (x, y) ->
        Int.Map.merge x y ~f:(fun ~key:_ -> function
          | `Both _ -> failwith "collision" | `Left x | `Right x -> Some x)
        |> Option.return
    | `Left x -> Some x
    | `Right y -> Some y)

let rebase_edges method_id ~old_src ~new_src loc_map =
  let new_method_locs =
    Int.Map.map (Method_id.Map.find_exn loc_map method_id) ~f:(fun { entry; exit; ret; exc } ->
        if Loc.equal entry old_src then { entry = new_src; exit; ret; exc }
        else { entry; exit; ret; exc })
  in
  Method_id.Map.set loc_map ~key:method_id ~data:new_method_locs

let pp =
  let pp_inner = Map.pp Int.pp pp_loc_ctx in
  Map.pp Method_id.pp pp_inner
