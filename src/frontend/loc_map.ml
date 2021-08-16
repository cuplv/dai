open Dai.Import
open Tree_sitter_java
open Syntax.Cfg

type loc_ctx = { entry : Loc.t; exit : Loc.t; ret : Loc.t }

let pp_loc_ctx fs { entry; exit; ret } =
  Format.fprintf fs "{%a -> %a; ret=%a}" Loc.pp entry Loc.pp exit Loc.pp ret

type t = loc_ctx Int.Map.t String.Map.t

let empty = String.Map.empty

let add method_id stmt loc_ctx lmap =
  let stmt_hash = CST.sexp_of_statement stmt |> Sexp.hash in
  match String.Map.find lmap method_id with
  | None -> String.Map.add_exn lmap ~key:method_id ~data:(Int.Map.singleton stmt_hash loc_ctx)
  | Some stmt_hash_map ->
      String.Map.set lmap ~key:method_id
        ~data:(Int.Map.set stmt_hash_map ~key:stmt_hash ~data:loc_ctx)

let remove method_id stmt lmap =
  let stmt_hash = CST.sexp_of_statement stmt |> Sexp.hash in
  let method_lmap = String.Map.find_exn lmap method_id |> flip Int.Map.remove stmt_hash in
  String.Map.set lmap ~key:method_id ~data:method_lmap

let remove_fn = String.Map.remove

let remove_region method_id (region : Loc.Set.t) lmap =
  let new_method_lmap =
    Int.Map.filter (String.Map.find_exn lmap method_id) ~f:(fun { entry; exit; ret = _ } ->
        Loc.Set.(mem region entry && mem region exit))
  in
  String.Map.set lmap ~key:method_id ~data:new_method_lmap

let get method_id stmt lmap =
  let stmt_hash = CST.sexp_of_statement stmt |> Sexp.hash in
  String.Map.find_exn lmap method_id |> flip Int.Map.find_exn stmt_hash

let union l r =
  String.Map.merge l r ~f:(fun ~key:_ -> function
    | `Both (x, y) ->
        Int.Map.merge x y ~f:(fun ~key:_ -> function
          | `Both _ -> failwith "collision" | `Left x | `Right x -> Some x)
        |> Option.return
    | `Left x -> Some x
    | `Right y -> Some y)

let rebase_edges method_id ~old_src ~new_src loc_map =
  let new_method_locs =
    Int.Map.map (String.Map.find_exn loc_map method_id) ~f:(fun { entry; exit; ret } ->
        if Loc.equal entry old_src then { entry = new_src; exit; ret } else { entry; exit; ret })
  in
  String.Map.set loc_map ~key:method_id ~data:new_method_locs

let pp =
  let pp_inner = Map.pp Int.pp pp_loc_ctx in
  Map.pp String.pp pp_inner
