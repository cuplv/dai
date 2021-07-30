open Dai.Import
open Tree_sitter_java

type loc = Dai.Cfg.Loc.t

type loc_ctx = { entry : loc; exit : loc; ret : loc }

let pp_loc_ctx fs { entry; exit; ret } =
  let open Dai.Cfg in
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

let get method_id stmt lmap =
  let stmt_hash = CST.sexp_of_statement stmt |> Sexp.hash in
  String.Map.find_exn lmap method_id |> flip Int.Map.find_exn stmt_hash

let pp =
  let pp_inner = Map.pp Int.pp pp_loc_ctx in
  Map.pp String.pp pp_inner
