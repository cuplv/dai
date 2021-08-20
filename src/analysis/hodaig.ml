open Dai.Import
open Domain
open Syntax
open Frontend

module Make (Dom : Abstract.Dom) = struct
  module D = Daig.Make (Dom)

  type t = D.t Cfg.Fn.Map.t

  (*let get_daig ~method_id hodaig =
    Map.fold hodaig ~init:None ~f:(fun ~key ~data -> function
        | None -> if Cfg.Fn.name key |> String.equal method_id then Some data else None
        | x -> x
      )*)

  let apply ~diff loc_map hodaig =
    List.fold diff ~init:(loc_map, hodaig) ~f:(fun (loc_map, hodaig) ->
        let open Tree_diff in
        function
        | Add_function { method_id; method_decl } ->
            let class_prefix = String.rindex_exn method_id '#' |> String.slice method_id 0 in
            ( Cfg_parser.of_method_decl loc_map ~class_prefix method_decl |> fun res ->
              Option.value_exn ~message:("Failed to parse method declaration: " ^ method_id) res )
            |> fun (loc_map, edges, fn) ->
            let cfg = Graph.create (module Cfg.G) ~edges () in
            let daig = failwith "todo" in
            (loc_map, Map.add_exn hodaig ~key:fn ~data:(daig, cfg))
        | Delete_function { method_id } -> (
            let loc_map = Loc_map.remove_fn loc_map method_id in
            match Cfg.Fn.Map.fn_by_method_id method_id hodaig with
            | Some fn -> (loc_map, Map.remove hodaig fn)
            | None ->
                failwith
                  (Format.asprintf "Can't remove function %s: does not exist in DAIG" method_id) )
        | Modify_function { method_id; new_header } -> (
            let name, formals = match new_header with _ -> failwith "todo" in
            match Cfg.Fn.Map.fn_by_method_id method_id hodaig with
            | Some ({ name = _; formals = _; locals; entry; exit } as old_fn) ->
                let daig, cfg = Map.find_exn hodaig old_fn in
                let (fn : Cfg.Fn.t) = { name; formals; locals; entry; exit } in
                (loc_map, Map.(remove hodaig old_fn |> add_exn ~key:fn ~data:(daig, cfg)))
            | None -> failwith (Format.asprintf "Can't modify unknown function %s" method_id) )
        | (Add_statements { method_id; _ } as edit)
        | (Modify_statements { method_id; _ } as edit)
        | (Modify_header { method_id; _ } as edit)
        | (Delete_statements { method_id; _ } as edit) -> (
            match Cfg.Fn.Map.fn_by_method_id method_id hodaig with
            | None ->
                failwith
                  (Format.asprintf "can't apply to edit to non-existent function %s" method_id)
            | Some fn ->
                let daig = Map.find_exn hodaig fn in
                let loc_map, daig = D.apply_edit loc_map daig ~ret:fn.exit edit in
                (loc_map, Map.set hodaig ~key:fn ~data:daig) ))
end
