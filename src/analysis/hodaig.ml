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
        | Add_function { method_id = { package; class_name; _ } as method_id; method_decl } ->
            ( Cfg_parser.of_method_decl loc_map ~package ~class_name method_decl |> fun res ->
              Option.value_exn
                ~message:
                  (Format.asprintf "Failed to parse method declaration: %a" Method_id.pp method_id)
                res )
            |> fun (loc_map, edges, fn) ->
            let cfg = Graph.create (module Cfg.G) ~edges () in
            (loc_map, Map.add_exn hodaig ~key:fn ~data:(cfg, []))
        | Delete_function { method_id } -> (
            let loc_map = Loc_map.remove_fn loc_map method_id in
            match Cfg.Fn.Map.fn_by_method_id method_id hodaig with
            | Some fn -> (loc_map, Map.remove hodaig fn)
            | None ->
                failwith
                  (Format.asprintf "Can't remove function %a: does not exist in DAIG" Method_id.pp
                     method_id) )
        | Modify_function { method_id; new_header } -> (
            let new_method_id, formals = match new_header with _ -> failwith "todo" in
            match Cfg.Fn.Map.fn_by_method_id method_id hodaig with
            | Some ({ method_id = _; formals = _; locals; entry; exit; exc_exit } as old_fn) ->
                let cfg, daigs = Map.find_exn hodaig old_fn in
                let (fn : Cfg.Fn.t) =
                  { method_id = new_method_id; formals; locals; entry; exit; exc_exit }
                in
                (loc_map, Map.(remove hodaig old_fn |> add_exn ~key:fn ~data:(cfg, daigs)))
            | None ->
                failwith (Format.asprintf "Can't modify unknown function %a" Method_id.pp method_id)
            )
        | (Add_statements { method_id; _ } as edit)
        | (Modify_statements { method_id; _ } as edit)
        | (Modify_header { method_id; _ } as edit)
        | (Delete_statements { method_id; _ } as edit) -> (
            match Cfg.Fn.Map.fn_by_method_id method_id hodaig with
            | None ->
                failwith
                  (Format.asprintf "can't apply to edit to non-existent function %a" Method_id.pp
                     method_id)
            | Some fn ->
                let cfg, daigs = Map.find_exn hodaig fn in
                let cfg_edit =
                  Tree_diff.apply_edit edit loc_map cfg ~ret:fn.exit ~exc:fn.exc_exit
                in
                let new_daigs =
                  List.map daigs ~f:(fun (init_state, daig) ->
                      (init_state, D.apply_edit ~daig ~cfg_edit ~fn edit))
                in
                (cfg_edit.new_loc_map, Map.set hodaig ~key:fn ~data:(cfg, new_daigs)) ))
end
