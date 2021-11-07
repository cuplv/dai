open Dai.Import
open Domain
open Syntax
open Frontend

module Make (Dom : Abstract.Dom) = struct
  module Dom = Abstract.DomWithDataStructures (Dom)
  module D = Daig.Make (Dom)
  module Q = Query.Make (Dom)

  (*module R = Relation.Make (Dom)
    type summary = {exit_triples : R.Set.t; exc_exit_triples : R.Set.t}*)
  (* let summary_store : R.Set.t Cfg.Fn.Map.t ref = ref Cfg.Fn.Map.empty *)

  module Dep = struct
    module T = struct
      type t = { callsite : Ast.Stmt.t; ctx : Dom.t; fn : Cfg.Fn.t; nm : D.Name.t }
      [@@deriving compare, hash, sexp_of]

      let _pp fs dep =
        Format.fprintf fs "(%a in %a with ctx %a summarizing %a)" D.Name.pp dep.nm Cfg.Fn.pp dep.fn
          Dom.pp dep.ctx Ast.Stmt.pp dep.callsite

      (* tracks a single _use_ of a summary, to support interprocedural dirtying
         represents the value of [nm] in the DAIG for procedure [fn] in context [ctx]*)
    end

    include T

    module T_comparator = struct
      include Comparator.Make (T)
      include T
    end

    module Set = struct
      include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

      type t = Set.M(T_comparator).t

      let empty = Set.empty (module T_comparator)

      let singleton = Set.singleton (module T_comparator)
    end

    let interproc_deps : Set.t Cfg.Fn.Map.t ref = ref Cfg.Fn.Map.empty

    let add ~callee ~caller:(nm, fn, ctx, callsite) =
      let data =
        { nm; fn; ctx; callsite }
        |>
        match Map.find !interproc_deps fn with None -> Set.singleton | Some deps -> Set.add deps
      in
      interproc_deps := Map.set !interproc_deps ~key:callee ~data

    let pop ~fn =
      match Map.find !interproc_deps fn with
      | None -> Set.empty
      | Some deps ->
          interproc_deps := Map.set !interproc_deps ~key:fn ~data:Set.empty;
          deps

    let self_loops fn =
      match Map.find !interproc_deps fn with
      | None -> Set.empty
      | Some deps -> Set.filter deps ~f:(fun dep -> Cfg.Fn.equal fn dep.fn)
  end

  type t = (Cfg.t * D.t Dom.Map.t) Cfg.Fn.Map.t

  (* Each procedure (i.e [Cfg.Fn.t]'s) maps to its [Cfg.t] representation as well as a map from procedure-entry abstract states (i.e. [Dom.t]'s)
     to corresponding DAIGs ([D.t]'s and any interprocedural dependencies thereof ([dep list]'s)*)

  let init ~cfgs : t = Cfg.Fn.Map.map cfgs ~f:(flip pair Dom.Map.empty)

  let add_exn ~cfgs dsg =
    Cfg.Fn.Map.fold cfgs ~init:dsg ~f:(fun ~key ~data dsg ->
        match Map.add dsg ~key ~data:(data, Dom.Map.empty) with
        | `Ok res -> res
        | `Duplicate -> failwith (Format.asprintf "can't add duplicate Cfg.Fn.t: %a" Cfg.Fn.pp key))

  let fns = Cfg.Fn.Map.keys

  let set_daig dsg daig fn entry_state =
    let cfg, daigs = Map.find_exn dsg fn in
    Map.set dsg ~key:fn ~data:(cfg, Map.set daigs ~key:entry_state ~data:daig)

  let get_daig_exn dsg fn entry_state = Map.find_exn dsg fn |> snd |> flip Map.find_exn entry_state

  let dump_dot ~filename (dsg : t) =
    let daigs : (Cfg.Fn.t * D.t) list =
      Cfg.Fn.Map.fold dsg ~init:[] ~f:(fun ~key:fn ~data:(_, daigs) acc ->
          List.map (Map.data daigs) ~f:(pair fn) @ acc)
    in
    let _ = Sys.command "mkdir -p scratch" in
    List.iteri daigs ~f:(fun idx (fn, daig) ->
        let filename = "scratch/" ^ Int.to_string idx ^ ".dot" in
        let loc_labeller l =
          if Cfg.Loc.equal l fn.entry then
            Some (Format.asprintf "ENTRY[%a]: " Method_id.pp fn.method_id)
          else None
        in
        D.dump_dot ~filename ~loc_labeller daig);
    let _ = Sys.command ("dot scratch/*.dot | gvpack 2>/dev/null > " ^ filename) in
    let _ = Sys.command "rm -r scratch" in
    ()

  let materialize_daig ~(fn : Cfg.Fn.t) ~(entry_state : Dom.t) (dsg : t) =
    let cfg, daigs = Map.find_exn dsg fn in
    match Map.find daigs entry_state with
    | Some daig -> (daig, dsg)
    | None ->
        let (daig as data) = D.of_cfg ~entry_state ~cfg ~fn in
        let daigs = Map.add_exn daigs ~key:entry_state ~data in
        (daig, Map.set dsg ~key:fn ~data:(cfg, daigs))

  let dirty_interproc_deps dsg fn =
    Set.fold (Dep.pop ~fn) ~init:dsg ~f:(fun dsg dep ->
        let daig = D.dirty dep.nm (get_daig_exn dsg dep.fn dep.ctx) in
        set_daig dsg daig dep.fn dep.ctx)

  let summarize_with_callgraph (dsg : t) fields callgraph (caller : Cfg.Fn.t) caller_entry_state
      ~callsite:(callsite, nm) caller_state =
    let open Option.Monad_infix in
    let callees =
      Callgraph.resolve_with_callgraph ~callsite ~caller_method:caller.method_id ~callgraph
    in
    if Set.is_empty callees then Some (Dom.approximate_missing_callee ~caller_state ~callsite)
    else
      let is_exc =
        match callsite with
        | Ast.Stmt.Call _ -> false
        | Ast.Stmt.Exceptional_call _ -> true
        | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)
      in
      Set.fold
        ~init:(Some (Dom.bottom ()))
        callees
        ~f:(fun acc_result callee ->
          acc_result >>= fun acc_poststate ->
          let callee_entry_state = Dom.call ~callee ~callsite ~caller_state ~fields in
          (* four cases:
             (1) callee entry state is bottom, so no effect on return state
             (2) this is a recusrive call and we already have an exactly-matching summary, so no widening needed
             (3) this is a recursive call and widening has converged on the entry state, so we add a self-dependency and continue
             (4) this is a non-recursive call or recursive call yet to converge on its entry state, so attempt to summarize
          *)
          if
            Dom.is_bot callee_entry_state
            (* case (1); do this before widening to avoid needless work *)
          then acc_result
          else
            let is_recursive = Method_id.equal caller.method_id callee.method_id in
            let exit_loc = if is_exc then callee.exc_exit else callee.exit in
            let exactly_matching_daig =
              Map.find_exn dsg callee |> snd |> flip Map.find callee_entry_state
            in
            if
              is_recursive && Option.exists exactly_matching_daig ~f:(D.is_solved callee.exit)
              (* case (2) *)
            then
              exactly_matching_daig >>= D.read_by_loc callee.exit
              >>| (fun return_state ->
                    Dom.return ~callee ~caller ~callsite ~caller_state ~return_state ~fields)
              >>| Dom.join acc_poststate
            else
              let callee_entry_state =
                if is_recursive then Dom.widen caller_entry_state callee_entry_state
                else callee_entry_state
              in
              Map.find (snd @@ Map.find_exn dsg callee) callee_entry_state >>= fun daig ->
              if is_recursive && Dom.equal callee_entry_state caller_entry_state (* case (3) *) then
                let _ = Dep.add ~callee ~caller:(nm, caller, caller_entry_state, callsite) in
                match D.read_by_loc exit_loc daig with
                | Some phi -> Some (Dom.join acc_poststate phi)
                | None -> acc_result
              else if (* case (4) *)
                      D.is_solved exit_loc daig then
                let _ = Dep.add ~callee ~caller:(nm, caller, caller_entry_state, callsite) in
                let new_poststate =
                  match D.get_by_loc exit_loc daig with
                  | D.Result return_state, _daig ->
                      Dom.return ~callee ~caller ~callsite ~caller_state ~return_state ~fields
                  | D.Summ_qry _, _daig -> failwith "unreachable due to preceding [is_solved] check"
                in
                Some (Dom.join acc_poststate new_poststate)
              else None)

  let callee_subqueries_of_summ_qry (dsg : t) fields ~callsite ~caller_state ~caller_entry_state
      ~callgraph caller_method =
    let callees = Callgraph.resolve_with_callgraph ~callsite ~caller_method ~callgraph in
    (* generate a subquery for each possible callee without a compatible summary *)
    Set.fold callees ~init:[] ~f:(fun acc callee ->
        let is_exc =
          match callsite with
          | Ast.Stmt.Call _ -> false
          | Ast.Stmt.Exceptional_call _ -> true
          | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)
        in
        let callee_entry_state =
          Dom.call ~callee ~callsite ~caller_state ~fields
          |>
          if Method_id.equal caller_method callee.method_id then Dom.widen caller_entry_state
          else Fn.id
        in
        Map.find_exn dsg callee |> snd |> flip Map.find callee_entry_state
        (* looking here for exactly-matching summaries; could instead find weakly-matching summaries (i.e. with entry state implied by the exact entry state) at the cost of from-scratch consistency *)
        |>
        function
        | Some daig when D.is_solved (if is_exc then callee.exc_exit else callee.exit) daig -> acc
        | None | Some _ ->
            let q : Q.t = { fn = callee; is_exc; entry_state = callee_entry_state } in
            q :: acc)

  let query ~method_id ~entry_state ~loc ~callgraph ~fields dsg : Dom.t * t =
    let fn =
      match Cfg.Fn.Map.fn_by_method_id method_id dsg with
      | None ->
          failwith (Format.asprintf "No source available for method_id %a" Method_id.pp method_id)
      | Some fn -> fn
    in
    (* shorthand for issuing the initial query against the corresponding daig*)
    let issue_root_query h =
      let d, h = materialize_daig ~fn ~entry_state h in
      let res, d =
        D.get_by_loc ~summarizer:(summarize_with_callgraph h fields callgraph fn entry_state) loc d
      in
      (res, set_daig h d fn entry_state)
    in
    (* try getting state at loc directly from sub-DAIG; return if success; process generated summary queries otherwise *)
    let daig_qry_result, dsg = issue_root_query dsg in
    match daig_qry_result with
    | D.Result res -> (res, dsg)
    | D.Summ_qry { callsite; caller_state } -> (
        (* one or more additional summary is needed to analyze [callsite] in [caller_state] *)
        (* next, while (query stack is nonempty) pop, solve, add new queries as needed to analyze transitive callees *)
        let rec solve_subqueries dsg = function
          | [] -> (
              let daig_qry_result, dsg = issue_root_query dsg in
              match daig_qry_result with
              | D.Result _ -> dsg
              | D.Summ_qry { callsite; caller_state } ->
                  let new_qrys =
                    callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~callgraph
                      method_id ~caller_entry_state:entry_state
                  in
                  if List.exists new_qrys ~f:(fun q -> Dom.is_bot q.entry_state) then
                    failwith "got bottom new_qry"
                  else ();
                  if List.is_empty new_qrys then dsg else solve_subqueries dsg new_qrys)
          | qry :: qrys when Dom.is_bot qry.entry_state -> solve_subqueries dsg qrys
          | qry :: qrys -> (
              let callee_daig, dsg = materialize_daig ~fn:qry.fn ~entry_state:qry.entry_state dsg in
              let daig_qry_result, dsg, new_callee_daig =
                let res, new_callee_daig =
                  D.get_by_loc
                    ~summarizer:
                      (summarize_with_callgraph dsg fields callgraph qry.fn qry.entry_state)
                    (Q.exit_loc qry) callee_daig
                in
                (res, set_daig dsg new_callee_daig qry.fn qry.entry_state, new_callee_daig)
              in
              match daig_qry_result with
              | D.Result res ->
                  let recursively_dirtied_daig, needs_requery =
                    Dep.self_loops qry.fn
                    |> Set.fold ~init:(new_callee_daig, false)
                         ~f:(fun (daig, needs_requery) (recursive_dep : Dep.t) ->
                           match D.read_by_name recursive_dep.nm daig with
                           | None -> (daig, needs_requery)
                           | Some return_state ->
                               let new_return_state =
                                 Dom.return ~return_state:res ~fields ~callee:qry.fn ~caller:qry.fn
                                   ~callsite:recursive_dep.callsite
                                   ~caller_state:(D.pred_state_exn recursive_dep.nm daig)
                                 |> Dom.widen return_state
                               in
                               if Dom.equal return_state new_return_state then (daig, needs_requery)
                               else
                                 let daig =
                                   D.write_by_name recursive_dep.nm new_return_state daig
                                 in
                                 (daig, true))
                  in
                  let dsg = set_daig dsg recursively_dirtied_daig qry.fn qry.entry_state in
                  let qrys = if needs_requery then qry :: qrys else qrys in
                  solve_subqueries dsg qrys
              | D.Summ_qry { callsite; caller_state } ->
                  let new_qrys =
                    callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~callgraph
                      qry.fn.method_id ~caller_entry_state:qry.entry_state
                  in
                  solve_subqueries dsg (new_qrys @ qrys))
        in
        let dsg =
          solve_subqueries dsg
            (callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~callgraph method_id
               ~caller_entry_state:entry_state)
        in
        (* requery loc; the callsite that triggered a summary query is now resolvable since solve_subqueries terminated *)
        let _, dsg = materialize_daig ~fn ~entry_state dsg in
        match issue_root_query dsg with
        | D.Result res, dsg -> (res, dsg)
        | _ ->
            failwith
              (Format.asprintf
                 "error: solve_subqueries terminated but more summaries are needed to resolve \
                  query for %a in %a"
                 Method_id.pp method_id Dom.pp entry_state))

  let apply_edit ~cha ~diff loc_map (dsg : t) =
    List.fold diff ~init:(loc_map, dsg) ~f:(fun (loc_map, dsg) ->
        let open Tree_diff in
        function
        | Add_function { method_id = { package; class_name; _ } as method_id; decl; instance_init }
          ->
            (match decl with
            | `Meth_decl md ->
                Cfg_parser.of_method_decl loc_map ~package ~class_name md |> fun res ->
                Option.value_exn
                  ~message:
                    (Format.asprintf "Failed to parse method declaration: %a" Method_id.pp method_id)
                  res
            | `Cons_decl (_, decl, _, body) ->
                Cfg_parser.of_constructor_decl loc_map ~package ~class_name ~instance_init ~cha decl
                  body
            | `Static_init (_, blk) -> Cfg_parser.of_static_init loc_map ~package ~class_name blk
            | _ ->
                failwith
                  "unexpected class body declaration in stmt-diff: not a method, static init \
                   block, or constructor")
            |> fun (loc_map, edges, fn) ->
            let cfg = Graph.create (module Cfg.G) ~edges () in
            (loc_map, Map.add_exn dsg ~key:fn ~data:(cfg, Dom.Map.empty))
        | Delete_function { method_id } -> (
            let loc_map = Loc_map.remove_fn loc_map method_id in
            match Cfg.Fn.Map.fn_by_method_id method_id dsg with
            | Some fn -> (loc_map, Map.remove dsg fn)
            | None ->
                failwith
                  (Format.asprintf "Can't remove function %a: does not exist in DAIG" Method_id.pp
                     method_id))
        | Modify_function { method_id; new_header } -> (
            let new_method_id, formals =
              match new_header with _ -> failwith "todo: Modify_function edit"
            in
            match Cfg.Fn.Map.fn_by_method_id method_id dsg with
            | Some ({ method_id = _; formals = _; locals; entry; exit; exc_exit } as old_fn) ->
                let cfg, daigs = Map.find_exn dsg old_fn in
                let (fn : Cfg.Fn.t) =
                  { method_id = new_method_id; formals; locals; entry; exit; exc_exit }
                in
                (loc_map, Map.(remove dsg old_fn |> add_exn ~key:fn ~data:(cfg, daigs)))
            | None ->
                failwith (Format.asprintf "Can't modify unknown function %a" Method_id.pp method_id)
            )
        | (Add_statements { method_id; _ } as edit)
        | (Modify_statements { method_id; _ } as edit)
        | (Modify_header { method_id; _ } as edit)
        | (Delete_statements { method_id; _ } as edit) -> (
            match Cfg.Fn.Map.fn_by_method_id method_id dsg with
            | None ->
                failwith
                  (Format.asprintf "can't apply to edit to non-existent function %a" Method_id.pp
                     method_id)
            | Some fn ->
                let cfg, daigs = Map.find_exn dsg fn in
                let dsg = dirty_interproc_deps dsg fn in
                let cfg_edit =
                  Tree_diff.apply_edit edit loc_map cfg ~ret:fn.exit ~exc:fn.exc_exit
                in
                let new_daigs =
                  Map.map daigs ~f:(fun daig -> D.apply_edit ~daig ~cfg_edit ~fn edit)
                in
                (cfg_edit.new_loc_map, Map.set dsg ~key:fn ~data:(cfg_edit.cfg, new_daigs))))
end

open Make (Array_bounds)

open Frontend

let%test "single-file interprocedurality with a public-static-void-main" =
  let ({ cfgs; fields; _ } : Cfg_parser.prgm_parse_result) =
    Cfg_parser.parse_file_exn (abs_of_rel_path "test_cases/java/Procedures.java")
  in
  (*Cfg.dump_dot_interproc ~filename:(abs_of_rel_path "procedures.dot") cfgs;*)
  let dsg : t = init ~cfgs in
  let fns = fns dsg in
  let main_fn =
    List.find_exn fns ~f:(fun (fn : Cfg.Fn.t) -> String.equal "main" fn.method_id.method_name)
  in
  let _, dsg = materialize_daig ~fn:main_fn ~entry_state:(Dom.init ()) dsg in
  let callgraph =
    Callgraph.deserialize ~fns
      (Src_file.of_file @@ abs_of_rel_path "test_cases/procedures.callgraph")
  in
  let exit_state, dsg =
    query ~method_id:main_fn.method_id ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~callgraph
      ~fields dsg
  in
  let _ = dump_dot ~filename:(abs_of_rel_path "solved_procedures.dsg.dot") dsg in
  (* hacky check that the computed exit state contains the string of the correct analysis result for the made-up numerical program [Procedures.java] *)
  String.substr_index (Array_bounds.show exit_state) ~pattern:"14_245" |> Option.is_some

let%test "motivating example from SRH'96" =
  let ({ cfgs; fields; _ } : Cfg_parser.prgm_parse_result) =
    Cfg_parser.parse_file_exn (abs_of_rel_path "test_cases/java/Srh.java")
  in
  let dsg : t = init ~cfgs in
  let fns = Cfg.Fn.Map.keys dsg in
  let main_fn =
    List.find_exn fns ~f:(fun (fn : Cfg.Fn.t) -> String.equal "main" fn.method_id.method_name)
  in
  let _, dsg = materialize_daig ~fn:main_fn ~entry_state:(Dom.init ()) dsg in
  let callgraph =
    Callgraph.deserialize ~fns (Src_file.of_file @@ abs_of_rel_path "test_cases/srh.callgraph")
  in
  let _exit_state, dsg =
    query ~method_id:main_fn.method_id ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~callgraph
      ~fields dsg
  in
  let _ = dump_dot ~filename:(abs_of_rel_path "solved_srh.dsg.dot") dsg in
  true

let%test "apply edit to SRH'96 example" =
  let parse = Cfg_parser.parse_file_exn (abs_of_rel_path "test_cases/diff/pre/Srh.java") in
  let dsg = init ~cfgs:parse.cfgs in
  let fns = Cfg.Fn.Map.keys dsg in
  let main_fn =
    List.find_exn fns ~f:(fun (fn : Cfg.Fn.t) -> String.equal "main" fn.method_id.method_name)
  in
  let callgraph =
    Callgraph.deserialize ~fns (Src_file.of_file @@ abs_of_rel_path "test_cases/srh.callgraph")
  in

  let _exit_state, dsg =
    query ~method_id:main_fn.method_id ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~callgraph
      ~fields:parse.fields dsg
  in
  let _ = dump_dot ~filename:(abs_of_rel_path "srh_pre_edit.dot") dsg in
  true

(*
let%test "variadic arguments" =
  let ({ cfgs; fields; cha = _; loc_map = _ } : Cfg_parser.prgm_parse_result) =
    Cfg_parser.parse_file_exn (abs_of_rel_path "test_cases/java/Variadic.java")
  in
  let dsg : t = init ~cfgs in
  let fns = Cfg.Fn.Map.keys dsg in
  let main_fn =
    List.find_exn fns ~f:(fun (fn : Cfg.Fn.t) -> String.equal "main" fn.method_id.method_name)
  in
  let _, dsg = materialize_daig ~fn:main_fn ~entry_state:(Dom.init ()) dsg in
  let callgraph =
    Callgraph.deserialize ~fns (Src_file.of_file @@ abs_of_rel_path "test_cases/varargs.callgraph")
  in
  let _exit_state,dsg =
    query ~method_id:main_fn.method_id ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~callgraph
      ~fields dsg
  in
  let _ = dump_dot ~filename:(abs_of_rel_path "solved_varargs.dsg.dot") dsg in
  true
*)
