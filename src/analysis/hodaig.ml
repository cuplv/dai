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

      let pp fs dep =
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

    let interproc_deps : Set.t Dom.Map.t Cfg.Fn.Map.t ref = ref Cfg.Fn.Map.empty

    let add ~summary:(summary_fn, summary_ctx) ~callsite:(nm, fn, ctx, callsite) =
      let key, data =
        match Map.find !interproc_deps summary_fn with
        | None ->
            let summary_deps =
              Dom.Map.singleton summary_ctx (Set.singleton { nm; fn; ctx; callsite })
            in
            (summary_fn, summary_deps)
        | Some deps ->
            let summary_deps =
              Map.find deps summary_ctx |> Option.value ~default:Set.empty
              |> flip Set.add { nm; fn; ctx; callsite }
            in
            (summary_fn, Map.set deps ~key:summary_ctx ~data:summary_deps)
      in
      interproc_deps := Map.set !interproc_deps ~key ~data

    let _pop ~summary:(summary_fn, summary_ctx) =
      match Map.find !interproc_deps summary_fn with
      | None -> Set.empty
      | Some fn_deps -> (
          match Map.find fn_deps summary_ctx with
          | None -> Set.empty
          | Some deps when Set.is_empty deps -> Set.empty
          | Some deps ->
              let new_fn_deps = Map.set fn_deps ~key:summary_ctx ~data:Set.empty in
              interproc_deps := Map.set !interproc_deps ~key:summary_fn ~data:new_fn_deps;
              deps )

    let self_loops fn ctx =
      match Map.find !interproc_deps fn with
      | None -> Set.empty
      | Some fn_deps -> (
          match Map.find fn_deps ctx with
          | None -> Set.empty
          | Some deps ->
              Set.filter deps ~f:(fun dep -> Cfg.Fn.equal fn dep.fn && Dom.equal ctx dep.ctx) )
  end

  type t = (Cfg.t * D.t Dom.Map.t) Cfg.Fn.Map.t

  (* Each procedure (i.e [Cfg.Fn.t]'s) maps to its [Cfg.t] representation as well as a map from procedure-entry abstract states (i.e. [Dom.t]'s)
     to corresponding DAIGs ([D.t]'s and any interprocedural dependencies thereof ([dep list]'s)*)

  let init ~(cfgs : Cfg.t Cfg.Fn.Map.t) : t = Cfg.Fn.Map.map cfgs ~f:(flip pair Dom.Map.empty)

  let set_daig hodaig daig fn entry_state =
    let cfg, daigs = Map.find_exn hodaig fn in
    Map.set hodaig ~key:fn ~data:(cfg, Map.set daigs ~key:entry_state ~data:daig)

  let dump_dot ~filename (hodaig : t) =
    let daigs : (Cfg.Fn.t * D.t) list =
      Cfg.Fn.Map.fold hodaig ~init:[] ~f:(fun ~key:fn ~data:(_, daigs) acc ->
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

  let materialize_daig ~(fn : Cfg.Fn.t) ~(entry_state : Dom.t) (hodaig : t) =
    let cfg, daigs = Map.find_exn hodaig fn in
    match Map.find daigs entry_state with
    | Some daig -> (daig, hodaig)
    | None ->
        let (daig as data) = D.of_cfg ~entry_state ~cfg ~fn in
        let daigs = Map.add_exn daigs ~key:entry_state ~data in
        (daig, Map.set hodaig ~key:fn ~data:(cfg, daigs))

  let summarize_with_callgraph (hodaig : t) fields callgraph (caller : Cfg.Fn.t) caller_entry_state
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
             (2) we already have an exactly-matching summary, so no widening needed
             (3) this is a recursive call and widening has converged on the entry state, so we add a self-dependency and continue
             (4) this is a non-recursive call or recursive call yet to converge on its entry state, so attempt to summarize
          *)
          if
            Dom.is_bot callee_entry_state
            (* case (1); do this before widening to avoid needless work *)
          then acc_result
          else
            let exactly_matching_daig =
              Map.find_exn hodaig callee |> snd |> flip Map.find callee_entry_state
            in
            if Option.exists exactly_matching_daig ~f:(D.is_solved callee.exit) then
              exactly_matching_daig >>= D.read_by_loc callee.exit >>| Dom.join acc_poststate
            else
              let is_recursive = Method_id.equal caller.method_id callee.method_id in
              let exit_loc = if is_exc then callee.exc_exit else callee.exit in
              let callee_entry_state =
                if is_recursive then Dom.widen caller_entry_state callee_entry_state
                else callee_entry_state
              in
              Map.find (snd @@ Map.find_exn hodaig callee) callee_entry_state >>= fun daig ->
              if is_recursive && Dom.equal callee_entry_state caller_entry_state (* case (3) *) then
                let _ =
                  Dep.add ~summary:(caller, caller_entry_state)
                    ~callsite:(nm, caller, caller_entry_state, callsite)
                in
                match D.read_by_loc exit_loc daig with
                | Some phi -> Some (Dom.join acc_poststate phi)
                | None -> acc_result
              else if (* case (4) *)
                      D.is_solved exit_loc daig then
                let _ =
                  Dep.add ~summary:(callee, callee_entry_state)
                    ~callsite:(nm, caller, caller_entry_state, callsite)
                in
                let new_poststate =
                  match D.get_by_loc exit_loc daig with
                  | D.Result return_state, _daig ->
                      Dom.return ~callee ~caller ~callsite ~caller_state ~return_state ~fields
                  | D.Summ_qry _, _daig -> failwith "unreachable due to preceding [is_solved] check"
                in
                Some (Dom.join acc_poststate new_poststate)
              else None)

  let callee_subqueries_of_summ_qry (hodaig : t) fields ~callsite ~caller_state ~caller_entry_state
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
        Map.find_exn hodaig callee |> snd |> flip Map.find callee_entry_state
        (* looking here for exactly-matching summaries; could instead find weakly-matching summaries (i.e. with entry state implied by the exact entry state) at the cost of from-scratch consistency *)
        |>
        function
        | Some daig when D.is_solved (if is_exc then callee.exc_exit else callee.exit) daig -> acc
        | None | Some _ ->
            let q : Q.t = { fn = callee; is_exc; entry_state = callee_entry_state } in
            q :: acc)

  let query ~method_id ~entry_state ~loc ~callgraph ~fields hodaig : Dom.t * t =
    let fn =
      match Cfg.Fn.Map.fn_by_method_id method_id hodaig with
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
    let daig_qry_result, hodaig = issue_root_query hodaig in
    match daig_qry_result with
    | D.Result res -> (res, hodaig)
    | D.Summ_qry { callsite; caller_state } -> (
        (* one or more additional summary is needed to analyze [callsite] in [caller_state] *)
        (* next, while (query stack is nonempty) pop, solve, add new queries as needed to analyze transitive callees *)
        let rec solve_subqueries hodaig qrys ~gas : t =
          if gas <= 0 then (
            dump_dot ~filename:(abs_of_rel_path "hodaig_outta_gas.dot") hodaig;
            failwith "outta gas" )
          else
            match qrys with
            | [] -> (
                let daig_qry_result, hodaig = issue_root_query hodaig in
                match daig_qry_result with
                | D.Result _ -> hodaig
                | D.Summ_qry { callsite; caller_state } ->
                    let new_qrys =
                      callee_subqueries_of_summ_qry hodaig fields ~callsite ~caller_state ~callgraph
                        method_id ~caller_entry_state:entry_state
                    in
                    if List.exists new_qrys ~f:(fun q -> Dom.is_bot q.entry_state) then
                      failwith "got bottom new_qry"
                    else ();
                    if List.is_empty new_qrys then hodaig
                    else solve_subqueries hodaig new_qrys ~gas:(gas - 1) )
            | qry :: qrys when Dom.is_bot qry.entry_state ->
                solve_subqueries hodaig qrys ~gas:(gas - 1)
            | qry :: qrys -> (
                let callee_daig, hodaig =
                  materialize_daig ~fn:qry.fn ~entry_state:qry.entry_state hodaig
                in
                Format.(fprintf std_formatter) "HANDLING QRY: %a\n" Q.pp qry;
                Format.print_flush ();
                let daig_qry_result, hodaig, new_callee_daig =
                  let res, new_callee_daig =
                    D.get_by_loc
                      ~summarizer:
                        (summarize_with_callgraph hodaig fields callgraph qry.fn qry.entry_state)
                      (Q.exit_loc qry) callee_daig
                  in
                  (res, set_daig hodaig new_callee_daig qry.fn qry.entry_state, new_callee_daig)
                in
                match daig_qry_result with
                | D.Result res ->
                    let recursively_dirtied_daig, needs_requery =
                      Dep.self_loops qry.fn qry.entry_state
                      |> Set.fold ~init:(new_callee_daig, false)
                           ~f:(fun (daig, needs_requery) (recursive_dep : Dep.t) ->
                             Format.(fprintf std_formatter)
                               "\tPROCESSING RECURSIVE DEP: %a\n" Dep.pp recursive_dep;
                             match D.read_by_name recursive_dep.nm daig with
                             | None -> (daig, needs_requery)
                             | Some return_state ->
                                 let new_return_state =
                                   Dom.return ~return_state:res ~fields ~callee:qry.fn
                                     ~caller:qry.fn ~callsite:recursive_dep.callsite
                                     ~caller_state:(D.pred_state_exn recursive_dep.nm daig)
                                   |> Dom.widen return_state
                                 in
                                 Format.(fprintf std_formatter)
                                   "\t\tNEW RETURN STATE: %a\n\
                                    \t\tQUERY RESULT: %a\n\
                                    \t\tWIDEN RESULT: %a\n"
                                   Dom.pp new_return_state Dom.pp res Dom.pp
                                   (Dom.widen return_state new_return_state);
                                 if Dom.equal return_state new_return_state then
                                   (daig, needs_requery)
                                 else
                                   let daig =
                                     D.write_by_name recursive_dep.nm new_return_state daig
                                   in
                                   (daig, true))
                    in
                    let hodaig = set_daig hodaig recursively_dirtied_daig qry.fn qry.entry_state in
                    let qrys = if needs_requery then qry :: qrys else qrys in
                    solve_subqueries hodaig qrys ~gas:(gas - 1)
                | D.Summ_qry { callsite; caller_state } ->
                    let new_qrys =
                      callee_subqueries_of_summ_qry hodaig fields ~callsite ~caller_state ~callgraph
                        qry.fn.method_id ~caller_entry_state:qry.entry_state
                    in
                    solve_subqueries hodaig (new_qrys @ qrys) ~gas:(gas - 1) )
        in
        let hodaig =
          solve_subqueries hodaig
            (callee_subqueries_of_summ_qry hodaig fields ~callsite ~caller_state ~callgraph
               method_id ~caller_entry_state:entry_state)
            ~gas:100
        in
        (* requery loc; the callsite that triggered a summary query is now resolvable since solve_subqueries terminated *)
        let _, hodaig = materialize_daig ~fn ~entry_state hodaig in
        match issue_root_query hodaig with
        | D.Result res, hodaig -> (res, hodaig)
        | _ ->
            failwith
              (Format.asprintf
                 "error: solve_subqueries terminated but more summaries are needed to resolve \
                  query for %a in %a"
                 Method_id.pp method_id Dom.pp entry_state) )

  let apply_edit ~diff loc_map (hodaig : t) =
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
            (loc_map, Map.add_exn hodaig ~key:fn ~data:(cfg, Dom.Map.empty))
        | Delete_function { method_id } -> (
            let loc_map = Loc_map.remove_fn loc_map method_id in
            match Cfg.Fn.Map.fn_by_method_id method_id hodaig with
            | Some fn -> (loc_map, Map.remove hodaig fn)
            | None ->
                failwith
                  (Format.asprintf "Can't remove function %a: does not exist in DAIG" Method_id.pp
                     method_id) )
        | Modify_function { method_id; new_header } -> (
            let new_method_id, formals =
              match new_header with _ -> failwith "todo: Modify_function edit"
            in
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
                  Map.map daigs ~f:(fun daig -> D.apply_edit ~daig ~cfg_edit ~fn edit)
                in
                (cfg_edit.new_loc_map, Map.set hodaig ~key:fn ~data:(cfg, new_daigs)) ))
end

module H = Make (Array_bounds)
open Frontend

(*
let%test "single-file interprocedurality with a public-static-void-main" =
  let ({ cfgs; fields; cha; loc_map = _ } : Cfg_parser.prgm_parse_result) =
    Cfg_parser.of_file_exn (abs_of_rel_path "test_cases/java/Procedures.java")
  in
  let fields = Class_hierarchy.compute_closure ~cha ~fields in
  (*Cfg.dump_dot_interproc ~filename:(abs_of_rel_path "procedures.dot") cfgs;*)
  let h : H.t = H.init ~cfgs in
  let fns = Cfg.Fn.Map.keys h in
  let main_fn =
    List.find_exn fns ~f:(fun (fn : Cfg.Fn.t) -> String.equal "main" fn.method_id.method_name)
  in
  let _, h = H.materialize_daig ~fn:main_fn ~entry_state:(H.Dom.init ()) h in
  let callgraph =
    Callgraph.deserialize ~fns
      (Src_file.of_file @@ abs_of_rel_path "test_cases/procedures.callgraph")
  in
  let exit_state, h =
    H.query ~method_id:main_fn.method_id ~entry_state:(H.Dom.init ()) ~loc:main_fn.exit ~callgraph
      ~fields h
  in
  let _ = H.dump_dot ~filename:(abs_of_rel_path "solved_procedures.hodaig.dot") h in
  (* hacky check that the computed exit state contains the string of the correct analysis result for the made-up numerical program [Procedures.java] *)
  String.substr_index (Array_bounds.show exit_state) ~pattern:"14245" |> Option.is_some
*)
let%test "motivating example from SRH'96" =
  let ({ cfgs; fields; cha; loc_map = _ } : Cfg_parser.prgm_parse_result) =
    Cfg_parser.of_file_exn (abs_of_rel_path "test_cases/java/Srh.java")
  in
  let fields = Class_hierarchy.compute_closure ~cha ~fields in
  let h : H.t = H.init ~cfgs in
  let fns = Cfg.Fn.Map.keys h in
  let main_fn =
    List.find_exn fns ~f:(fun (fn : Cfg.Fn.t) -> String.equal "main" fn.method_id.method_name)
  in
  let _, h = H.materialize_daig ~fn:main_fn ~entry_state:(H.Dom.init ()) h in
  let callgraph =
    Callgraph.deserialize ~fns (Src_file.of_file @@ abs_of_rel_path "test_cases/srh.callgraph")
  in
  let _exit_state, h =
    H.query ~method_id:main_fn.method_id ~entry_state:(H.Dom.init ()) ~loc:main_fn.exit ~callgraph
      ~fields h
  in
  let _ = H.dump_dot ~filename:(abs_of_rel_path "solved_srh.hodaig.dot") h in
  true
