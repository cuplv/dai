open Dai.Import
open Domain
open Syntax
open Frontend

module Make (Dom : Abstract.Dom) (Ctx : Context.Sig) = struct
  (*  module Dom = Abstract.DomWithDataStructures (Dom)*)
  module D = Daig.Make (Dom)
  module Ctx = Ctx

  module Q = struct
    module T = struct
      type t = { fn : Cfg.Fn.t; is_exc : bool; entry_state : Dom.t; ctx : Ctx.t }
      [@@deriving compare, sexp_of]

      let _pp fs { fn; is_exc; entry_state; ctx } =
        Format.fprintf fs "QRY[%s(%a) in context %a with entry_state %a]"
          (if is_exc then "exc_exit" else "exit")
          Cfg.Fn.pp fn Ctx.pp ctx Dom.pp entry_state

      let exit_loc { fn; is_exc; _ } = if is_exc then fn.exc_exit else fn.exit
    end

    module T_comp = struct
      include Comparator.Make (T)
      include T
    end

    include T_comp

    module Set = struct
      include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

      type t = Set.M(T_comp).t [@@deriving compare]
    end
  end

  type t = (Cfg.t * D.t Ctx.Map.t) Cfg.Fn.Map.t

  (* Each procedure (i.e [Cfg.Fn.t]'s) maps to its [Cfg.t] representation as well as a map from procedure-entry abstract states (i.e. [Dom.t]'s)
     to corresponding DAIGs ([D.t]'s and any interprocedural dependencies thereof ([dep list]'s)*)

  let init ~cfgs : t = Cfg.Fn.Map.map cfgs ~f:(flip pair Ctx.Map.empty)

  let add_exn ~cfgs dsg =
    Cfg.Fn.Map.fold cfgs ~init:dsg ~f:(fun ~key ~data dsg ->
        match Map.add dsg ~key ~data:(data, Ctx.Map.empty) with
        | `Ok res -> res
        | `Duplicate -> failwith (Format.asprintf "can't add duplicate Cfg.Fn.t: %a" Cfg.Fn.pp key))

  let fns = Cfg.Fn.Map.keys

  let set_daig dsg daig fn ctx =
    let cfg, daigs = Map.find_exn dsg fn in
    Map.set dsg ~key:fn ~data:(cfg, Map.set daigs ~key:ctx ~data:daig)

  let dump_dot ~filename (dsg : t) =
    let daigs : (Cfg.Fn.t * (Ctx.t * D.t)) list =
      Cfg.Fn.Map.fold dsg ~init:[] ~f:(fun ~key:fn ~data:(_, daigs) acc ->
          List.map (Map.to_alist daigs) ~f:(pair fn) @ acc)
    in
    let _ = Sys.command "mkdir -p scratch" in
    List.iteri daigs ~f:(fun idx (fn, (ctx, daig)) ->
        let filename = "scratch/" ^ Int.to_string idx ^ ".dot" in
        let loc_labeller l =
          if Cfg.Loc.equal l fn.entry then
            Some (Format.asprintf "ENTRY[%a]@CTX[%a]: " Method_id.pp fn.method_id Ctx.pp ctx)
          else None
        in
        D.dump_dot ~filename ~loc_labeller daig);
    let _ = Sys.command ("dot scratch/*.dot | gvpack 2>/dev/null > " ^ filename) in
    let _ = Sys.command "rm -r scratch" in
    ()

  let print_stats fs (dsg : t) =
    let daigs : D.t list = List.bind (Map.data dsg) ~f:(fun (_cfg, daigs) -> Map.data daigs) in
    let procedures : int = Map.count dsg ~f:(snd >> Map.is_empty >> not) in
    let self_loops : int =
      (* Map.fold dsg ~init:0 ~f:(fun ~key:fn ~data:(_, daigs) acc ->
           List.fold (Map.keys daigs) ~init:acc ~f:(fun acc ctx ->
               acc + (Set.length @@ Dep.self_loops fn ctx)))*)
      -1
    in
    let total_astates : int =
      List.fold daigs ~init:0 ~f:(fun sum daig -> sum + D.total_astate_refs daig)
    in
    let nonemp_astates : int =
      List.fold daigs ~init:0 ~f:(fun sum daig -> sum + D.nonempty_astate_refs daig)
    in
    let total_deps : int = (*Dep.count ()*) -1 in
    Format.fprintf fs "[EXPERIMENT][STATS] %i, %i, %i, %i, %i, %i\n" (List.length daigs) total_deps
      procedures total_astates nonemp_astates self_loops

  let materialize_daig ~(fn : Cfg.Fn.t) ~(ctx : Ctx.t) ~(entry_state : Dom.t) (dsg : t) =
    let cfg, daigs = Map.find_exn dsg fn in
    match Map.find daigs ctx with
    | Some daig -> (daig, dsg)
    | None ->
        (*Format.(
          fprintf err_formatter "[INFO] materializing: %a in %a\n" Cfg.Fn.pp fn Dom.pp entry_state;
          pp_print_flush err_formatter ());*)
        let (daig as data) = D.of_cfg ~entry_state ~cfg ~fn in
        let daigs = Map.add_exn daigs ~key:ctx ~data in
        (daig, Map.set dsg ~key:fn ~data:(cfg, daigs))

  exception No_entry_state of Cfg.Fn.t * Ctx.t

  let summarize_with_callgraph (dsg : t) fields (cg : Callgraph.t) (caller : Cfg.Fn.t)
      (caller_ctx : Ctx.t) ~callsite:(callsite, _nm) caller_state =
    let open Option.Monad_infix in
    let callees = Callgraph.callees ~callsite ~caller_method:caller.method_id ~cg:cg.forward in
    if List.is_empty callees then Some (Dom.approximate_missing_callee ~caller_state ~callsite)
    else
      let is_exc =
        match callsite with
        | Ast.Stmt.Call _ -> false
        | Ast.Stmt.Exceptional_call _ -> true
        | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)
      in
      List.fold
        ~init:(Some (Dom.bottom ()))
        callees
        ~f:(fun acc_result callee ->
          acc_result >>= fun acc_poststate ->
          let callee_entry_state = Dom.call ~callee ~caller ~callsite ~caller_state ~fields in
          let callee_ctx = Ctx.callee_ctx ~callsite ~caller_ctx in
          (* four cases:
             (1) callee entry state is bottom, so no effect on return state
             (2) the callee's entry state includes callee_entry_state and its exit is available
             (3) the call is recursive and there is no new dataflow to the entry, so we propagate and continue
             (4) the callee does not exist or does not yet cover callee_entry_state, so we need to do more analysis there
          *)
          if Dom.is_bot callee_entry_state then (* (1) *)
            acc_result
          else
            let exit_loc = if is_exc then callee.exc_exit else callee.exit in
            Map.find_exn dsg callee |> snd |> flip Map.find callee_ctx >>= fun callee_daig ->
            let old_callee_entry_state =
              match D.read_by_loc callee.entry callee_daig with
              | Some phi -> phi
              | None -> raise (No_entry_state (callee, callee_ctx))
            in
            let is_new_dataflow = not @@ Dom.implies callee_entry_state old_callee_entry_state in
            let is_solved = D.is_solved exit_loc callee_daig in
            (*Format.(fprintf err_formatter)
                "Attempting to summarize %a in context %a\n" Cfg.Fn.pp callee Ctx.pp callee_ctx;
              Format.(fprintf err_formatter)
                "\tcallee_entry_state: %a\n\
                 \told_callee_entry_st:%a\n\
                 \tis_new_dataflow: %b\n\
                 \tis_solved: %b\n"
                Dom.pp callee_entry_state Dom.pp old_callee_entry_state is_new_dataflow is_solved;*)
            if (not is_new_dataflow) && is_solved then
              (* (2) *)
              D.read_by_loc exit_loc callee_daig
              >>| (fun return_state ->
                    Dom.return ~callee ~caller ~callsite ~caller_state ~return_state ~fields)
              >>| Dom.join acc_poststate
            else if (not is_new_dataflow) && (not is_solved) && Cfg.Fn.equal caller callee then
              (* (3) *)
              acc_result
            else (* (4) *)
              None)
  (*$> fun res ->
    Format.(fprintf err_formatter)
      "\tgot %a from summarizing %a in context %a\n" (Option.pp Dom.pp) res Ast.Stmt.pp callsite
      Ctx.pp caller_ctx*)

  let callee_subqueries_of_summ_qry (dsg : t) fields ~callsite ~caller_state ~caller_ctx
      ~(cg : Callgraph.t) caller_method : t * Q.t list =
    let callees = Callgraph.callees ~callsite ~caller_method ~cg:cg.forward in
    (* generate a subquery for each possible callee that needs further analysis *)
    List.fold callees ~init:(dsg, []) ~f:(fun ((acc_dsg, acc_qrys) as acc) callee ->
        let is_exc =
          match callsite with
          | Ast.Stmt.Call _ -> false
          | Ast.Stmt.Exceptional_call _ -> true
          | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)
        in
        let caller =
          Map.keys dsg
          |> List.find_exn ~f:(fun (fn : Cfg.Fn.t) -> Method_id.equal fn.method_id caller_method)
        in
        let qry_callee_entry_state = Dom.call ~caller ~callee ~callsite ~caller_state ~fields in
        let callee_ctx = Ctx.callee_ctx ~callsite ~caller_ctx in
        match Map.find_exn dsg callee |> snd |> flip Map.find callee_ctx with
        | Some daig ->
            let curr_callee_entry_state =
              match D.read_by_loc callee.entry daig with
              | Some phi -> phi
              | None -> failwith "malformed DAIG: entry state can not be empty"
            in
            let entry_state =
              if Method_id.equal caller_method callee.method_id then
                Dom.widen curr_callee_entry_state qry_callee_entry_state
              else Dom.join curr_callee_entry_state qry_callee_entry_state
            in
            let is_new_dataflow = not @@ Dom.implies entry_state curr_callee_entry_state in
            if
              (not is_new_dataflow)
              && D.is_solved (if is_exc then callee.exc_exit else callee.exit) daig
            then acc
            else
              let acc_dsg =
                if is_new_dataflow then
                  (*Format.(fprintf err_formatter) "\twriting new_entry to %a\n" Cfg.Fn.pp callee;*)
                  let daig = D.write_by_loc callee.entry entry_state daig in
                  set_daig acc_dsg daig callee callee_ctx
                else acc_dsg
              in
              (acc_dsg, ({ fn = callee; is_exc; entry_state; ctx = callee_ctx } : Q.t) :: acc_qrys)
        | None ->
            ( acc_dsg,
              ({ fn = callee; is_exc; entry_state = qry_callee_entry_state; ctx = callee_ctx }
                : Q.t)
              :: acc_qrys ))

  let query_impl ~fn ~ctx ~entry_state ~loc ~cg ~fields dsg : Dom.t * t =
    (* shorthand for issuing the initial query against the corresponding daig*)
    let issue_root_query h =
      let d, h = materialize_daig ~fn ~ctx ~entry_state h in
      let res, d =
        try D.get_by_loc ~summarizer:(summarize_with_callgraph h fields cg fn ctx) loc d
        with D.Ref_not_found _ ->
          D.dump_dot d ~filename:(abs_of_rel_path "refnotfound.daig.dot");
          failwith "ref not found"
      in
      (res, set_daig h d fn ctx)
    in
    (* try getting state at loc directly from sub-DAIG; return if success; process generated summary queries otherwise *)
    let daig_qry_result, dsg = issue_root_query dsg in
    match daig_qry_result with
    | D.Result res -> (res, dsg)
    | D.Summ_qry { callsite; returnsite = _; caller_state } -> (
        (* one or more additional summary is needed to analyze [callsite] in [caller_state] *)
        (* next, while (query stack is nonempty) pop, solve, add new queries as needed to analyze transitive callees *)
        let rec solve_subqueries dsg = function
          | [] -> (
              let daig_qry_result, dsg = issue_root_query dsg in
              match daig_qry_result with
              | D.Result _ -> dsg
              | D.Summ_qry { callsite; returnsite = _; caller_state } ->
                  let dsg, new_qrys =
                    callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~cg
                      fn.method_id ~caller_ctx:ctx
                  in
                  if List.exists new_qrys ~f:(fun q -> Dom.is_bot q.entry_state) then
                    failwith "got bottom new_qry"
                  else ();
                  if List.is_empty new_qrys then dsg else solve_subqueries dsg new_qrys)
          | qry :: qrys when Dom.is_bot qry.entry_state -> solve_subqueries dsg qrys
          | qry :: qrys -> (
              let callee_daig, dsg =
                materialize_daig ~fn:qry.fn ~ctx:qry.ctx ~entry_state:qry.entry_state dsg
              in
              let daig_qry_result, dsg, callee_daig =
                let res, new_callee_daig =
                  D.get_by_loc
                    ~summarizer:(summarize_with_callgraph dsg fields cg qry.fn qry.ctx)
                    (Q.exit_loc qry) callee_daig
                in
                D.assert_wf new_callee_daig;
                (res, set_daig dsg new_callee_daig qry.fn qry.ctx, new_callee_daig)
              in
              match daig_qry_result with
              | D.Result exit_state ->
                  let rec_return_sites =
                    D.recursive_call_return_sites callee_daig ~cg ~self:qry.fn
                  in
                  let recursively_dirtied_daig, needs_requery =
                    List.fold rec_return_sites ~init:(callee_daig, false)
                      ~f:(fun ((daig, _) as acc) (callsite, retsite_nm) ->
                        match D.read_by_name retsite_nm daig with
                        | None | (exception D.Ref_not_found _) ->
                            (* Format.(
                               fprintf err_formatter "no value found at retsite_nm: %a\n" D.Name.pp
                                 retsite_nm);*)
                            acc
                            (* this can happen if dirtying in one retsite clobbers an unrolled loop iteration containing another retsite *)
                        | Some retsite_state ->
                            (* Format.(
                               fprintf err_formatter "\tvalue %a found at retsite_nm: %a\n" Dom.pp
                                 retsite_state D.Name.pp retsite_nm);*)
                            let caller_state = D.pred_state_exn retsite_nm daig in
                            let returned_state =
                              Dom.return ~callee:qry.fn ~caller:qry.fn ~callsite ~caller_state
                                ~fields ~return_state:exit_state
                            in
                            if Dom.(returned_state <= retsite_state) then acc
                            else
                              let new_retsite_state = Dom.widen retsite_state returned_state in
                              assert (not (Dom.equal new_retsite_state retsite_state));
                              let daig = D.write_by_name retsite_nm new_retsite_state daig in
                              D.assert_wf daig;
                              (daig, true))
                  in
                  let dsg = set_daig dsg recursively_dirtied_daig qry.fn qry.ctx in
                  let qrys = if needs_requery then qry :: qrys else qrys in
                  solve_subqueries dsg qrys
              | D.Summ_qry { callsite; returnsite; caller_state } ->
                  (*Format.(fprintf err_formatter)
                    "\tGOT SUMM QRY callsite: %a caller_state: %a\n" Ast.Stmt.pp callsite Dom.pp
                    caller_state;*)
                  let dsg, new_qrys =
                    callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~cg
                      ~caller_ctx:qry.ctx qry.fn.method_id
                  in
                  (*Format.(fprintf err_formatter)
                    "\tproduced new queries:\n\t\t%a\n" (List.pp "\n\t\t" Q._pp) new_qrys;*)
                  (* For each new_qry, there are four cases:
                     (1) it is for results in this DAIG and we've converged at the entry, so propagate bottom over the call, add [new_qry]
                     (2) it is for results in this DAIG and we haven't converged, so propagate flow back to the procedure entry, add [new_qry]
                     (3) it is for a mutually recursive procedure.  propagate top over the call, since we can't handle that yet, don't add [new_qry]
                     (4) it is not recursive in any way -- leave the current DAIG unchanged and add [new_qry] to subquery queue
                  *)
                  let dsg, new_qrys =
                    List.fold new_qrys ~init:(dsg, []) ~f:(fun (acc_dsg, acc_qrys) new_qry ->
                        let is_recursive =
                          Ctx.equal new_qry.ctx qry.ctx && Cfg.Fn.equal new_qry.fn qry.fn
                        in
                        let is_mutually_recursive =
                          Callgraph.is_mutually_recursive cg.scc new_qry.fn qry.fn
                        in
                        if is_recursive then
                          let daig =
                            Map.find_exn acc_dsg qry.fn |> snd |> flip Map.find_exn qry.ctx
                          in
                          let new_daig =
                            match D.read_by_loc qry.fn.entry daig with
                            | Some old_entry_state ->
                                if Dom.implies new_qry.entry_state old_entry_state then
                                  (* (1) *)
                                  D.write_by_name returnsite (Dom.bottom ()) daig
                                else
                                  (* (2) *)
                                  D.write_by_loc qry.fn.entry
                                    (Dom.widen old_entry_state new_qry.entry_state)
                                    daig
                            | None ->
                                (* also (2) *) D.write_by_loc qry.fn.entry new_qry.entry_state daig
                          in
                          let new_dsg = set_daig acc_dsg new_daig qry.fn qry.ctx in
                          (* commit DAIG changes and add new_qry for cases (1),(2) *)
                          (new_dsg, new_qry :: acc_qrys)
                        else if is_mutually_recursive then
                          (* (3) *)
                          let daig =
                            Map.find_exn acc_dsg qry.fn |> snd |> flip Map.find_exn qry.ctx
                            |> D.write_by_name returnsite (Dom.top ())
                          in
                          let acc_dsg = set_daig acc_dsg daig qry.fn qry.ctx in
                          (acc_dsg, acc_qrys)
                        else (acc_dsg, new_qry :: acc_qrys))
                  in
                  solve_subqueries dsg (new_qrys @ qrys))
        in
        let dsg, new_queries =
          callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~cg fn.method_id
            ~caller_ctx:ctx
        in
        let dsg = solve_subqueries dsg new_queries in
        (* requery loc; the callsite that triggered a summary query is now resolvable since solve_subqueries terminated *)
        let _, dsg = materialize_daig ~fn ~ctx ~entry_state dsg in
        match issue_root_query dsg with
        | D.Result res, dsg -> (res, dsg)
        | _ ->
            failwith
              (Format.asprintf
                 "error: solve_subqueries terminated but more summaries are needed to resolve \
                  query for %a in %a"
                 Method_id.pp fn.method_id Dom.pp entry_state))

  let rec query ~fn ~ctx ~entry_state ~loc ~cg ~fields dsg : Dom.t * t =
    try query_impl ~fn ~ctx ~entry_state ~loc ~cg ~fields dsg
    with No_entry_state (blocked_fn, blocked_ctx) ->
      let entry_state, dsg =
        get_entry_state ~fields ~entrypoints:[] dsg blocked_fn blocked_ctx cg
      in
      let dsg =
        match Map.find_exn dsg blocked_fn |> snd |> flip Map.find blocked_ctx with
        | Some daig ->
            let daig = D.write_by_loc blocked_fn.entry entry_state daig in
            set_daig dsg daig blocked_fn blocked_ctx
        | None -> materialize_daig ~fn:blocked_fn ~ctx:blocked_ctx ~entry_state dsg |> snd
      in
      query ~fn ~ctx ~entry_state ~loc ~cg ~fields dsg

  and loc_only_query ~(fn : Cfg.Fn.t) ~(loc : Cfg.Loc.t) ~(cg : Callgraph.t)
      ~(fields : Declared_fields.t) ~(entrypoints : Cfg.Fn.t list) (dsg : t) :
      (Dom.t * Ctx.t) list * t =
    let callers = Callgraph.callers ~callee_method:fn.method_id ~reverse_cg:cg.reverse in
    if List.is_empty callers then (
      (*NOTE(benno): we can just query with \top as entry here soundly, but I want to know if it's happening a lot because it indicates some callgraphissues*)
      if not (List.mem entrypoints fn ~equal:Cfg.Fn.equal) then
        Format.printf "warning: non-entrypoint function %a has no callers\n" Cfg.Fn.pp fn;

      let ctx = Ctx.init () in
      let res, dsg = query dsg ~fn ~entry_state:(Dom.init ()) ~ctx ~loc ~cg ~fields in
      ([ (res, ctx) ], dsg))
    else
      let rec_callers, nonrec_callers =
        List.partition_tf callers ~f:(Callgraph.is_mutually_recursive cg.scc fn)
      in
      let entry_states_and_contexts, dsg =
        List.fold nonrec_callers ~init:([], dsg) ~f:(fun (acc_entries, dsg) caller ->
            let calledges =
              Sequence.filter
                (Cfg.G.edges (Cfg.Fn.Map.find_exn dsg caller |> fst))
                ~f:(Cfg.G.Edge.label >> flip Callgraph.is_syntactically_compatible fn)
            in
            Sequence.fold calledges ~init:(acc_entries, dsg) ~f:(fun (acc_entries, dsg) calledge ->
                let caller_loc = Cfg.G.Edge.src calledge in
                let callsite = Cfg.G.Edge.label calledge in
                let caller_states, dsg =
                  loc_only_query ~fn:caller ~loc:caller_loc ~cg ~fields ~entrypoints dsg
                in
                let acc_entries =
                  List.fold caller_states ~init:acc_entries
                    ~f:(fun acc (caller_state, caller_ctx) ->
                      let callee_ctx = Ctx.callee_ctx ~callsite ~caller_ctx in
                      let entry_state =
                        Dom.call ~caller ~callee:fn ~callsite ~caller_state ~fields
                      in
                      (entry_state, callee_ctx) :: acc)
                in
                (acc_entries, dsg)))
      in
      let states_by_context =
        List.fold entry_states_and_contexts ~init:Ctx.Map.empty ~f:(fun acc (state, ctx) ->
            match Map.find acc ctx with
            | Some acc_state -> Map.set acc ~key:ctx ~data:(Dom.join acc_state state)
            | None -> Map.add_exn acc ~key:ctx ~data:state)
      in
      match rec_callers with
      | [] ->
          (* for non-recursive functions, analyze to [loc] in each reachable [entry_state] *)
          Map.fold states_by_context ~init:([], dsg) ~f:(fun ~key:ctx ~data:entry_state (rs, dsg) ->
              let r, dsg = query dsg ~fn ~ctx ~entry_state ~loc ~cg ~fields in

              ((r, ctx) :: rs, dsg))
      | _ ->
          (* for recursive functions, analyze to the exit in each reachable [entry_state] first, then gather up all the abstract states at [loc]*)
          let dsg =
            Map.fold states_by_context ~init:dsg ~f:(fun ~key:ctx ~data:entry_state dsg ->
                query dsg ~fn ~ctx ~entry_state ~loc:fn.exit ~cg ~fields |> snd)
          in
          let results =
            Map.find_exn dsg fn |> snd |> Map.to_alist
            |> List.filter_map ~f:(fun (ctx, daig) -> D.read_by_loc loc daig >>| flip pair ctx)
          in
          (results, dsg)

  and get_entry_state ~fields ~entrypoints (dsg : t) (fn : Cfg.Fn.t) (ctx : Ctx.t)
      (cg : Callgraph.t) =
    let callers = Callgraph.callers ~callee_method:fn.method_id ~reverse_cg:cg.reverse in
    let _rec_callers, nonrec_callers =
      List.partition_tf callers ~f:(Callgraph.is_mutually_recursive cg.scc fn)
    in
    (* doing this in an ugly imperative way to avoid stack overflows *)
    let res : Dom.t ref = ref (Dom.bottom ()) in
    let dsg : t ref = ref dsg in
    let callsites =
      Sequence.of_list nonrec_callers
      |> Sequence.bind ~f:(fun caller ->
             Sequence.filter
               (Cfg.G.edges (Cfg.Fn.Map.find_exn !dsg caller |> fst))
               ~f:(Cfg.G.Edge.label >> flip Callgraph.is_syntactically_compatible fn)
             |> Sequence.map ~f:(pair caller))
    in
    Sequence.iter callsites ~f:(fun (caller, calledge) ->
        let caller_loc = Cfg.G.Edge.src calledge in
        let callsite = Cfg.G.Edge.label calledge in
        let caller_states, updated_dsg =
          loc_only_query ~fn:caller ~loc:caller_loc ~cg ~fields ~entrypoints !dsg
        in
        dsg := updated_dsg;
        List.iter caller_states ~f:(fun (caller_state, caller_ctx) ->
            if Ctx.(equal ctx @@ callee_ctx ~callsite ~caller_ctx) then
              let entry_state = Dom.call ~caller ~callee:fn ~callsite ~caller_state ~fields in
              res := Dom.join !res entry_state));
    (!res, !dsg)

  (*
    let entry_states_and_contexts, _dsg =
      List.fold nonrec_callers ~init:([], dsg) ~f:(fun (acc_entries, dsg) caller ->
          let calledges =
            Sequence.filter
              (Cfg.G.edges (Cfg.Fn.Map.find_exn dsg caller |> fst))
              ~f:(Cfg.G.Edge.label >> flip Callgraph.is_syntactically_compatible fn)
          in
          Sequence.fold calledges ~init:(acc_entries, dsg) ~f:(fun (acc_entries, dsg) calledge ->
              let caller_loc = Cfg.G.Edge.src calledge in
              let callsite = Cfg.G.Edge.label calledge in
              let caller_states, dsg =
                loc_only_query ~fn:caller ~loc:caller_loc ~cg ~fields ~entrypoints dsg
              in
              let acc_entries =
                List.fold caller_states ~init:acc_entries ~f:(fun acc (caller_state, caller_ctx) ->
                    let callee_ctx = Ctx.callee_ctx ~callsite ~caller_ctx in
                    let entry_state = Dom.call ~caller ~callee:fn ~callsite ~caller_state ~fields in
                    (entry_state, callee_ctx) :: acc)
              in
              (acc_entries, dsg)))
    in

    List.filter entry_states_and_contexts ~f:(snd >> Ctx.equal ctx)
    |> List.fold ~init:(Dom.bottom ()) ~f:(fun acc (state, _) -> Dom.join acc state)
*)
  let rec dirty_interproc dsg ~(fn : Cfg.Fn.t) ~loc ~ctx ~(cg : Callgraph.t) =
    (*Format.(fprintf err_formatter)
      "[DIRTYING] %a from %a in %a\n" Cfg.Fn.pp fn Cfg.Loc.pp loc Ctx.pp ctx;*)
    match Map.find dsg fn >>= (snd >> flip Map.find ctx) with
    | Some daig when Option.is_none (D.read_by_loc loc daig) -> dsg
    | Some daig ->
        let affected_callsites = D.reachable_callsites loc daig in
        let dsg = set_daig dsg (D.dirty_by_loc loc daig) fn ctx in
        let dsg =
          List.fold affected_callsites ~init:dsg ~f:(fun dsg callsite ->
              let callee_ctx = Ctx.callee_ctx ~callsite ~caller_ctx:ctx in
              let callees =
                Callgraph.callees ~callsite ~cg:cg.forward ~caller_method:fn.method_id
              in
              List.fold callees ~init:dsg ~f:(fun acc callee ->
                  dirty_interproc acc ~fn:callee ~loc:callee.entry ~ctx:callee_ctx ~cg))
        in
        let callers = Callgraph.callers ~callee_method:fn.method_id ~reverse_cg:cg.reverse in
        List.fold callers ~init:dsg ~f:(fun dsg caller ->
            let calledges =
              Sequence.filter
                (Cfg.G.edges (Cfg.Fn.Map.find_exn dsg caller |> fst))
                ~f:(Cfg.G.Edge.label >> flip Callgraph.is_syntactically_compatible fn)
            in
            let callsites =
              Sequence.fold calledges ~init:[] ~f:(fun acc calledge ->
                  let return_loc = Cfg.G.Edge.dst calledge in
                  let callsite = Cfg.G.Edge.label calledge in
                  (return_loc, callsite) :: acc)
            in
            let caller_contexts = Map.find_exn dsg caller |> snd |> Map.keys in
            List.fold caller_contexts ~init:dsg ~f:(fun dsg caller_ctx ->
                List.fold callsites ~init:dsg ~f:(fun dsg (loc, callsite) ->
                    if Ctx.(callee_ctx ~callsite ~caller_ctx |> equal ctx) then
                      dirty_interproc dsg ~fn:caller ~loc ~ctx:caller_ctx ~cg
                    else dsg)))
    | None -> dsg

  let apply_edit ~cha ~cg ~diff loc_map (dsg : t) =
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
            (loc_map, Map.add_exn dsg ~key:fn ~data:(cfg, Ctx.Map.empty))
        | Delete_function { method_id } -> (
            let loc_map = Loc_map.remove_fn loc_map method_id in
            match Cfg.Fn.Map.fn_by_method_id method_id dsg with
            | Some fn ->
                let dsg =
                  Map.find_exn dsg fn |> snd |> Map.keys
                  |> List.fold ~init:dsg ~f:(fun dsg ctx ->
                         dirty_interproc dsg ~fn ~loc:fn.entry ~ctx ~cg)
                in
                (loc_map, dsg)
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
        | Modify_header _ -> failwith "todo: modify_header"
        | (Add_statements { method_id; at_loc = loc; _ } as edit)
        | (Modify_statements { method_id; to_loc = loc; _ } as edit)
        | (Delete_statements { method_id; to_loc = loc; _ } as edit) -> (
            match Cfg.Fn.Map.fn_by_method_id method_id dsg with
            | None ->
                failwith
                  (Format.asprintf "can't apply to edit to non-existent function %a" Method_id.pp
                     method_id)
            | Some fn ->
                let cfg, daigs = Map.find_exn dsg fn in
                let dsg =
                  Map.find_exn dsg fn |> snd |> Map.keys
                  |> List.fold ~init:dsg ~f:(fun dsg ctx -> dirty_interproc dsg ~fn ~loc ~ctx ~cg)
                in
                let cfg_edit =
                  Tree_diff.apply_edit edit loc_map cfg ~ret:fn.exit ~exc:fn.exc_exit
                in

                let new_daigs =
                  Map.map daigs ~f:(fun daig -> D.apply_edit ~daig ~cfg_edit ~fn edit)
                in
                (cfg_edit.new_loc_map, Map.set dsg ~key:fn ~data:(cfg_edit.cfg, new_daigs))))
end

module Dom = Array_bounds
module Ctx = Context.OneCFA

open Make (Dom) (Ctx)

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
  let _, dsg = materialize_daig ~fn:main_fn ~ctx:(Ctx.init ()) ~entry_state:(Dom.init ()) dsg in
  let cg =
    Callgraph.deserialize ~fns
      (Src_file.of_file @@ abs_of_rel_path "test_cases/procedures.callgraph")
  in
  let exit_state, dsg =
    query ~fn:main_fn ~ctx:(Ctx.init ()) ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~cg ~fields
      dsg
  in
  let _ = dump_dot ~filename:(abs_of_rel_path "solved_procedures.csdsg.dot") dsg in
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
  let _, dsg = materialize_daig ~fn:main_fn ~ctx:(Ctx.init ()) ~entry_state:(Dom.init ()) dsg in
  let cg =
    Callgraph.deserialize ~fns (Src_file.of_file @@ abs_of_rel_path "test_cases/srh.callgraph")
  in
  let _exit_state, dsg =
    query ~fn:main_fn ~ctx:(Ctx.init ()) ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~cg ~fields
      dsg
  in
  let _ = dump_dot ~filename:(abs_of_rel_path "solved_srh.csdsg.dot") dsg in
  true

let%test "apply edit to SRH'96 example" =
  let parse = Cfg_parser.parse_file_exn (abs_of_rel_path "test_cases/diff/pre/Srh.java") in
  let dsg = init ~cfgs:parse.cfgs in
  let fns = Cfg.Fn.Map.keys dsg in
  let main_fn =
    List.find_exn fns ~f:(fun (fn : Cfg.Fn.t) -> String.equal "main" fn.method_id.method_name)
  in
  let cg =
    Callgraph.deserialize ~fns (Src_file.of_file @@ abs_of_rel_path "test_cases/srh.callgraph")
  in

  let _exit_state, dsg =
    query ~fn:main_fn ~ctx:(Ctx.init ()) ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~cg
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
