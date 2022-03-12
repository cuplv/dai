open Dai.Import
open Domain
open Syntax
open Frontend

module Make (Dom : Abstract.Dom) = struct
  module Dom = Abstract.DomWithDataStructures (Dom)
  module D = Daig.Make (Dom)
  module Q = Query.Make (Dom)

  module Summary = struct
    module Table = struct
      type t = Dom.t Dom.Map.t Cfg.Fn.Map.t

      let reg_summs : t ref = ref Cfg.Fn.Map.empty

      let exc_summs : t ref = ref Cfg.Fn.Map.empty

      let instance exc = !(if exc then exc_summs else reg_summs)

      let set ~exc ~fn ~summs =
        if exc then exc_summs := Map.set !exc_summs ~key:fn ~data:summs
        else reg_summs := Map.set !reg_summs ~key:fn ~data:summs
    end

    let add ?(exc = false) fn ~(pre : Dom.t) ~(post : Dom.t) =
      let summs =
        match Map.find (Table.instance exc) fn with
        | None -> Dom.Map.singleton pre post
        | Some summs -> Map.add_exn summs ~key:pre ~data:post
      in
      Table.set ~exc ~fn ~summs

    let find ?(exc = false) pre fn = Map.find (Table.instance exc) fn >>= flip Map.find pre

    let _find_weakened ?(exc = false) pre fn =
      match Map.find (Table.instance exc) fn with
      | None -> Dom.Map.empty
      | Some summs -> Map.filter_keys summs ~f:(Dom.( <= ) pre)

    let dirty ?exc ?pre fn =
      match (pre, exc) with
      | Some pre, Some exc ->
          Table.set ~exc ~fn ~summs:(Map.remove (Map.find_exn (Table.instance exc) fn) pre)
      | _ ->
          Table.set ~exc:true ~fn ~summs:Dom.Map.empty;
          Table.set ~exc:false ~fn ~summs:Dom.Map.empty
  end

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

    (* Maps summaries to callsites that depend upon them; i.e. map callee functions to callee contexts to callers *)
    let interproc_deps : Set.t Dom.Map.t Cfg.Fn.Map.t ref = ref Cfg.Fn.Map.empty

    let add ~callee ~callee_entry_state ~caller:(nm, fn, ctx, callsite) =
      let caller = { nm; fn; ctx; callsite } in
      let new_callee_deps =
        match Map.find !interproc_deps callee with
        | None -> Dom.Map.singleton callee_entry_state (Set.singleton caller)
        | Some callee_deps ->
            Map.set callee_deps ~key:callee_entry_state
              ~data:
                (match Map.find callee_deps callee_entry_state with
                | None -> Set.singleton caller
                | Some callers -> Set.add callers caller)
      in
      interproc_deps := Map.set !interproc_deps ~key:callee ~data:new_callee_deps

    (* if [ctx] is provided, pop _only_ dependencies on [fn] in that context; otherwise, pop _all_ dependencies on [fn]*)
    let pop ~ctx ~fn =
      match Map.find !interproc_deps fn with
      | None -> Set.empty
      | Some fn_deps -> (
          match ctx with
          | None ->
              interproc_deps := Map.remove !interproc_deps fn;
              Map.fold fn_deps ~init:Set.empty ~f:(fun ~key:_ ~data acc -> Set.union acc data)
          | Some ctx -> (
              match Map.find fn_deps ctx with
              | None -> Set.empty
              | Some fn_ctx_deps ->
                  let fn_deps = Map.remove fn_deps ctx in
                  interproc_deps := Map.set !interproc_deps ~key:fn ~data:fn_deps;
                  fn_ctx_deps))

    let self_loops fn ctx =
      match Map.find !interproc_deps fn >>= flip Map.find ctx with
      | None -> Set.empty
      | Some deps -> Set.filter deps ~f:(fun dep -> Cfg.Fn.equal fn dep.fn)

    let count () =
      Map.fold !interproc_deps ~init:0 ~f:(fun ~key:_ ~data acc ->
          List.fold (Map.data data) ~init:acc ~f:(fun acc deps -> acc + Set.length deps))

    let clear () = interproc_deps := Cfg.Fn.Map.empty

    let cleanup dsg =
      let is_stale { ctx; fn; nm; _ } =
        try
          not @@ Option.is_some
          @@ (Map.find dsg fn >>= (snd >> flip Map.find ctx) >>= D.read_by_name nm)
        with D.Ref_not_found _ -> true
      in
      interproc_deps := Map.map !interproc_deps ~f:(Map.map ~f:(Set.filter ~f:(is_stale >> not)))
  end

  type t = (Cfg.t * D.t Dom.Map.t) Cfg.Fn.Map.t

  (* Each procedure (i.e [Cfg.Fn.t]'s) maps to its [Cfg.t] representation as well as a map from procedure-entry abstract states (i.e. [Dom.t]'s)
     to corresponding DAIGs ([D.t]'s and any interprocedural dependencies thereof ([dep list]'s)*)

  let init ~cfgs : t =
    Dep.clear ();
    Cfg.Fn.Map.map cfgs ~f:(flip pair Dom.Map.empty)

  let add_exn ~cfgs dsg =
    Cfg.Fn.Map.fold cfgs ~init:dsg ~f:(fun ~key ~data dsg ->
        match Map.add dsg ~key ~data:(data, Dom.Map.empty) with
        | `Ok res -> res
        | `Duplicate -> failwith (Format.asprintf "can't add duplicate Cfg.Fn.t: %a" Cfg.Fn.pp key))

  let fns = Cfg.Fn.Map.keys

  let get_cfg_exn dsg fn = fst (Cfg.Fn.Map.find_exn dsg fn)

  let set_daig dsg daig fn entry_state =
    let cfg, daigs = Map.find_exn dsg fn in
    Map.set dsg ~key:fn ~data:(cfg, Map.set daigs ~key:entry_state ~data:daig)

  let _summarize_daig_exn dsg (fn : Cfg.Fn.t) entry_state =
    let cfg, daigs = Map.find_exn dsg fn in
    let daig = Map.find_exn daigs entry_state in
    if D.is_solved fn.exit daig then
      let post = Option.value_exn (D.read_by_loc fn.exit daig) in
      Summary.add ~exc:false fn ~pre:entry_state ~post
    else ();
    if D.is_solved fn.exc_exit daig then
      let post = Option.value_exn (D.read_by_loc fn.exc_exit daig) in
      Summary.add ~exc:true fn ~pre:entry_state ~post
    else ();
    Map.set dsg ~key:fn ~data:(cfg, Map.remove daigs entry_state)

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

  let print_stats fs (dsg : t) =
    let daigs : D.t list = List.bind (Map.data dsg) ~f:(fun (_cfg, daigs) -> Map.data daigs) in
    let procedures : int = Map.count dsg ~f:(snd >> Map.is_empty >> not) in
    let self_loops : int =
      Map.fold dsg ~init:0 ~f:(fun ~key:fn ~data:(_, daigs) acc ->
          List.fold (Map.keys daigs) ~init:acc ~f:(fun acc ctx ->
              acc + (Set.length @@ Dep.self_loops fn ctx)))
    in
    let total_astates : int =
      List.fold daigs ~init:0 ~f:(fun sum daig -> sum + D.total_astate_refs daig)
    in
    let nonemp_astates : int =
      List.fold daigs ~init:0 ~f:(fun sum daig -> sum + D.nonempty_astate_refs daig)
    in
    let total_deps : int = Dep.count () in
    Format.fprintf fs "[EXPERIMENT][STATS] %i, %i, %i, %i, %i, %i\n" (List.length daigs) total_deps
      procedures total_astates nonemp_astates self_loops

  let materialize_daig ~(fn : Cfg.Fn.t) ~(entry_state : Dom.t) (dsg : t) =
    let cfg, daigs = Map.find_exn dsg fn in
    match Map.find daigs entry_state with
    | Some daig -> (daig, dsg)
    | None ->
        (*Format.(
          fprintf err_formatter "[INFO] materializing: %a in %a\n" Cfg.Fn.pp fn Dom.pp entry_state;
          pp_print_flush err_formatter ());*)
        let (daig as data) = D.of_cfg ~entry_state ~cfg ~fn in
        (try D.assert_wf daig
         with _ ->
           Cfg.dump_dot_intraproc ~filename:(abs_of_rel_path "assertwf_fail.cfg.dot") cfg;
           D.dump_dot ~filename:(abs_of_rel_path "assertwf_fail.daig.dot") daig;
           failwith (Format.asprintf "assertwf_fail: %a" Cfg.Fn.pp fn));
        let daigs = Map.add_exn daigs ~key:entry_state ~data in
        (daig, Map.set dsg ~key:fn ~data:(cfg, daigs))

  (* dirty [fn] in [dsg], recursively dirtying its dependencies and affected summaries.
   * [ctx], if provided, restricts dirtying to a particular context
   *)
  let rec dirty_interproc_deps ?ctx dsg fn =
    (match ctx with Some pre -> Summary.dirty ~pre fn | None -> Summary.dirty fn);
    let deps = Dep.pop ~ctx ~fn in
    Set.fold deps ~init:dsg ~f:(fun dsg dep ->
        let dsg =
          match Map.find dsg dep.fn >>= (snd >> flip Map.find dep.ctx) with
          | None -> dsg
          | Some dep_daig ->
              let dep_daig =
                try D.dirty dep.nm dep_daig
                with D.Ref_not_found _ ->
                  (* dependency was in a loop unrolling that has since been dirtied, no harm done *)
                  dep_daig
              in
              D.assert_wf dep_daig;
              set_daig dsg dep_daig dep.fn dep.ctx
        in
        dirty_interproc_deps ~ctx:dep.ctx dsg dep.fn)

  let summarize_with_callgraph (dsg : t) fields (cg : Callgraph.t) (caller : Cfg.Fn.t)
      caller_entry_state ~callsite:(callsite, nm) caller_state =
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
      if
        List.exists callees ~f:(fun callee ->
            (not (Cfg.Fn.equal caller callee))
            && Callgraph.is_mutually_recursive cg.scc caller callee)
      then Some (Dom.top ())
      else
        List.fold
          ~init:(Some (Dom.bottom ()))
          callees
          ~f:(fun acc_result callee ->
            acc_result >>= fun acc_poststate ->
            let callee_entry_state = Dom.call ~callee ~callsite ~caller_state ~fields in
            (* four cases:
               (1) callee entry state is bottom, so no effect on return state
               (2) this is a recursive call and we already have an exactly-matching summary, so no widening needed
               (3) this is a recursive call and widening has converged on the entry state, so we add a self-dependency and continue
               (4) this is a non-recursive call or recursive call yet to converge on its entry state, so attempt to summarize
            *)
            if
              Dom.is_bot callee_entry_state
              (* case (1); do this before widening to avoid needless work *)
            then acc_result
            else
              let is_recursive = Cfg.Fn.equal caller callee in
              let exit_loc = if is_exc then callee.exc_exit else callee.exit in
              let exactly_matching_daig =
                Map.find_exn dsg callee |> snd |> flip Map.find callee_entry_state
              in
              if is_recursive && Option.exists exactly_matching_daig ~f:(D.is_solved callee.exit)
              then
                (* case (2) *)
                exactly_matching_daig >>= D.read_by_loc callee.exit
                >>| (fun return_state ->
                      Dom.return ~callee ~caller ~callsite ~caller_state ~return_state ~fields)
                >>| Dom.join acc_poststate
              else
                let callee_entry_state =
                  if is_recursive then Dom.widen caller_entry_state callee_entry_state
                  else callee_entry_state
                in
                let add_dep = Dep.add ~callee ~callee_entry_state in
                let summary = if is_exc then None else Summary.find callee_entry_state callee in
                match summary with
                | Some poststate ->
                    let _ = add_dep ~caller:(nm, caller, caller_entry_state, callsite) in
                    Some (Dom.join acc_poststate poststate)
                | None ->
                    Map.find (snd @@ Map.find_exn dsg callee) callee_entry_state >>= fun daig ->
                    if
                      is_recursive && Dom.equal callee_entry_state caller_entry_state (* case (3) *)
                    then
                      let _ = add_dep ~caller:(nm, caller, caller_entry_state, callsite) in
                      match D.read_by_loc exit_loc daig with
                      | Some phi -> Some (Dom.join acc_poststate phi)
                      | None -> acc_result
                    else if (* case (4) *)
                            D.is_solved exit_loc daig then
                      let _ = add_dep ~caller:(nm, caller, caller_entry_state, callsite) in
                      let new_poststate =
                        let return_state = Option.value_exn (D.read_by_loc exit_loc daig) in
                        Dom.return ~callee ~caller ~callsite ~caller_state ~return_state ~fields
                      in
                      Some (Dom.join acc_poststate new_poststate)
                    else None)

  let callee_subqueries_of_summ_qry (dsg : t) fields ~callsite ~caller_state ~caller_entry_state
      ~(cg : Callgraph.t) caller_method =
    let callees = Callgraph.callees ~callsite ~caller_method ~cg:cg.forward in
    (* generate a subquery for each possible callee without a compatible summary *)
    List.fold callees ~init:[] ~f:(fun acc callee ->
        let is_exc =
          match callsite with
          | Ast.Stmt.Call _ -> false
          | Ast.Stmt.Exceptional_call _ -> true
          | s -> failwith (Format.asprintf "error: %a is not a callsite" Ast.Stmt.pp s)
        in
        let callee_entry_state =
          Dom.call ~callee ~callsite ~caller_state ~fields
          |>
          if Callgraph.methods_mutually_recursive cg.scc caller_method callee.method_id then
            Dom.widen caller_entry_state
          else Fn.id
        in
        match Summary.find ~exc:is_exc callee_entry_state callee with
        | Some _ -> acc
        | None -> (
            match Map.find_exn dsg callee |> snd |> flip Map.find callee_entry_state with
            (* looking here for exactly-matching summaries; could instead find weakly-matching summaries (i.e. with entry state implied by the exact entry state) at the cost of from-scratch consistency *)
            | Some daig when D.is_solved (if is_exc then callee.exc_exit else callee.exit) daig ->
                acc
            | None | Some _ ->
                let q : Q.t = { fn = callee; is_exc; entry_state = callee_entry_state } in
                q :: acc))

  let query ~fn ~entry_state ~loc ~cg ~fields dsg : Dom.t * t =
    (* shorthand for issuing the initial query against the corresponding daig*)
    let issue_root_query h =
      let d, h = materialize_daig ~fn ~entry_state h in
      let res, d =
        try D.get_by_loc ~summarizer:(summarize_with_callgraph h fields cg fn entry_state) loc d
        with D.Ref_not_found (`By_loc l) ->
          Format.(fprintf err_formatter)
            "Location %a not found in fn %a with (exit: %a; exc_exit: %a)" Cfg.Loc.pp l Cfg.Fn.pp fn
            Cfg.Loc.pp fn.exit Cfg.Loc.pp fn.exc_exit;
          D.dump_dot d ~filename:(abs_of_rel_path "refnotfound.daig.dot");
          failwith "ref not found"
      in
      D.assert_wf d;
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
                    callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~cg
                      fn.method_id ~caller_entry_state:entry_state
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
                    ~summarizer:(summarize_with_callgraph dsg fields cg qry.fn qry.entry_state)
                    (Q.exit_loc qry) callee_daig
                in
                D.assert_wf new_callee_daig;
                (res, set_daig dsg new_callee_daig qry.fn qry.entry_state, new_callee_daig)
              in
              match daig_qry_result with
              | D.Result res ->
                  let recursively_dirtied_daig, needs_requery =
                    Dep.self_loops qry.fn qry.entry_state
                    |> Set.fold ~init:(new_callee_daig, false)
                         ~f:(fun (daig, needs_requery) (recursive_dep : Dep.t) ->
                           match D.read_by_name recursive_dep.nm daig with
                           | None | (exception D.Ref_not_found (`By_name _)) -> (daig, needs_requery)
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
                                 D.assert_wf daig;
                                 (daig, true))
                  in
                  let dsg = set_daig dsg recursively_dirtied_daig qry.fn qry.entry_state in
                  let qrys = if needs_requery then qry :: qrys else qrys in
                  solve_subqueries dsg qrys
              | D.Summ_qry { callsite; caller_state } ->
                  let new_qrys =
                    callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~cg
                      qry.fn.method_id ~caller_entry_state:qry.entry_state
                    |> List.filter ~f:(fun (new_qry : Q.t) ->
                           (not @@ Callgraph.is_mutually_recursive cg.scc qry.fn new_qry.fn)
                           || not
                              @@ (Dom.equal new_qry.entry_state qry.entry_state
                                 && Cfg.Fn.equal qry.fn new_qry.fn))
                  in
                  solve_subqueries dsg (new_qrys @ qrys))
        in
        let dsg =
          solve_subqueries dsg
            (callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~cg fn.method_id
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
                 Method_id.pp fn.method_id Dom.pp entry_state))

  let rec loc_only_query ~(fn : Cfg.Fn.t) ~(loc : Cfg.Loc.t) ~(cg : Callgraph.t)
      ~(fields : Declared_fields.t) ~(entrypoints : Cfg.Fn.t list) (dsg : t) : Dom.t list * t =
    let callers = Callgraph.callers ~callee_method:fn.method_id ~reverse_cg:cg.reverse in
    if List.is_empty callers then (
      (*NOTE(benno): we can just query with \top as entry here soundly, but I want to know if it's happening a lot because it indicates some callgraphissues*)
      if not @@ List.mem entrypoints fn ~equal:Cfg.Fn.equal then
        Format.printf "warning: non-entrypoint function %a has no callers\n" Cfg.Fn.pp fn;
      let res, dsg = query dsg ~fn ~entry_state:(Dom.init ()) ~loc ~cg ~fields in
      ([ res ], dsg))
    else
      let rec_callers, nonrec_callers =
        List.partition_tf callers ~f:(Callgraph.is_mutually_recursive cg.scc fn)
      in
      let entry_states, dsg =
        List.fold nonrec_callers ~init:(Dom.Set.empty, dsg)
          ~f:(fun (acc_entry_states, dsg) caller ->
            let calledges =
              Sequence.filter
                (Cfg.G.edges (get_cfg_exn dsg caller))
                ~f:(Cfg.G.Edge.label >> flip Callgraph.is_syntactically_compatible fn)
            in
            Sequence.fold calledges ~init:(acc_entry_states, dsg)
              ~f:(fun (acc_entry_states, dsg) calledge ->
                let caller_loc = Cfg.G.Edge.src calledge in
                let callsite = Cfg.G.Edge.label calledge in
                let caller_states, dsg =
                  loc_only_query ~fn:caller ~loc:caller_loc ~cg ~fields ~entrypoints dsg
                in
                let acc_entry_states =
                  List.fold caller_states ~init:acc_entry_states ~f:(fun acc caller_state ->
                      Set.add acc (Dom.call ~callee:fn ~callsite ~caller_state ~fields))
                in
                (acc_entry_states, dsg)))
      in
      match rec_callers with
      | [] ->
          (* for non-recursive functions, analyze to [loc] in each reachable [entry_state] *)
          let results, dsg =
            Set.fold entry_states ~init:([], dsg) ~f:(fun (rs, dsg) entry_state ->
                let r, dsg = query dsg ~fn ~entry_state ~loc ~cg ~fields in
                (r :: rs, dsg))
          in
          (results, dsg)
      | _ ->
          (* for recursive functions, analyze to the exit in each reachable [entry_state] first, then gather up all the abstract states at [loc]*)
          let dsg =
            Set.fold entry_states ~init:dsg ~f:(fun dsg entry_state ->
                query dsg ~fn ~entry_state ~loc:fn.exit ~cg ~fields |> snd)
          in
          let results =
            Map.find_exn dsg fn |> snd |> Map.data |> List.filter_map ~f:(D.read_by_loc loc)
          in
          (results, dsg)

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
            | Some fn -> (loc_map, dirty_interproc_deps (Map.remove dsg fn) fn)
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
                (*Map.iter new_daigs ~f:D.assert_wf;*)
                (cfg_edit.new_loc_map, Map.set dsg ~key:fn ~data:(cfg_edit.cfg, new_daigs))))
    $> (snd >> Dep.cleanup)
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
  let cg =
    Callgraph.deserialize ~fns
      (Src_file.of_file @@ abs_of_rel_path "test_cases/procedures.callgraph")
  in
  let exit_state, dsg =
    query ~fn:main_fn ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~cg ~fields dsg
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
  let cg =
    Callgraph.deserialize ~fns (Src_file.of_file @@ abs_of_rel_path "test_cases/srh.callgraph")
  in
  let _exit_state, dsg =
    query ~fn:main_fn ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~cg ~fields dsg
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
  let cg =
    Callgraph.deserialize ~fns (Src_file.of_file @@ abs_of_rel_path "test_cases/srh.callgraph")
  in

  let _exit_state, dsg =
    query ~fn:main_fn ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~cg ~fields:parse.fields dsg
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
