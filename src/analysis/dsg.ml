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
      (* Map.fold dsg ~init:0 ~f:(fun ~key:fn ~data:(_, daigs) acc ->
           List.fold (Map.keys daigs) ~init:acc ~f:(fun acc ctx ->
               acc + (Set.length @@ Dep.self_loops fn ctx)))*)
      failwith "todo"
    in
    let total_astates : int =
      List.fold daigs ~init:0 ~f:(fun sum daig -> sum + D.total_astate_refs daig)
    in
    let nonemp_astates : int =
      List.fold daigs ~init:0 ~f:(fun sum daig -> sum + D.nonempty_astate_refs daig)
    in
    let total_deps : int = (*Dep.count ()*) failwith "todo" in
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
          (* three cases:
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
              | None -> failwith "malformed DAIG: entry state can not be empty"
            in
            let is_new_dataflow = not @@ Dom.implies callee_entry_state old_callee_entry_state in
            let is_solved = D.is_solved exit_loc callee_daig in
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

  let callee_subqueries_of_summ_qry (dsg : t) fields ~callsite ~caller_state ~caller_ctx ~cg
      caller_method =
    let callees = Callgraph.callees ~callsite ~caller_method ~cg in
    (* generate a subquery for each possible callee that needs further analysis *)
    List.fold callees ~init:[] ~f:(fun acc callee ->
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
            else ({ fn = callee; is_exc; entry_state; ctx = callee_ctx } : Q.t) :: acc
        | None ->
            ({ fn = callee; is_exc; entry_state = qry_callee_entry_state; ctx = callee_ctx } : Q.t)
            :: acc)

  let query ~fn ~ctx ~entry_state ~loc ~cg ~fields dsg : Dom.t * t =
    (* shorthand for issuing the initial query against the corresponding daig*)
    let issue_root_query h =
      let d, h = materialize_daig ~fn ~ctx ~entry_state h in
      let res, d =
        try D.get_by_loc ~summarizer:(summarize_with_callgraph h fields cg fn ctx) loc d
        with D.Ref_not_found (`By_loc l) ->
          Format.(fprintf err_formatter)
            "Location %a not found in fn %a with (exit: %a; exc_exit: %a)" Cfg.Loc.pp l Cfg.Fn.pp fn
            Cfg.Loc.pp fn.exit Cfg.Loc.pp fn.exc_exit;
          D.dump_dot d ~filename:(abs_of_rel_path "refnotfound.daig.dot");
          failwith "ref not found"
      in
      (res, set_daig h d fn ctx)
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
                    callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~cg:cg.forward
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
                        | None -> acc
                        | exception D.Ref_not_found _ ->
                            acc
                            (* this can happen if dirtying in one retsite clobbers an unrolled loop iteration containing another retsite *)
                        | Some retsite_state ->
                            let caller_state = D.pred_state_exn retsite_nm daig in
                            let returned_state =
                              Dom.return ~callee:qry.fn ~caller:qry.fn ~callsite ~caller_state
                                ~fields ~return_state:exit_state
                            in
                            if Dom.(returned_state <= retsite_state) then acc
                            else
                              let new_retsite_state = Dom.widen retsite_state returned_state in
                              let daig = D.write_by_name retsite_nm new_retsite_state daig in
                              D.assert_wf daig;
                              (daig, true))
                  in
                  let dsg = set_daig dsg recursively_dirtied_daig qry.fn qry.ctx in
                  let qrys = if needs_requery then qry :: qrys else qrys in
                  solve_subqueries dsg qrys
              | D.Summ_qry { callsite; caller_state } ->
                  let new_qrys =
                    callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~cg:cg.forward
                      ~caller_ctx:qry.ctx qry.fn.method_id
                  in
                  let dsg =
                    List.fold new_qrys ~init:dsg ~f:(fun dsg { fn; ctx; entry_state; _ } ->
                        if Ctx.equal ctx qry.ctx && Cfg.Fn.equal fn qry.fn then
                          let daig =
                            Map.find_exn dsg fn |> snd |> flip Map.find_exn ctx
                            |> D.write_by_loc fn.entry entry_state
                          in
                          set_daig dsg daig fn ctx
                        else dsg)
                  in
                  solve_subqueries dsg (new_qrys @ qrys))
        in
        let dsg =
          solve_subqueries dsg
            (callee_subqueries_of_summ_qry dsg fields ~callsite ~caller_state ~cg:cg.forward
               fn.method_id ~caller_ctx:ctx)
        in
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

  let rec loc_only_query ~(fn : Cfg.Fn.t) ~(loc : Cfg.Loc.t) ~(cg : Callgraph.t)
      ~(fields : Declared_fields.t) ~(entrypoints : Cfg.Fn.t list) (dsg : t) :
      (Dom.t * Ctx.t) list * t =
    if List.mem entrypoints fn ~equal:Cfg.Fn.equal then
      let ctx = Ctx.init () in
      let res, dsg = query dsg ~fn ~cg ~entry_state:(Dom.init ()) ~ctx ~loc ~fields in
      ([ (res, ctx) ], dsg)
    else
      let callers = Callgraph.callers ~callee_method:fn.method_id ~reverse_cg:cg.reverse in
      if List.is_empty callers then (
        (*NOTE(benno): we can just query with \top as entry here soundly, but I want to know if it's happening a lot because it indicates some callgraphissues*)
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
              Sequence.fold calledges ~init:(acc_entries, dsg)
                ~f:(fun (acc_entries, dsg) calledge ->
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
            Map.fold states_by_context ~init:([], dsg)
              ~f:(fun ~key:ctx ~data:entry_state (rs, dsg) ->
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
            (loc_map, Map.add_exn dsg ~key:fn ~data:(cfg, Ctx.Map.empty))
        | Delete_function { method_id } -> (
            let loc_map = Loc_map.remove_fn loc_map method_id in
            match Cfg.Fn.Map.fn_by_method_id method_id dsg with
            | Some _fn -> (loc_map, failwith "dirty_interproc_deps (Map.remove dsg fn) fn")
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
                let dsg = failwith "dirty_interproc_deps dsg fn" in
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
