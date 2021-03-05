open D1a
open Import
open Cfg_parser
open Ast

module Arr_bounds_check = struct
  module Dom = Context.Make2CFA (Array_bounds)
  include Daig.Make (Dom)

  let check_all_accesses (daig : t) fs =
    let _, daig = get_by_name (Name.Loc (Cfg.Loc.exit, Dom.Ctx.init)) daig in
    let candidates : ((Expr.t * Expr.t) list * Ref.t) Seq.t =
      Seq.filter_map
        (G.nodes (fst daig))
        ~f:(function
          | Ref.Stmt { stmt; name = _ } as refcell -> (
              match Array_bounds.array_accesses stmt with
              | [] -> None
              | accesses -> Some (accesses, refcell) )
          | _ -> None)
    in
    Seq.fold candidates ~init:daig ~f:(fun daig (accesses, stmt_ref) ->
        let astates_at_stmt_entry =
          Seq.bind
            (G.Node.succs stmt_ref (fst daig))
            ~f:(fun poststate_ref ->
              Seq.filter ~f:Ref.is_astate (G.Node.preds poststate_ref (fst daig)))
        in
        List.fold accesses ~init:daig ~f:(fun daig ->
          function
          | Expr.Var rcvr, field ->
              Seq.fold astates_at_stmt_entry ~init:daig ~f:(fun daig ->
                function
                | Stmt _ -> failwith "unreachable by preceding filter on Ref.is_astate"
                | AState _ as astate_ref ->
                    let astate_ref, daig = fixpoint_of astate_ref daig in
                    let _ = Ref.astate_exn astate_ref in
                    Format.fprintf fs
                      ( match Array_bounds.is_safe rcvr field (Ref.astate_exn astate_ref) with
                      | Some true ->
                          "SAFE\t (Array access %s[%a] of statement %a at %a, context %a)\n"
                      | Some false ->
                          "UNSAFE\t (Array access %s[%a] of statement %a at %a, context %a)\n"
                      | None ->
                          "UNKNOWN\t (Array access %s[%a] of statement %a at %a, context %a)\n" )
                      rcvr Expr.pp field Stmt.pp (Ref.stmt_exn stmt_ref) Name.pp (Ref.name stmt_ref)
                      Dom.Ctx.pp
                      ((fun x -> Option.value_exn x) @@ Name.ctx (Ref.name astate_ref));
                    daig)
          | rcvr, field ->
              Format.fprintf fs
                "UNKNOWN\t(Array access %a[%a] of statement at %a has too complex of a receiver)\n"
                Expr.pp rcvr Expr.pp field Name.pp (Ref.name stmt_ref);
              daig))
end

let test_array_accesses id =
  let cfg = (Util.test_case >> json_of_file >> cfg_of_json) id in
  let daig = Arr_bounds_check.of_cfg cfg in
  Arr_bounds_check.dump_dot daig ~filename:(Util.daig_output id);
  let fs =
    Unix.openfile ~mode:[ Unix.O_WRONLY; Unix.O_CREAT ] (Util.log_output id)
    |> Unix.out_channel_of_descr |> Format.formatter_of_out_channel
  in
  let daig = Arr_bounds_check.check_all_accesses daig fs in
  Arr_bounds_check.dump_dot daig ~filename:(Util.daig_output (id ^ "_post"))

let%test "interproc buckets tests" =
  test_array_accesses "buckets_contains";
  test_array_accesses "buckets_equals";
  true

(*let%test "intraproc buckets tests" =
  test_array_accesses "buckets_swap1";
  test_array_accesses "buckets_swap2";
  test_array_accesses "buckets_swap3";
  test_array_accesses "buckets_swap4";
  test_array_accesses "buckets_contains1";
  test_array_accesses "buckets_contains2";
  test_array_accesses "buckets_contains3";
  test_array_accesses "buckets_contains4";
  test_array_accesses "buckets_contains5";
  test_array_accesses "buckets_equals1";
  test_array_accesses "buckets_equals2";
  test_array_accesses "buckets_equals3";
  test_array_accesses "buckets_equals4";
  test_array_accesses "buckets_equals5";
  test_array_accesses "buckets_equals6";
  test_array_accesses "buckets_indexof1";
  test_array_accesses "buckets_indexof2";
  test_array_accesses "buckets_indexof3";
  test_array_accesses "buckets_indexof4";
  test_array_accesses "buckets_indexof5";
  test_array_accesses "buckets_indexof6";
  test_array_accesses "buckets_indexof7";
  test_array_accesses "buckets_indexof8";
  true
 *)
