open Dai.Import

module Test (Dom : Domain.Abstract.Dom) = struct
	module Daig = Analysis.Daig.Make (Dom)
	module Dsg = Analysis.Dsg.Make (Dom)
	
	let dname = "usertest/"

  (* JB: this code is mostly copied from src/analysis/daig.ml *)
	let test_simple fname = 
		let ({ cfgs; _ } : Frontend.Cfg_parser.prgm_parse_result) =
		  Frontend.Cfg_parser.parse_file_exn (abs_of_rel_path (dname ^ fname ^ ".java"))
		in
		Map.to_alist cfgs
		|> List.iter ~f:(fun (fn, cfg) ->
		       let daig = Daig.of_cfg ~entry_state:(Dom.init ()) ~cfg ~fn in
		       let fname_ = fname ^ "_" in
		       Daig.dump_dot ~filename:(abs_of_rel_path (fname_ ^ fn.method_id.method_name ^ ".dot")) daig;
		       let _, analyzed_daig = Daig.get_by_loc fn.exit daig in
		       Daig.dump_dot
		         ~filename:(abs_of_rel_path ("analyzed_" ^ fname_ ^ fn.method_id.method_name ^ ".dot"))
		         analyzed_daig);
		true
end

(* Domain modules we've tried:
- Domain.Itv
- Domain.Array_bounds
*)

module TestInt = Test (Domain.Itv)
module TestArrBounds = Test (Domain.Array_bounds)

let%test "User test: simple sum and if with intervals" =
  TestInt.test_simple "Sum"
  
let%test "User test: simple static arrays with array bounds" =
  TestArrBounds.test_simple "ArrayFun"

(* JB: I can't figure out where to get a callgraph.
It seems that tests in src/analysis/dsg.ml rely on existing .callgraph files. *)
(*
let%test "User test: interprocedural" =
  let ({ cfgs; fields; _ } : Frontend.Cfg_parser.prgm_parse_result) =
    Frontend.Cfg_parser.parse_file_exn (abs_of_rel_path ("jbtest/" ^ fname ^ ".java"))
  in
  let dsg (*: t*) = Dsg.init ~cfgs in
  let fns = Syntax.Cfg.Fn.Map.keys dsg in
  let main_fn =
    List.find_exn fns ~f:(fun (fn : Syntax.Cfg.Fn.t) -> String.equal "main" fn.method_id.method_name)
  in
  let _, dsg = Dsg.materialize_daig ~fn:main_fn ~entry_state:(Dom.init ()) dsg in
  let cg =
    Frontend.Callgraph.deserialize ~fns (Frontend.Src_file.of_file @@ abs_of_rel_path (dname ^ fname ^ ".callgraph"))
  in
  let _exit_state, dsg =
    Dsg.query ~fn:main_fn ~entry_state:(Dom.init ()) ~loc:main_fn.exit ~cg ~fields dsg
  in
  let _ = Dsg.dump_dot ~filename:(abs_of_rel_path ("solved_" ^ fname ^ ".dsg.dot")) dsg in
  true
*)
