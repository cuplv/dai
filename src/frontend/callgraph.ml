open Dai.Import
open Syntax

type forward_t = Cfg.Fn.t list Method_id.Map.t

type reverse_t = Cfg.Fn.t list Method_id.Map.t

type scc = string Graphlib.Std.partition

type t = { forward : forward_t; reverse : reverse_t; scc : scc option }

let empty = { forward = Method_id.Map.empty; reverse = Method_id.Map.empty; scc = None }

let add ~(caller : Cfg.Fn.t) ~(callee : Cfg.Fn.t) cg =
  let update map m_id fn =
    Map.update map m_id ~f:(function None -> [ fn ] | Some fns -> fn :: fns)
  in
  {
    cg with
    forward = update cg.forward caller.method_id callee;
    reverse = update cg.reverse callee.method_id caller;
  }

(* a callgraph is a map from caller [Method_id]'s to sets of callee [Cfg.Fn]'s *)

let is_syntactically_compatible (callsite : Ast.Stmt.t) (callee : Cfg.Fn.t) =
  let num_formals = List.length callee.formals in
  let compatible_args actuals =
    num_formals = List.length actuals
    || (* WALA callgraph output doesn't know about varargs,
          so we conservatively assume any function whose last argument has array type may be variadic *)
    (num_formals > 0 && String.is_suffix ~suffix:"[]" (List.last_exn callee.method_id.arg_types))
  in
  match callsite with
  | Ast.Stmt.Call { lhs = _; rcvr; meth = "<init>"; actuals; alloc_site = _ }
  | Ast.Stmt.Exceptional_call { rcvr; meth = "<init>"; actuals } ->
      String.equal rcvr (String.split callee.method_id.class_name ~on:'$' |> List.last_exn)
      && String.equal "<init>" callee.method_id.method_name
      && compatible_args actuals
  | Ast.Stmt.Call { lhs = _; rcvr = _; meth; actuals; alloc_site = _ }
  | Ast.Stmt.Exceptional_call { rcvr = _; meth; actuals } ->
      String.equal meth callee.method_id.method_name && compatible_args actuals
  | _ -> false

let callees ~callsite ~caller_method ~(cg : forward_t) =
  let candidates = match Method_id.Map.find cg caller_method with Some cs -> cs | None -> [] in
  List.filter candidates ~f:(is_syntactically_compatible callsite)

(* serialized format is "(caller_line callee_line^* )^*", where
   * a caller_line is "CALLER: <method_id>\n"
   * a callee_line is "\tCALLEE: <method_id>\n"
   * <method_id>'s can be deserialized by [deserialized_method]
   * there exists a call edge to each CALLEE from the preceding CALLER
*)
let deserialize_forward ~fns =
  Src_file.lines
  >> Array.fold ~init:(Method_id.Map.empty, None) ~f:(fun (acc_cg, curr_caller) line ->
         if String.is_prefix line ~prefix:"CALLER: " then
           let caller = String.chop_prefix_exn ~prefix:"CALLER: " line |> Method_id.deserialize in
           (acc_cg, Some caller)
         else
           let caller = Option.value_exn curr_caller in
           let callee_method =
             String.chop_prefix_exn ~prefix:"\tCALLEE: " line |> Method_id.deserialize
           in
           let callee =
             List.find fns ~f:(fun (f : Cfg.Fn.t) -> Method_id.equal f.method_id callee_method)
           in
           match callee with
           | None -> (acc_cg, curr_caller)
           | Some callee ->
               let cg =
                 Method_id.Map.set acc_cg ~key:caller
                   ~data:
                     (match Method_id.Map.find acc_cg caller with
                     | Some callees -> callee :: callees
                     | None -> [ callee ])
               in
               (cg, curr_caller))
  >> fst

let reverse ~(fns : Cfg.Fn.t list) (cg : forward_t) : reverse_t =
  Map.fold cg ~init:Method_id.Map.empty ~f:(fun ~key:caller ~data:callees acc ->
      match List.find fns ~f:(fun (f : Cfg.Fn.t) -> Method_id.equal f.method_id caller) with
      | Some caller_fn ->
          List.fold callees ~init:acc ~f:(fun acc callee ->
              Map.update acc callee.method_id ~f:(function
                | Some callers -> caller_fn :: callers
                | None -> [ caller_fn ]))
      | None -> acc)

module G = Graph.Make (String) (Unit)

let to_graph (cg : forward_t) : G.t =
  let edges =
    Map.fold cg ~init:[] ~f:(fun ~key:caller ~data:callees acc ->
        let caller = Format.asprintf "%a" Method_id.pp caller in
        List.fold callees ~init:acc ~f:(fun acc callee ->
            let callee = Format.asprintf "%a" Method_id.pp callee.method_id in
            (caller, callee, ()) :: acc))
  in
  Graph.create (module G) ~edges ()

let strongly_connected_components (cg : forward_t) : scc =
  Graph.strong_components (module G) (to_graph cg)

let deserialize ~fns file =
  let forward = deserialize_forward ~fns file in
  let reverse = reverse ~fns forward in
  let scc = strongly_connected_components forward in
  { forward; reverse; scc = Some scc }

let callers ~callee_method ~reverse_cg =
  match Map.find reverse_cg callee_method with Some callers -> callers | None -> []

let methods_mutually_recursive scc m1 m2 =
  match scc with
  | None -> Method_id.equal m1 m2
  | Some scc ->
      Graphlib.Std.Partition.equiv scc
        (Format.asprintf "%a" Method_id.pp m1)
        (Format.asprintf "%a" Method_id.pp m2)

let is_mutually_recursive scc (fn1 : Cfg.Fn.t) (fn2 : Cfg.Fn.t) =
  methods_mutually_recursive scc fn1.method_id fn2.method_id

let filter ~fns file =
  let have_fn m_id = List.exists fns ~f:(fun (f : Cfg.Fn.t) -> Method_id.equal f.method_id m_id) in
  Src_file.lines file
  |> Array.fold ~init:None ~f:(fun curr_caller line ->
         if String.is_prefix line ~prefix:"CALLER: " then
           let caller_method =
             String.chop_prefix_exn ~prefix:"CALLER: " line |> Method_id.deserialize
           in
           if have_fn caller_method then (
             Format.print_string (line ^ "\n");
             Some caller_method)
           else None
         else
           match curr_caller with
           | None -> None
           | Some _ ->
               let callee_method =
                 String.chop_prefix_exn ~prefix:"\tCALLEE: " line |> Method_id.deserialize
               in
               if have_fn callee_method then (
                 Format.print_string (line ^ "\n");
                 curr_caller)
               else curr_caller)
  |> ignore

let dump_dot ~filename (cg : forward_t) : unit =
  Graph.to_dot (module G) (to_graph cg) ~filename ~string_of_node:(Format.asprintf "\"%s\"")

let%test "procedures example" =
  let src_file = Src_file.of_file (abs_of_rel_path "test_cases/procedures.callgraph") in
  let fns =
    Cfg_parser.parse_file_exn (abs_of_rel_path "test_cases/java/Procedures.java")
    |> fun { cfgs; _ } -> Map.keys cfgs
  in
  let cg = deserialize_forward ~fns src_file in
  List.length (Map.keys cg) |> Int.equal 3
