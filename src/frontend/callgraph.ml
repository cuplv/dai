open Dai.Import
open Syntax

type t = Cfg.Fn.t list Method_id.Map.t

type reverse_t = Cfg.Fn.t list Method_id.Map.t

type bidirectional = { forward : t; reverse : reverse_t }

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

let resolve_with_callgraph ~callsite ~caller_method ~(callgraph : t) =
  let candidates =
    match Method_id.Map.find callgraph caller_method with Some cs -> cs | None -> []
  in
  List.filter candidates ~f:(is_syntactically_compatible callsite)

let deserialize_method m : Method_id.t =
  let open String in
  let static, rest_of_m =
    if is_prefix m ~prefix:"static " then (true, drop_prefix m 7) else (false, m)
  in
  let pkg_and_class_str, rest_of_m =
    match split rest_of_m ~on:'#' with
    | [ before; after ] -> (before, after)
    | _ -> failwith "malformed serialized method: %s"
  in
  let package = deserialize_package pkg_and_class_str in
  let class_name = deserialize_class pkg_and_class_str in
  let method_name, arg_types =
    match split rest_of_m ~on:'(' with
    | [ meth; args_and_close_paren ] ->
        let args =
          sub args_and_close_paren ~pos:0 ~len:(length args_and_close_paren - 1)
          |> split ~on:','
          |> List.filter ~f:(String.is_empty >> not)
        in
        let arg_types =
          List.map args ~f:(fun arg_type ->
              let last_dot_idx = rindex arg_type '.' in
              match last_dot_idx with
              | Some idx -> drop_prefix arg_type (idx + 1)
              | None -> arg_type)
        in
        (meth, arg_types)
    | _ -> failwith ("malformed serialized method: " ^ m)
  in
  { package; class_name; method_name; static; arg_types }

(* serialized format is "(caller_line callee_line^* )^*", where
   * a caller_line is "CALLER: <method_id>\n"
   * a callee_line is "\tCALLEE: <method_id>\n"
   * <method_id>'s can be deserialized by [deserialized_method]
   * there exists a call edge to each CALLEE from the preceding CALLER
*)
let deserialize ~fns =
  Src_file.lines
  >> Array.fold ~init:(Method_id.Map.empty, None) ~f:(fun (acc_cg, curr_caller) line ->
         if String.is_prefix line ~prefix:"CALLER: " then
           let caller = String.chop_prefix_exn ~prefix:"CALLER: " line |> deserialize_method in
           (acc_cg, Some caller)
         else
           let caller = Option.value_exn curr_caller in
           let callee_method =
             String.chop_prefix_exn ~prefix:"\tCALLEE: " line |> deserialize_method
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

let reverse ~(fns : Cfg.Fn.t list) (cg : t) : reverse_t =
  Map.fold cg ~init:Method_id.Map.empty ~f:(fun ~key:caller ~data:callees acc ->
      let caller =
        List.find_exn fns ~f:(fun (f : Cfg.Fn.t) -> Method_id.equal f.method_id caller)
      in
      List.fold callees ~init:acc ~f:(fun acc callee ->
          Map.update acc callee.method_id ~f:(function
            | Some callers -> caller :: callers
            | None -> [ caller ])))

let callers ~callee_method ~reverse_cg =
  match Map.find reverse_cg callee_method with Some callers -> callers | None -> []

module G = Graph.Make (String) (Unit)

let dump_dot ~filename (cg : t) : unit =
  let to_graph (cg : t) : G.t =
    let edges =
      Map.fold cg ~init:[] ~f:(fun ~key:caller ~data:callees acc ->
          let caller = Format.asprintf "%a" Method_id.pp caller in
          List.fold callees ~init:acc ~f:(fun acc callee ->
              let callee = Format.asprintf "%a" Method_id.pp callee.method_id in
              (caller, callee, ()) :: acc))
    in
    Graph.create (module G) ~edges ()
  in
  Graph.to_dot (module G) (to_graph cg) ~filename

let%test "procedures example" =
  let src_file = Src_file.of_file (abs_of_rel_path "test_cases/procedures.callgraph") in
  let fns =
    Cfg_parser.parse_file_exn (abs_of_rel_path "test_cases/java/Procedures.java")
    |> fun { cfgs; _ } -> Map.keys cfgs
  in
  let cg : t = deserialize ~fns src_file in
  List.length (Map.keys cg) |> Int.equal 3
