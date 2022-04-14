open Dai.Import

module T = struct
  type t = {
    package : string list;
    class_name : string;
    method_name : string;
    static : bool;
    arg_types : string list;
  }
  [@@deriving compare, equal, hash, sexp_of]
  (** NB:
   * [class_name] is "Foo$Bar" for inner class "Bar" of outer class "Foo"
   * [arg_types] are _unqualified_: always "String", never "java.lang.String"
  *)

  let pp fs { package; class_name; method_name; static; arg_types } =
    Format.fprintf fs "%a.%s%s%s(%a)" (List.pp "." String.pp) package class_name
      (if static then "." else "#")
      method_name (List.pp "," String.pp) arg_types
end

module T_comparator : sig
  type t = T.t [@@deriving compare, equal, hash, sexp_of]

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t

  val pp : t pp
end = struct
  include T
  include Comparator.Make (T)
end

module Set = struct
  include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

  type t = Set.M(T_comparator).t
end

module Map = struct
  include (Map : module type of Map with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) Map.t)

  type 'v t = 'v Map.M(T_comparator).t

  let empty = Map.empty (module T_comparator)
end

include T_comparator

let deserialize m : t =
  let open String in
  let static, rest_of_m =
    if is_prefix m ~prefix:"static " then (true, drop_prefix m 7) else (false, m)
  in
  let pkg_and_class_str, rest_of_m =
    match split rest_of_m ~on:'#' with
    | [ before; after ] -> (before, after)
    | _ -> failwith ("malformed serialized method: " ^ m)
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

let current_method_id = ref []

let set_current_method_id (method_id : t) = current_method_id := method_id :: !current_method_id

let clear_current_method_id () = current_method_id := List.tl_exn !current_method_id

let get_current_method_id () = List.hd !current_method_id
