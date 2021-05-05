open Dai.Import

type t = Memloc.t String.Map.t [@@deriving compare, equal]

let pp fs (env : t) =
  String.Map.to_alist env |> List.pp ~pre:"{" ~suf:"}" ", " (pp_pair String.pp Memloc.pp) fs

(* I'm sure there's a better way to expose String.Map functionality -- this is quick and dirty *)

let add_exn = String.Map.add_exn

let empty = String.Map.empty

let find = String.Map.find

let find_exn = String.Map.find_exn

let fold = String.Map.fold

let keys = String.Map.keys

let mem = String.Map.mem

let of_alist_exn = String.Map.of_alist_exn

let remove = String.Map.remove

let to_alist = String.Map.to_alist

let update = String.Map.update
