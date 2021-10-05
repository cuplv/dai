open Dai.Import

type t = String.Set.t String.Map.t
(** for each application class, keep a set of instance field names on that class *)

let pp : t pp = Map.pp String.pp (Set.pp String.pp)

let empty = String.Map.empty

let merge : t -> t -> t =
  Map.merge ~f:(fun ~key:_ -> function
    | `Right v | `Left v -> Some v | `Both _ -> failwith "class name collision")

let lookup_cid ~class_id : t -> String.Set.t =
  flip Map.find class_id >> function Some fs -> fs | None -> String.Set.empty

let lookup ~package ~class_name : t -> String.Set.t =
  let class_id = String.(concat ~sep:"." package ^ "." ^ class_name) in
  lookup_cid ~class_id

let add_cid ~class_id ~fields df =
  lookup_cid df ~class_id |> String.Set.union fields |> fun data ->
  String.Map.set df ~key:class_id ~data

let add ~package ~class_name ~fields : t -> t =
  let class_id = String.(concat ~sep:"." package ^ "." ^ class_name) in
  add_cid ~class_id ~fields
