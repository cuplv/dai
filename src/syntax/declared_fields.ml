open Dai.Import

type fields = { static : String.Set.t; instance : String.Set.t }

type t = fields String.Map.t
(** for each application class, keep a set of instance field names on that class *)

let pp_fields fs flds =
  let pp_set = Set.pp String.pp in
  Format.fprintf fs "static: %a; instance: %a" pp_set flds.static pp_set flds.instance

let pp : t pp = Map.pp String.pp pp_fields

let empty = String.Map.empty

let merge : t -> t -> t =
  Map.merge ~f:(fun ~key:_ -> function
    | `Right v | `Left v -> Some v | `Both _ -> failwith "class name collision")

let lookup_cid ~class_id =
  flip Map.find class_id >> function
  | Some fs -> fs
  | None -> { static = String.Set.empty; instance = String.Set.empty }

let lookup ~package ~class_name =
  let class_id = String.(concat ~sep:"." package ^ "." ^ class_name) in
  lookup_cid ~class_id

let lookup_static ~package ~class_name fs =
  lookup ~package ~class_name fs |> fun { static; _ } -> static

let lookup_instance ~package ~class_name fs =
  lookup ~package ~class_name fs |> fun { instance; _ } -> instance

let pointwise_union fs1 fs2 =
  let static = String.Set.union fs1.static fs2.static in
  let instance = String.Set.union fs1.instance fs2.instance in
  { static; instance }

let add_cid ~class_id ~fields df =
  lookup_cid df ~class_id |> pointwise_union fields |> fun data ->
  String.Map.set df ~key:class_id ~data

let add ~package ~class_name ~fields : t -> t =
  let class_id = String.(concat ~sep:"." package ^ "." ^ class_name) in
  add_cid ~class_id ~fields
