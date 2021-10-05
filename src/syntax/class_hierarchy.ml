open Dai.Import
module G = Graph.Make (String) (Unit)

type t = G.t
(** keep a class hierarchy as a directed graph, where an edge from "com.example.Foo" to "com.example.Bar" denotes that Foo extends/implements Bar*)

let empty = Graph.create (module G) ()

let add ~(package : string list) ~(class_name : string) ~(super_package : string list)
    ~(superclass_name : string) : t -> t =
  let class_id = String.(concat ~sep:"." package ^ "." ^ class_name) in
  let superclass_id = String.(concat ~sep:"." super_package ^ "." ^ superclass_name) in
  G.Edge.(insert (create class_id superclass_id ()))

let ancestors ~(package : string list) ~(class_name : string) cha : string list =
  let class_id = String.(concat ~sep:"." package ^ "." ^ class_name) in
  Graph.fold_reachable (module G) cha class_id ~init:[] ~f:(flip List.cons)

let compute_closure ~(cha : t) ~(fields : Declared_fields.t) : Declared_fields.t =
  Graph.depth_first_search
    (module G)
    cha
    ~enter_edge:(fun _ e acc ->
      let parent_cid = G.Edge.dst e in
      let child_cid = G.Edge.src e in
      Declared_fields.(
        add_cid acc ~class_id:child_cid ~fields:(lookup_cid acc ~class_id:parent_cid)))
    ~init:fields

let merge = Graph.union (module G)

let%test "closure operation" =
  let cha =
    empty
    |> add ~package:[ "foo"; "bar" ] ~class_name:"One" ~super_package:[ "com"; "example" ]
         ~superclass_name:"Two"
    |> add ~package:[ "com"; "example" ] ~class_name:"Two" ~super_package:[ "foo"; "bar" ]
         ~superclass_name:"Three"
    |> add ~package:[ "com"; "example" ] ~class_name:"Four" ~super_package:[ "foo"; "bar" ]
         ~superclass_name:"Three"
    |> add ~package:[ "com"; "example" ] ~class_name:"Five" ~super_package:[ "foo"; "bar" ]
         ~superclass_name:"Six"
    |> add ~package:[ "com"; "example" ] ~class_name:"Seven" ~super_package:[ "foo"; "bar" ]
         ~superclass_name:"Six"
    |> add ~package:[ "foo"; "bar" ] ~class_name:"Eight" ~super_package:[ "com"; "example" ]
         ~superclass_name:"Seven"
  in
  let fields =
    Declared_fields.(
      empty
      |> add ~package:[ "foo"; "bar" ] ~class_name:"One" ~fields:(String.Set.singleton "A")
      |> add ~package:[ "com"; "example" ] ~class_name:"Two" ~fields:(String.Set.singleton "B")
      |> add ~package:[ "foo"; "bar" ] ~class_name:"Three" ~fields:(String.Set.singleton "C")
      |> add ~package:[ "com"; "example" ] ~class_name:"Four" ~fields:(String.Set.singleton "D")
      |> add ~package:[ "com"; "example" ] ~class_name:"Five" ~fields:(String.Set.singleton "E")
      |> add ~package:[ "foo"; "bar" ] ~class_name:"Six" ~fields:(String.Set.singleton "F")
      |> add ~package:[ "com"; "example" ] ~class_name:"Seven" ~fields:(String.Set.singleton "G")
      |> add ~package:[ "foo"; "bar" ] ~class_name:"Eight" ~fields:(String.Set.singleton "H"))
  in
  let transitive_fields = compute_closure ~cha ~fields in
  Declared_fields.(
    ( lookup transitive_fields ~package:[ "foo"; "bar" ] ~class_name:"One"
    |> String.Set.(equal @@ of_list [ "A"; "B"; "C" ]) )
    && ( lookup transitive_fields ~package:[ "com"; "example" ] ~class_name:"Two"
       |> String.Set.(equal @@ of_list [ "B"; "C" ]) )
    && ( lookup transitive_fields ~package:[ "foo"; "bar" ] ~class_name:"Three"
       |> String.Set.(equal @@ of_list [ "C" ]) )
    && ( lookup transitive_fields ~package:[ "com"; "example" ] ~class_name:"Four"
       |> String.Set.(equal @@ of_list [ "C"; "D" ]) )
    && ( lookup transitive_fields ~package:[ "com"; "example" ] ~class_name:"Five"
       |> String.Set.(equal @@ of_list [ "E"; "F" ]) )
    && ( lookup transitive_fields ~package:[ "foo"; "bar" ] ~class_name:"Six"
       |> String.Set.(equal @@ of_list [ "F" ]) )
    && ( lookup transitive_fields ~package:[ "com"; "example" ] ~class_name:"Seven"
       |> String.Set.(equal @@ of_list [ "F"; "G" ]) )
    && lookup transitive_fields ~package:[ "foo"; "bar" ] ~class_name:"Eight"
       |> String.Set.(equal @@ of_list [ "F"; "G"; "H" ]))
