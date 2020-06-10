(* stashing because Import shadows Hashtbl *)
let seeded_hash = Hashtbl.seeded_hash

open Import

module Comp = struct
  type t = [ `Transfer | `Join | `Widen | `Fix ]
  [@@deriving compare, equal, hash]

  let pp fs = function
    | `Transfer -> Format.fprintf fs "transfer"
    | `Join -> Format.fprintf fs "join"
    | `Widen -> Format.fprintf fs "widen"
    | `Fix -> Format.fprintf fs "fix"
end

module Name = struct
  type t =
    | Loc of Cfg.Loc.t
    | Fn of Comp.t
    | Idx of int
    | Prod of t * t
    (* todo: do something fancier for nested loops *)
    | Iterate of t * int
  [@@deriving compare, equal]

  let rec hash : t -> int = function
    | Loc l -> Cfg.Loc.hash l
    | Fn fn -> Comp.hash fn
    | Idx i ->
        Hashtbl.hash (Hashtbl.hash i)
        (* doubly hashing to avoid collisions with Loc's, since both are isomorphic to int *)
    | Prod (n1, n2) -> seeded_hash (hash n1) n2
    | Iterate (n, idx) -> seeded_hash idx n

  let rec pp fs = function
    | Loc l -> Cfg.Loc.pp fs l
    | Fn c -> Comp.pp fs c
    | Idx i -> Format.fprintf fs "%i" i
    | Prod (n1, n2) -> Format.fprintf fs "(%a . %a)" pp n1 pp n2
    | Iterate (n, idx) -> Format.fprintf fs "%a^%i" pp n idx
end

module Make (Dom : Domain_intf.Dom) = struct
  module Ref = struct
    type t =
      | Stmt of { mutable stmt : Ast.Stmt.t; name : Name.t }
      | AState of { mutable state : Dom.t option; name : Name.t }
    [@@deriving compare, equal]

    let name = function
      | Stmt { stmt = _; name } -> name
      | AState { state = _; name } -> name

    let hash = name >> Name.hash

    let compare r1 r2 =
      match (r1, r2) with
      | Stmt _, AState _ -> -1
      | AState _, Stmt _ -> 1
      | _ -> Name.compare (name r1) (name r2)

    let stmt_exn = function
      | Stmt { stmt; name = _ } -> stmt
      | _ ->
          failwith "Error: stmt_exn called on reference cell with no statement"

    let astate_exn = function
      | AState { state = Some phi; name = _ } -> phi
      | _ ->
          failwith
            "Error: astate_exn called on reference cell with no abstract state"
  end

  module Opaque_ref = struct
    include Regular.Std.Opaque.Make (Ref)

    type t = Ref.t
  end

  let of_cfg (_cfg : Cfg.t) = failwith "todo"

  module G = Graph.Make (Opaque_ref) (Comp)

  type t = G.t

  (** IMPURE -- possibly mutates argument [g] by computing and filling empty ref cells
   * Return value is a [Ref.t] guaranteed to be non-empty
   *)
  let rec get (r : Ref.t) (g : t) =
    ( match r with
    | Stmt { stmt = _; name = _ } -> ()
    | AState { state = Some _; name = _ } -> ()
    | AState phi ->
        (* recursively [get] all predecessors *)
        let preds =
          G.Node.preds r g
          |> Seq.map ~f:(flip get g)
          |> Seq.to_list
          |> List.sort ~compare:Ref.compare
        in
        (* [hd_exn] call is safe because empty ref cells always have parents by DCG well-formedness *)
        let result =
          match G.Edge.label (Seq.hd_exn (G.Node.inputs r g)) with
          | `Transfer -> (
              match preds with
              | [ s; phi ] ->
                  Dom.interpret (Ref.stmt_exn s) (Ref.astate_exn phi)
              | _ ->
                  failwith
                    "malformed DCG: transfer function must have one Stmt and \
                     one AState input" )
          | `Join ->
              List.map preds ~f:Ref.astate_exn |> List.reduce_exn ~f:Dom.join
          | `Widen ->
              List.map preds ~f:Ref.astate_exn |> List.reduce_exn ~f:Dom.widen
          | `Fix -> failwith "todo"
        in
        phi.state <- Some result );
    r

  (** IMPURE -- see [get] *)
  let get_by_name (nm : Name.t) g =
    let ref_by_name nm = G.nodes >> Seq.find ~f:(Ref.name >> Name.equal nm) in
    match ref_by_name nm g with
    | Some r -> get r g
    | None ->
        failwith
          ( Format.fprintf Format.str_formatter
              "No reference cell found with name %a" Name.pp nm;
            Format.flush_str_formatter () )

  (** IMPURE -- modifies the specified cell and clears the value of forwards-reachable cells*)
  let edit_stmt (_nm : Name.t) (_stmt : Ast.Stmt.t) (_g : t) = failwith "todo"

  (** IMPURE --*)
  let delete_stmt (nm : Name.t) (g : t) = edit_stmt nm Ast.Stmt.Skip g

  let add_stmt (_loc : Cfg.Loc.t) (_stmt : Ast.Stmt.t) (_g : t) =
    failwith "todo"
end
