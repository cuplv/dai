open Import

module Comp = struct
  type t = [ `Transfer | `Join | `Widen | `Fix ] [@@deriving compare, equal, hash]

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
    (* todo: do something fancier (set of loop/iteration pairs?) for nested loops -- for now, assuming loop bodies are disjoint, so just one iteration count needed *)
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

module Make (Dom : Abstract.Dom) = struct
  module Ref = struct
    type t =
      | Stmt of { mutable stmt : Ast.Stmt.t; name : Name.t }
      | AState of { mutable state : Dom.t option; name : Name.t }
    [@@deriving compare, equal]

    let name = function Stmt { stmt = _; name } -> name | AState { state = _; name } -> name

    let hash = name >> Name.hash

    let compare r1 r2 =
      match (r1, r2) with
      | Stmt _, AState _ -> -1
      | AState _, Stmt _ -> 1
      | _ -> Name.compare (name r1) (name r2)

    let stmt_exn = function
      | Stmt { stmt; name = _ } -> stmt
      | _ -> failwith "Error: stmt_exn called on reference cell with no statement"

    let astate_exn = function
      | AState { state = Some phi; name = _ } -> phi
      | _ -> failwith "Error: astate_exn called on reference cell with no abstract state"
  end

  module Opaque_ref = struct
    include Regular.Std.Opaque.Make (Ref)

    type t = Ref.t
  end

  module G = Graph.Make (Opaque_ref) (Comp)

  type t = G.t

  type edge = Ref.t * Ref.t * Comp.t

  (** Directly implements the DCG Encoding procedure of Section 4.2; OCaml variables are labelled by LaTeX equivalents where applicable *)
  let of_cfg (cfg : Cfg.t) : t =
    let open List.Monad_infix in
    let dom_tree = Graph.dominators (module Cfg.G) cfg Cfg.Loc.entry in
    let name_of_edge e = Name.(Prod (Loc (Cfg.src e), Loc (Cfg.dst e))) in
    let dst_name e = Name.Loc (Cfg.dst e) in

    (* L_\not\sqcup, L_\sqcup *)
    let nonjoin_locs, join_locs =
      Graph.depth_first_search
        (module Cfg.G)
        ~init:([], [])
        ~leave_node:(fun _order n (nj_acc, j_acc) ->
          if Cfg.G.Node.degree ~dir:`In n cfg <= 1 then (n :: nj_acc, j_acc)
          else (nj_acc, n :: j_acc))
        cfg
    in

    (* E_f, E_b *)
    let _forward_edges, back_edges =
      Graph.depth_first_search
        (module Cfg.G)
        ~init:([], [])
        ~leave_edge:(fun kind e (f_acc, b_acc) ->
          match kind with `Back -> (f_acc, e :: b_acc) | _ -> (e :: f_acc, b_acc))
        cfg
    in

    let loop_heads = back_edges >>| Cfg.dst in
    let loop_head_dominators =
      Graphlib.Std.Tree.ancestors dom_tree
      >> Seq.filter ~f:(fun d -> List.mem loop_heads d ~equal:Cfg.Loc.equal)
      >> Seq.to_list
    in
    let name_of_loc l =
      match loop_head_dominators l with
      | [] -> Name.Loc l
      | [ _loop_head ] -> Name.(Iterate (Loc l, 0))
      | _ -> failwith "Nested loops not yet supported."
    in
    let forward_edges_to l =
      Seq.to_list (Cfg.G.Node.inputs l cfg)
      |> List.filter ~f:(fun e -> not @@ List.mem back_edges e ~equal:Cfg.G.Edge.equal)
      |> List.mapi ~f:pair
    in

    (* R_\mathit{Stmt} *)
    let stmt_refs : Ref.t list =
      let backedge =
        back_edges >>| fun e -> Ref.Stmt { stmt = Cfg.G.Edge.label e; name = name_of_edge e }
      and straightline =
        nonjoin_locs >>= (flip Cfg.G.Node.inputs cfg >> Seq.to_list) >>| fun e ->
        Ref.Stmt { stmt = Cfg.G.Edge.label e; name = name_of_edge e }
      and disambiguated =
        join_locs >>= fun n ->
        forward_edges_to n >>| fun (i, e) ->
        Ref.Stmt { stmt = Cfg.G.Edge.label e; name = Name.(Prod (Idx i, name_of_edge e)) }
      in
      List.(append backedge (append straightline disambiguated))
    in

    (* R_{\Sigma^\sharp} *)
    let astate_refs : Ref.t list =
      let at_locs =
        Seq.to_list (Cfg.G.nodes cfg) >>| fun l ->
        Ref.AState
          {
            state = (if Cfg.Loc.(equal entry l) then Some (Dom.init ()) else None);
            name = name_of_loc l;
          }
      and pre_joins =
        join_locs >>= fun l ->
        forward_edges_to l >>| fun (i, _) ->
        Ref.AState { state = None; name = Name.(Prod (Idx i, name_of_loc l)) }
      and pre_widens =
        back_edges >>| fun e ->
        Ref.AState
          { state = None; name = Name.(Prod (Iterate (dst_name e, 0), Iterate (dst_name e, 1))) }
      in
      List.(append at_locs (append pre_joins pre_widens))
    in
    (* R_\circlearrowleft *)
    let cycle_refs : Ref.t list =
      back_edges >>= fun e ->
      [
        Ref.AState { state = None; name = Name.Iterate (dst_name e, 0) };
        Ref.AState { state = None; name = Name.Iterate (dst_name e, 1) };
      ]
    in
    (* C_{\denote\cdot^\sharp}*)
    let transfer_comps : edge list = failwith "todo" in
    (* C_\sqcup *)
    let join_comps : edge list = failwith "todo" in
    (* C_\textsf{fix} *)
    let fix_comps : edge list = failwith "todo" in
    (* C_\nabla *)
    let widen_comps : edge list = failwith "todo" in
    Graph.create
      (module G)
      ~nodes:List.(append stmt_refs (append astate_refs cycle_refs))
      ~edges:List.(append transfer_comps (append join_comps (append fix_comps widen_comps)))
      ()

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
          |> Seq.to_list |> List.sort ~compare:Ref.compare
        in
        (* [hd_exn] call is safe because empty ref cells always have parents by DCG well-formedness *)
        let result =
          match G.Edge.label (Seq.hd_exn (G.Node.inputs r g)) with
          | `Transfer -> (
              match preds with
              | [ s; phi ] -> Dom.interpret (Ref.stmt_exn s) (Ref.astate_exn phi)
              | _ ->
                  failwith
                    "malformed DCG: transfer function must have one Stmt and one AState input" )
          | `Join -> List.map preds ~f:Ref.astate_exn |> List.reduce_exn ~f:Dom.join
          | `Widen -> List.map preds ~f:Ref.astate_exn |> List.reduce_exn ~f:Dom.widen
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
          ( Format.fprintf Format.str_formatter "No reference cell found with name %a" Name.pp nm;
            Format.flush_str_formatter () )

  (** IMPURE -- modifies the specified cell and clears the value of forwards-reachable cells*)
  let edit_stmt (_nm : Name.t) (_stmt : Ast.Stmt.t) (_g : t) = failwith "todo"

  (** IMPURE -- see [edit_stmt] *)
  let delete_stmt (nm : Name.t) (g : t) = edit_stmt nm Ast.Stmt.Skip g

  let add_stmt (_loc : Cfg.Loc.t) (_stmt : Ast.Stmt.t) (_g : t) = failwith "todo"
end
