open Import

module Comp = struct
  type t = [ `Transfer | `Join | `Widen | `Fix ] [@@deriving compare, equal, hash, sexp_of]

  let pp fs = function
    | `Transfer -> Format.fprintf fs "transfer"
    | `Join -> Format.fprintf fs "join"
    | `Widen -> Format.fprintf fs "widen"
    | `Fix -> Format.fprintf fs "fix"

  let to_string c =
    pp Format.str_formatter c;
    Format.flush_str_formatter ()
end

module Name = struct
  module T = struct
    type t =
      | Loc of Cfg.Loc.t
      | Fn of Comp.t
      | Idx of int
      | Prod of t * t
      (* todo: do something fancier (set of loop/iteration pairs?) for nested loops -- for now, assuming loop bodies are disjoint, so just one iteration count needed *)
      | Iterate of t * int
    [@@deriving compare, equal, sexp_of]

    let rec hash : t -> int = function
      | Loc l -> Cfg.Loc.hash l
      | Fn fn -> Comp.hash fn
      | Idx i -> Hashtbl.hash (Hashtbl.hash i)
      (* doubly hashing to avoid collisions with Loc's, since both are isomorphic to int *)
      | Prod (n1, n2) -> seeded_hash (hash n1) n2
      | Iterate (n, idx) -> seeded_hash idx n

    let rec pp fs = function
      | Loc l -> Cfg.Loc.pp fs l
      | Fn c -> Comp.pp fs c
      | Idx i -> Format.fprintf fs "%i" i
      | Prod (n1, n2) -> Format.fprintf fs "(%a . %a)" pp n1 pp n2
      | Iterate (n, idx) -> Format.fprintf fs "%a^%i" pp n idx

    let to_string n =
      pp Format.str_formatter n;
      Format.flush_str_formatter ()
  end

  include T

  module T_comparator : sig
    type t = T.t

    type comparator_witness

    val comparator : (t, comparator_witness) Comparator.t
  end = struct
    include T
    include Comparator.Make (T)
  end

  module Map = struct
    include (Map : module type of Map with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Map.t)

    type 'v t = 'v Map.M(T_comparator).t

    let empty = Map.empty (module T_comparator)
  end
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

    let pp fs = function
      | Stmt { stmt; name } -> Format.fprintf fs "%a[%a]" Name.pp name Ast.Stmt.pp stmt
      | AState { state = Some s; name } -> Format.fprintf fs "%a[%a]" Name.pp name Dom.pp s
      | AState { state = None; name } -> Format.fprintf fs "%a[???]" Name.pp name

    let to_string r =
      pp Format.str_formatter r;
      Format.flush_str_formatter ()
  end

  module Opaque_ref = struct
    include Regular.Std.Opaque.Make (Ref)

    type t = Ref.t
  end

  module G = Graph.Make (Opaque_ref) (Comp)

  type t = G.t

  type edge = Ref.t * Ref.t * Comp.t

  let dump_dot ?print ~filename daig =
    let output_fd =
      if Option.is_some print then Unix.stdout
      else Unix.openfile ~mode:[ Unix.O_WRONLY ] "/dev/null"
    in
    Graph.to_dot
      (module G)
      daig ~filename
      ~channel:(Unix.out_channel_of_descr output_fd)
      ~string_of_node:(fun r -> "\"" ^ Ref.to_string r ^ "\"")
      ~string_of_edge:(G.Edge.label >> Comp.to_string)

  (** Directly implements the DCG Encoding procedure of Section 4.2; OCaml variables are labelled by LaTeX equivalents where applicable *)
  let of_cfg (cfg : Cfg.t) : t =
    let open List.Monad_infix in
    let name_of_edge e = Name.(Prod (Loc (Cfg.src e), Loc (Cfg.dst e))) in

    (* E_b *)
    let back_edges = Cfg.back_edges cfg in
    let loop_heads = Cfg.loop_heads cfg in
    let loop_head_map = Cfg.containing_loop_heads cfg in
    (* L_\not\sqcup, L_\sqcup *)
    let nonjoin_locs, join_locs = Cfg.locs_by_forward_indegree cfg in

    (* Optional argument is used at loop heads, where there's a separate location for the 0th iterate and the fixpoint.
       When [dst] flag is set, yields the 0th iterate name, otherwise yield the fixpoint name.  This is such that flow
       into the loop head goes to the 0th iterate and flow out is from the fixpoint.
     *)
    let name_of_loc ?(dst = false) l =
      ( match Map.find loop_head_map l with
      | None -> Name.Loc l
      | Some heads ->
          if Int.equal (Set.length heads) 1 then Name.(Iterate (Loc l, 0))
          else failwith "Nested loops not yet supported." )
      |> fun n ->
      if dst && List.mem loop_heads l ~equal:Cfg.Loc.equal then Name.Iterate (n, 0) else n
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
        nonjoin_locs >>= forward_edges_to >>| fun (_i, e) ->
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
        let l_bar = Name.Loc (Cfg.dst e) in
        Ref.AState { state = None; name = Name.(Prod (Iterate (l_bar, 0), Iterate (l_bar, 1))) }
      in
      List.(append at_locs (append pre_joins pre_widens))
    in
    (* R_\circlearrowleft *)
    let cycle_refs : Ref.t list =
      back_edges >>= fun e ->
      let l_bar = Name.Loc (Cfg.dst e) in
      [
        Ref.AState { state = None; name = Name.Iterate (l_bar, 0) };
        Ref.AState { state = None; name = Name.Iterate (l_bar, 1) };
      ]
    in
    let all_refs = List.(append stmt_refs (append astate_refs cycle_refs)) in
    let ref_map : Ref.t Name.Map.t =
      List.fold all_refs ~init:Name.Map.empty ~f:(fun m r ->
          match Map.add m ~key:(Ref.name r) ~data:r with
          | `Ok m -> m
          | `Duplicate ->
              let nm = Name.to_string (Ref.name r) in
              failwith
                (Format.sprintf "Non-uniquely-named reference %s violates well-formedness" nm))
    in
    let lookup_name nm =
      match Name.Map.find ref_map nm with
      | Some r -> r
      | None ->
          let nm = Name.to_string nm in
          failwith (Format.sprintf "Name %s is not a vertex of the DCG under construction" nm)
    in

    (* C_{\denote\cdot^\sharp}, except for the last component, which is in [loop_comps] *)
    let transfer_comps : edge list =
      let straightline_transfers =
        nonjoin_locs >>= fun l ->
        let dst_astate = lookup_name (name_of_loc l ~dst:true) in
        List.fold (forward_edges_to l) ~init:[] ~f:(fun dcg_edges (_idx, cfg_edge) ->
            let stmt = lookup_name (name_of_edge cfg_edge) in
            let is_into_loop_body =
              match Map.find loop_head_map (Cfg.dst cfg_edge) with
              | None -> false
              | Some heads -> Set.mem heads (Cfg.src cfg_edge)
            in
            let src_astate =
              lookup_name
              @@
              if is_into_loop_body then Name.Iterate (name_of_loc (Cfg.src cfg_edge), 0)
              else name_of_loc (Cfg.src cfg_edge)
            in
            (stmt, dst_astate, `Transfer) :: (src_astate, dst_astate, `Transfer) :: dcg_edges)
      in
      let prejoin_transfers =
        join_locs >>= fun l ->
        List.fold (forward_edges_to l) ~init:[] ~f:(fun acc (idx, edge) ->
            let stmt = lookup_name @@ Name.Prod (Idx idx, name_of_edge edge) in
            let dst = lookup_name @@ Name.Prod (Idx idx, name_of_loc l) in
            let src_astate = lookup_name @@ name_of_loc (Cfg.src edge) in
            (stmt, dst, `Transfer) :: (src_astate, dst, `Transfer) :: acc)
      in
      List.append straightline_transfers prejoin_transfers
    in

    (* C_\sqcup *)
    let join_comps : edge list =
      join_locs >>= fun l ->
      let l_bar = name_of_loc l in
      let n =
        lookup_name
        @@ if List.mem loop_heads l ~equal:Cfg.Loc.equal then Name.Iterate (l_bar, 0) else l_bar
      in
      let ith_input i = lookup_name (Name.Prod (Name.Idx i, l_bar)) in
      forward_edges_to l >>| fun (i, _) -> (ith_input i, n, `Join)
    in

    (* C_\textsf{fix}  AND C_\nabla AND last component of C_{\denote\cdot^\sharp} *)
    let loop_comps : edge list =
      back_edges >>= fun e ->
      let l = name_of_loc (Cfg.dst e) in
      let l0 = Name.Iterate (l, 0) in
      let l1 = Name.Iterate (l, 1) in
      let iter0 = lookup_name l0 in
      let iter1 = lookup_name l1 in
      let pre_widen = lookup_name (Name.Prod (l0, l1)) in
      let fixpoint = lookup_name l in
      [
        (iter0, fixpoint, `Fix);
        (iter1, fixpoint, `Fix);
        (iter0, iter1, `Widen);
        (pre_widen, iter1, `Widen);
        (lookup_name @@ name_of_edge e, pre_widen, `Transfer);
        (lookup_name @@ name_of_loc (Cfg.src e), pre_widen, `Transfer);
      ]
    in
    Graph.create
      (module G)
      ~nodes:all_refs
      ~edges:List.(append transfer_comps (append join_comps loop_comps))
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

module Daig = Make (Set_of_concrete.Env)

let%test "build dcg and dump dot: arith0.js" =
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      "/Users/benno/Documents/CU/code/d1a/test_cases/arith0.js"
  in
  let daig = Daig.of_cfg cfg in
  Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/arith0_daig.dot";
  true

let%test "build dcg and issue a query: while.js" =
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      "/Users/benno/Documents/CU/code/d1a/test_cases/while.js"
  in
  let daig = Daig.of_cfg cfg in
  Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/while_daig.dot";
  let l1 = Name.Loc (Cfg.Loc.of_int_unsafe 1) in
  let _straightline_query =
    let _res = Daig.get_by_name Name.(Prod (Iterate (l1, 0), Iterate (l1, 1))) daig in
    Daig.dump_dot daig
      ~filename:"/Users/benno/Documents/CU/code/d1a/while_daig_straightline_demand.dot"
  in
  let _loop_query =
    let res = Daig.get_by_name l1 daig in
    Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/while_daig_loop_demand.dot";
    Format.fprintf Format.std_formatter "Loop head fixpoint: %a" Daig.Ref.pp res
  in
  true
