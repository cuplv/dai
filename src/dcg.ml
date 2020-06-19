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

  module Set = struct
    include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

    type t = Set.M(T_comparator).t [@@deriving compare]

    let empty = Set.empty (module T_comparator)

    let singleton = Set.singleton (module T_comparator)
  end

  module Map = struct
    include (Map : module type of Map with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Map.t)

    type 'v t = 'v Map.M(T_comparator).t

    let empty = Map.empty (module T_comparator)
  end
end

module Make (Dom : Abstract.Dom) = struct
  module Ref = struct
    module T = struct
      type t =
        | Stmt of { mutable stmt : Ast.Stmt.t; [@ignore] name : Name.t }
        | AState of { mutable state : Dom.t option; [@ignore] name : Name.t }
      [@@deriving compare, equal, sexp_of]

      let name = function Stmt { stmt = _; name } -> name | AState { state = _; name } -> name

      let hash = name >> Name.hash

      let compare r1 r2 =
        match (r1, r2) with
        | Stmt _, AState _ -> -1
        | AState _, Stmt _ -> 1
        | _ -> Name.compare (name r1) (name r2)

      let pp fs = function
        | Stmt { stmt; name } -> Format.fprintf fs "%a[%a]" Name.pp name Ast.Stmt.pp stmt
        | AState { state = Some s; name } -> Format.fprintf fs "%a[%a]" Name.pp name Dom.pp s
        | AState { state = None; name } -> Format.fprintf fs "%a[???]" Name.pp name

      let to_string r =
        pp Format.str_formatter r;
        Format.flush_str_formatter ()

      let stmt_exn = function
        | Stmt { stmt; name = _ } -> stmt
        | _ -> failwith "Error: stmt_exn called on reference cell with no statement"

      let astate_exn = function
        | AState { state = Some phi; name = _ } -> phi
        | r ->
            failwith
              (Format.asprintf
                 "Error: astate_exn called on reference cell %a with no abstract state" pp r)
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

    module Set = struct
      include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

      type t = Set.M(T_comparator).t [@@deriving compare]

      let empty = Set.empty (module T_comparator)

      let singleton = Set.singleton (module T_comparator)
    end
  end

  module Opaque_ref = struct
    include Regular.Std.Opaque.Make (Ref)

    type t = Ref.t
  end

  module G = Graph.Make (Opaque_ref) (Comp)

  type t = G.t

  type edge = Ref.t * Ref.t * Comp.t

  let ref_by_name nm = G.nodes >> Seq.find ~f:(Ref.name >> Name.equal nm)

  let ref_by_name_exn nm g =
    match ref_by_name nm g with
    | Some r -> r
    | None -> failwith (Format.asprintf "No reference exists with name: %s" (Name.to_string nm))

  let dump_dot ?print ~filename daig =
    let output_fd =
      if Option.is_some print then Unix.stdout
      else Unix.openfile ~mode:[ Unix.O_WRONLY ] "/dev/null"
    in
    let grey = 0xdedede in
    let green = 0xaaffaa in
    let blue = 0xaaaaff in
    Graph.to_dot
      (module G)
      daig ~filename
      ~channel:(Unix.out_channel_of_descr output_fd)
      ~string_of_node:(fun r -> "\"" ^ Ref.to_string r ^ "\"")
      ~string_of_edge:(G.Edge.label >> Comp.to_string)
      ~node_attrs:(function
        | Ref.AState _ as n
          when Seq.exists (G.Node.outputs n daig) ~f:(G.Edge.label >> Comp.equal `Fix) ->
            [ `Color blue; `Shape `Box ]
        | Ref.AState { state = Some _; name = _ } -> [ `Color green; `Shape `Box ]
        | Ref.AState { state = None; name = _ } -> [ `Shape `Box; `Style `Dashed ]
        | Ref.Stmt _ -> [ `Color grey ])

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

  (* TODO: handle nested loops, increasing a given loop head's iteration count and holding others constant *)
  let increment_iteration _loop_head = function
    | Name.Iterate (n, i) -> Name.Iterate (n, i + 1)
    | Name.Prod (Name.Iterate (n, i), Name.Iterate (n_prime, i_prime)) ->
        (* validate that this is a properly constructed-pre-widen name before incrementing its indices *)
        assert (Name.equal n n_prime);
        assert (Int.equal (i + 1) i_prime);
        Name.Prod (Name.Iterate (n, i_prime), Name.Iterate (n, i_prime + 1))
    | n -> n

  (* Transform the DAIG [g] by unrolling one step further the loop whose current abstract iterate is at [curr_iter] *)
  let unroll_loop (g : t) (curr_iter : Ref.t) =
    (* First, find the entire previous unrolling [loop_edges]: all DAIG edges backwards-reachable from [curr_iter] without going through the previous iteration *)
    let loop_head, curr_idx =
      match Ref.name curr_iter with
      | Name.Iterate (l, n) -> (l, n)
      | _ -> failwith "Current iterate [curr_iter] must be an iterate."
    in
    let fixpoint = ref_by_name_exn loop_head g in
    let prev_iter = ref_by_name_exn (Name.Iterate (loop_head, curr_idx - 1)) g in
    let next_iter =
      Ref.AState { state = None; name = increment_iteration loop_head (Ref.name curr_iter) }
    in
    let curr_pre_widen = ref_by_name_exn (Name.Prod (Ref.name prev_iter, Ref.name curr_iter)) g in
    let next_pre_widen =
      Ref.AState { state = None; name = Name.Prod (Ref.name curr_iter, Ref.name next_iter) }
    in

    let rec get_loop frontier nodes edges =
      if Set.is_empty frontier then (nodes, edges)
      else
        let process_node acc n =
          Seq.fold (G.Node.inputs n g) ~init:acc ~f:(fun (f, ns, es) e ->
              let pred = G.Edge.src e in
              if not (Ref.equal pred prev_iter || Set.mem ns pred) then
                (Set.add f pred, Set.add ns pred, Set.add es e)
              else (f, ns, Set.add es e))
        in
        (uncurry3 get_loop) (Set.fold frontier ~init:(Ref.Set.empty, nodes, edges) ~f:process_node)
    in
    let init_frontier = Ref.Set.singleton curr_pre_widen in
    let init_nodes = Ref.Set.empty in
    let init_edges = G.Edge.Set.empty in

    let curr_iter_loop_nodes, curr_iter_loop_edges = get_loop init_frontier init_nodes init_edges in

    (* Then, construct a copy of those nodes and edges, incrementing the loop iteration for AState refs and reusing Stmt refs. *)
    let new_fix_and_widen_edges =
      [
        G.Edge.create curr_iter fixpoint `Fix;
        G.Edge.create next_iter fixpoint `Fix;
        G.Edge.create next_pre_widen next_iter `Widen;
        G.Edge.create curr_iter next_iter `Widen;
      ]
    in

    let all_new_refs =
      Set.fold curr_iter_loop_nodes ~init:[ next_iter; next_pre_widen ] ~f:(fun acc ->
        function
        | Ref.AState { state = _; name } ->
            Ref.AState { state = None; name = increment_iteration loop_head name } :: acc
        | _ -> acc)
    in
    let ref_of_name n = List.find_exn all_new_refs ~f:(Ref.name >> Name.equal n) in

    let g = List.fold all_new_refs ~init:g ~f:(fun acc n -> G.Node.insert n acc) in

    let all_new_edges =
      Set.fold curr_iter_loop_edges ~init:new_fix_and_widen_edges ~f:(fun acc e ->
          let dst = G.Edge.dst e |> Ref.name |> increment_iteration loop_head |> ref_of_name in
          let src =
            let prev_iter_name = Ref.name prev_iter in
            match G.Edge.src e with
            | Ref.Stmt _ as src -> src
            | Ref.AState { state = _; name } ->
                if Name.equal name prev_iter_name then curr_iter
                else ref_of_name @@ increment_iteration loop_head name
          in
          G.Edge.create src dst (G.Edge.label e) :: acc)
    in
    (* Finally, remove the old "fix" edges and add the newly constructed edges. Incident nodes are added automatically.*)
    let g_without_fix_edges = Seq.fold (G.Node.inputs fixpoint g) ~init:g ~f:(flip G.Edge.remove) in
    List.fold all_new_edges ~init:g_without_fix_edges ~f:(flip G.Edge.insert)

  (** IMPURE -- possibly mutates argument [g] by computing and filling empty ref cells
   * Return value is a pair ([Ref.t] guaranteed to be non-empty, [t] reflecting possible changes to the DAIG structure
   *)
  let rec get (r : Ref.t) (g : t) =
    ( match r with
    | Stmt { stmt = _; name = _ } -> g
    | AState { state = Some _; name = _ } -> g
    | AState phi ->
        (* recursively [get] all predecessors *)
        let preds, g =
          Seq.fold (G.Node.preds r g) ~init:([], g) ~f:(fun (acc, g) pred ->
              let p, g = get pred g in
              (p :: acc, g))
        in
        let preds = List.sort preds ~compare:Ref.compare in
        (* [hd_exn] call is safe because empty ref cells always have parents by DCG well-formedness *)
        let result, new_daig =
          match G.Edge.label (Seq.hd_exn (G.Node.inputs r g)) with
          | `Transfer -> (
              match preds with
              | [ s; phi ] -> (Dom.interpret (Ref.stmt_exn s) (Ref.astate_exn phi), g)
              | _ ->
                  failwith
                    "malformed DCG: transfer function must have one Stmt and one AState input" )
          | `Join -> (List.map preds ~f:Ref.astate_exn |> List.reduce_exn ~f:Dom.join, g)
          | `Widen -> (List.map preds ~f:Ref.astate_exn |> List.reduce_exn ~f:Dom.widen, g)
          | `Fix -> (
              match preds with
              | [ p1; p2 ] ->
                  let iter1 = Ref.astate_exn p1 in
                  let iter2 = Ref.astate_exn p2 in
                  (* If fixpoint reached, return it.  Otherwise, unroll the loop and continue. *)
                  if Dom.equal iter1 iter2 then (iter1, g)
                  else unroll_loop g p2 |> get r |> fun (r, g) -> (Ref.astate_exn r, g)
              | _ -> failwith "fix always has two inputs (by construction)" )
        in
        phi.state <- Some result;
        new_daig )
    |> pair r

  (** IMPURE -- see [get] *)
  let get_by_name (nm : Name.t) g =
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

module Daig = Make (Incr.Make (Itv))

let%test "build dcg and dump dot: arith0.js" =
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      "/Users/benno/Documents/CU/code/d1a/test_cases/arith0.js"
  in
  let daig = Daig.of_cfg cfg in
  Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/arith0_daig.dot";
  true

let%test "build dcg and issue queries: while.js" =
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      "/Users/benno/Documents/CU/code/d1a/test_cases/while.js"
  in
  let l1 = Name.Loc (Cfg.Loc.of_int_unsafe 1) in
  let daig = Daig.of_cfg cfg in
  Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/while_daig.dot";
  let _, daig = Daig.get_by_name Name.(Prod (Iterate (l1, 0), Iterate (l1, 1))) daig in
  Daig.dump_dot daig
    ~filename:"/Users/benno/Documents/CU/code/d1a/while_daig_straightline_demand.dot";
  let _, daig = Daig.get_by_name (Name.Loc Cfg.Loc.exit) daig in
  Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/while_daig_loop_demand.dot";
  true
