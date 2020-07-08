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
      (* todo: do something fancier (set of loop/iteration pairs?) for nested loops -- for now, assuming loop bodies are disjoint, so just one iteration count needed *)
      | Iterate of int * t
      | Prod of t * t
    [@@deriving compare, equal, sexp_of]

    let rec hash : t -> int = function
      | Loc l -> Cfg.Loc.hash l
      | Fn fn -> Comp.hash fn
      | Idx i -> Hashtbl.hash (Hashtbl.hash i)
      (* doubly hashing to avoid collisions with Loc's, since both are isomorphic to int *)
      | Prod (n1, n2) -> seeded_hash (hash n1) n2
      | Iterate (idx, n) -> seeded_hash idx n

    let rec pp fs = function
      | Loc l -> Cfg.Loc.pp fs l
      | Fn c -> Comp.pp fs c
      | Idx i -> Format.fprintf fs "%i" i
      | Prod (n1, n2) -> Format.fprintf fs "(%a . %a)" pp n1 pp n2
      | Iterate (idx, n) -> Format.fprintf fs "%a^%i" pp n idx

    let to_string n =
      pp Format.str_formatter n;
      Format.flush_str_formatter ()

    let is_iterate = function Iterate _ -> true | _ -> false
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
        | Stmt of { mutable stmt : Dom.Stmt.t; name : Name.t }
        | AState of { mutable state : Dom.t option; name : Name.t }
      [@@deriving sexp_of]

      let name = function Stmt { stmt = _; name } -> name | AState { state = _; name } -> name

      let hash = name >> Name.hash

      let equal r1 r2 = Name.equal (name r1) (name r2)

      let compare r1 r2 =
        match (r1, r2) with
        | Stmt _, AState _ -> -1
        | AState _, Stmt _ -> 1
        | _ -> Name.compare (name r1) (name r2)

      let pp fs = function
        | Stmt { stmt; name } -> Format.fprintf fs "%a[%a]" Name.pp name Dom.Stmt.pp stmt
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

      let is_astate = function AState _ -> true | _ -> false

      let is_stmt = is_astate >> not

      let is_empty = function AState { state = None; name = _ } -> true | _ -> false
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
  module Cfg = Cfg.Make (Dom.Stmt)

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

  (** Directly implements the DAIG Encoding procedure; OCaml variables are labelled by LaTeX equivalents where applicable *)
  let of_cfg ?(init_state : Dom.t = Dom.init ()) (cfg : Cfg.G.t) : t =
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
          if Int.equal (Set.length heads) 1 then Name.(Iterate (0, Loc l))
          else failwith "Nested loops not yet supported." )
      |> fun n ->
      if dst && List.mem loop_heads l ~equal:Cfg.Loc.equal then Name.Iterate (0, n) else n
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
            state = (if Cfg.Loc.(equal entry l) then Some init_state else None);
            name = name_of_loc l;
          }
      and pre_joins =
        join_locs >>= fun l ->
        forward_edges_to l >>| fun (i, _) ->
        Ref.AState { state = None; name = Name.(Prod (Idx i, name_of_loc l)) }
      and pre_widens =
        back_edges >>| fun e ->
        let l_bar = Name.Loc (Cfg.dst e) in
        Ref.AState { state = None; name = Name.(Prod (Iterate (0, l_bar), Iterate (1, l_bar))) }
      in
      List.(append at_locs (append pre_joins pre_widens))
    in
    (* R_\circlearrowleft *)
    let cycle_refs : Ref.t list =
      back_edges >>= fun e ->
      let l_bar = Name.Loc (Cfg.dst e) in
      [
        Ref.AState { state = None; name = Name.Iterate (0, l_bar) };
        Ref.AState { state = None; name = Name.Iterate (1, l_bar) };
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
              if is_into_loop_body then Name.Iterate (0, name_of_loc (Cfg.src cfg_edge))
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
        @@ if List.mem loop_heads l ~equal:Cfg.Loc.equal then Name.Iterate (0, l_bar) else l_bar
      in
      let ith_input i = lookup_name (Name.Prod (Name.Idx i, l_bar)) in
      forward_edges_to l >>| fun (i, _) -> (ith_input i, n, `Join)
    in

    (* C_\textsf{fix}  AND C_\nabla AND last component of C_{\denote\cdot^\sharp} *)
    let loop_comps : edge list =
      back_edges >>= fun e ->
      let l = name_of_loc (Cfg.dst e) in
      let l0 = Name.Iterate (0, l) in
      let l1 = Name.Iterate (1, l) in
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

  let of_js_cfg_unsafe ?(init_state : Dom.t = Dom.init ()) (cfg : Cfg_parser.JsCfg.G.t) : t =
    let cast_cfg : Cfg.G.t = Obj.magic cfg in
    of_cfg ~init_state cast_cfg

  (* TODO: handle nested loops, increasing a given loop head's iteration count and holding others constant *)
  let increment_iteration _loop_head = function
    | Name.Iterate (i, n) -> Name.Iterate (i + 1, n)
    | Name.Prod (Name.Iterate (i, n), Name.Iterate (i_prime, n_prime)) ->
        (* validate that this is a properly constructed-pre-widen name before incrementing its indices *)
        assert (Name.equal n n_prime);
        assert (Int.equal (i + 1) i_prime);
        Name.Prod (Name.Iterate (i_prime, n), Name.Iterate (i_prime + 1, n))
    | n -> n

  (* Transform the DAIG [g] by unrolling one step further the loop whose current abstract iterate is at [curr_iter] *)
  let unroll_loop (g : t) (curr_iter : Ref.t) =
    (* First, find the entire previous unrolling [loop_edges]: all DAIG edges backwards-reachable from [curr_iter] without going through the previous iteration *)
    let curr_idx, loop_head =
      match Ref.name curr_iter with
      | Name.Iterate (n, l) -> (n, l)
      | _ -> failwith "Current iterate [curr_iter] must be an iterate."
    in
    let fixpoint = ref_by_name_exn loop_head g in
    let prev_iter = ref_by_name_exn (Name.Iterate (curr_idx - 1, loop_head)) g in
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

  (* traverse control-flow-reachable ref cells, clearing their values and accumulating
     all encountered loop fixpoints for the next step. NB: DAIG-reachability alone is
     unsound due to cyclic control flow, so also jump back to loop heads as needed *)
  let dirty_from r g =
    let dirty = function
      | Ref.AState a -> a.state <- None
      | Ref.Stmt _ -> failwith "dirty should only be called on AStates in a well-formed DAIG"
    in
    let change_prop_step acc n =
      Seq.fold (G.Node.outputs n g) ~init:acc ~f:(fun (f, lh_fps) e ->
          let succ = G.Edge.dst e in
          if Ref.is_empty succ then (f, lh_fps)
          else if Comp.equal `Fix (G.Edge.label e) && (not @@ List.mem lh_fps succ ~equal:Ref.equal)
          then
            match Ref.name succ with
            | Name.Loc l -> (
                match ref_by_name_exn Name.(Iterate (0, Loc l)) g with
                | Ref.AState { state = Some _; name = _ } as lh ->
                    dirty succ;
                    (lh :: succ :: f, succ :: lh_fps)
                | _ ->
                    dirty succ;
                    (succ :: f, succ :: lh_fps) )
            | _ ->
                failwith
                  "malformed DAIG -- the destination of a `Fix edge is always the loop fixpoint"
          else (
            dirty succ;
            (succ :: f, lh_fps) ))
    in
    let rec change_prop frontier loop_head_fixpoints =
      if List.is_empty frontier then loop_head_fixpoints
      else
        (uncurry change_prop)
          (List.fold frontier ~init:([], loop_head_fixpoints) ~f:change_prop_step)
    in
    (* For each encountered (and therefore dirtied) loop fixpoint, reset its fix edges to the 0th and 1st iterates,
       then remove any dangling unrollings *)
    List.fold
      (change_prop [ r ] [])
      ~init:g
      ~f:(fun g -> function
        | Ref.AState { state = _; name = Name.Loc _ as l } as loop_fp -> (
            let iter0 = ref_by_name_exn (Name.Iterate (0, l)) g in
            let iter1 = ref_by_name_exn (Name.Iterate (1, l)) g in
            let daig =
              Seq.fold (G.Node.inputs loop_fp g) ~init:g ~f:(flip G.Edge.remove)
              |> G.Edge.insert (G.Edge.create iter0 loop_fp `Fix)
              |> G.Edge.insert (G.Edge.create iter1 loop_fp `Fix)
            in
            match
              Seq.find (G.Node.succs iter1 daig) ~f:(fun r ->
                  match Ref.name r with Name.Iterate (1, _) -> true | _ -> false)
            with
            | Some root_of_dangling_unrollings ->
                Graph.fold_reachable
                  (module G)
                  daig root_of_dangling_unrollings ~init:daig
                  ~f:(fun daig n ->
                    Seq.fold ~init:(G.Node.remove n daig) ~f:(flip G.Edge.remove)
                      (G.Node.inputs n daig))
            | _ -> daig )
        | _ ->
            failwith "malformed DAIG -- loop fixpoints are always named by their syntactic location")

  let fixpoint_of astate_ref daig =
    match Ref.name astate_ref with
    | Name.Iterate (_, l) -> (
        (* loop head fixpoint postdominates all refcells in the loop, so just follow any path to find it *)
        let rec loop_head_fp refcell =
          match Ref.name refcell with
          | Name.Loc _ -> refcell
          | _ -> G.Node.succs refcell daig |> Seq.hd_exn |> loop_head_fp
        in
        let loop_head_fp, daig = get (loop_head_fp astate_ref) daig in
        let final_iterate =
          G.Node.preds loop_head_fp daig
          |> Seq.filter ~f:(Ref.name >> Name.is_iterate)
          |> Seq.max_elt ~compare:(fun x y -> Name.compare (Ref.name x) (Ref.name y))
        in
        match Option.map ~f:Ref.name final_iterate with
        | Some (Name.Iterate (k, _)) ->
            let k = Int.max 0 (pred k) in
            get_by_name (Name.Iterate (k, l)) daig
        | _ ->
            failwith
              "error, malformed DAIG -- predecessors of the fixpoint are named in the above form" )
    | _ -> get astate_ref daig

  let get_by_loc l daig =
    match ref_by_name (Name.Loc l) daig with
    | Some r -> get r daig
    | None -> (
        match ref_by_name Name.(Iterate (0, Loc l)) daig with
        | Some r -> fixpoint_of r daig
        | None ->
            dump_dot ~filename:"/Users/benno/Documents/CU/code/d1a/error_dump.dot" daig;
            failwith
              (Format.asprintf "location %a not found; daig dumped at error_dump.dot" Cfg.Loc.pp l)
        )

  (** IMPURE -- modifies the specified cell and clears the value of forwards-reachable cells*)
  let edit_stmt (r : Ref.t) (stmt : Dom.Stmt.t) (g : t) =
    match r with
    | Ref.Stmt cell ->
        cell.stmt <- stmt;
        dirty_from r g
    | Ref.AState _ ->
        failwith "Ill-typed edit -- can't store a statement in an abstract state reference"

  (** IMPURE -- see [edit_stmt] *)
  let delete_stmt (nm : Name.t) (g : t) =
    match ref_by_name nm g with
    | Some r -> edit_stmt r Dom.Stmt.skip g
    | None -> failwith "can't delete non-existent reference"

  (** add a statement*)
  let add_stmt src_loc dst_loc stmt daig =
    assert (not @@ Cfg.Loc.equal src_loc dst_loc);
    let dst_ref =
      match ref_by_name Name.(Iterate (0, Loc dst_loc)) daig with
      | Some r -> r
      | None -> ref_by_name_exn (Name.Loc dst_loc) daig
    in
    let src_ref =
      match Ref.name dst_ref with
      | Name.Loc _ -> ref_by_name_exn (Name.Loc src_loc) daig
      | _ -> (
          match ref_by_name Name.(Iterate (0, Loc src_loc)) daig with
          | Some r -> r
          | None -> ref_by_name_exn (Name.Loc src_loc) daig )
    in
    (* if dst is the target of...
     * ... no computation then just add the edge directly, no conflicts.
     * ... a join then tack this statement onto that join
     * ... a transfer function then add a join
     *)
    let new_daig =
      match Seq.to_list @@ G.Node.inputs dst_ref daig with
      | [] ->
          let stmt_ref = Ref.Stmt { stmt; name = Name.(Prod (Loc src_loc, Loc dst_loc)) } in
          daig |> G.Node.insert stmt_ref
          |> G.Edge.insert (G.Edge.create stmt_ref dst_ref `Transfer)
          |> G.Edge.insert (G.Edge.create src_ref dst_ref `Transfer)
      | e :: _ as edges when Comp.equal `Join @@ G.Edge.label e ->
          let k = List.length edges in
          let stmt_ref =
            Ref.Stmt { stmt; name = Name.(Prod (Idx k, Prod (Loc src_loc, Loc dst_loc))) }
          in
          let prejoin_astate =
            Ref.AState { state = None; name = Name.(Prod (Idx k, Ref.name dst_ref)) }
          in
          daig |> G.Node.insert stmt_ref
          |> G.Edge.insert (G.Edge.create stmt_ref prejoin_astate `Transfer)
          |> G.Edge.insert (G.Edge.create src_ref prejoin_astate `Transfer)
          |> G.Edge.insert (G.Edge.create prejoin_astate dst_ref `Join)
      | e :: _ as edges when Comp.equal `Transfer @@ G.Edge.label e ->
          let old_state = if Ref.is_empty dst_ref then None else Some (Ref.astate_exn dst_ref) in
          let old_transfer_dst =
            Ref.AState { state = old_state; name = Name.(Prod (Idx 0, Ref.name dst_ref)) }
          in
          let new_transfer_dst =
            Ref.AState { state = None; name = Name.(Prod (Idx 1, Ref.name dst_ref)) }
          in
          let stmt_ref =
            Ref.Stmt { stmt; name = Name.(Prod (Idx 1, Prod (Loc src_loc, Loc dst_loc))) }
          in
          List.fold edges ~init:daig ~f:(fun daig edge ->
              G.Edge.remove edge daig
              |> G.Edge.insert (G.Edge.create (G.Edge.src edge) old_transfer_dst `Transfer))
          |> G.Node.insert stmt_ref
          |> G.Edge.insert (G.Edge.create stmt_ref new_transfer_dst `Transfer)
          |> G.Edge.insert (G.Edge.create src_ref new_transfer_dst `Transfer)
          |> G.Edge.insert (G.Edge.create old_transfer_dst dst_ref `Join)
          |> G.Edge.insert (G.Edge.create new_transfer_dst dst_ref `Join)
      | _ -> failwith "malformed daig"
    in
    dirty_from dst_ref new_daig

  (*insert a new [stmt] at a control [loc] *)
  let add_stmt_at (loc : Cfg.Loc.t) (stmt : Dom.Stmt.t) (daig : t) =
    (* loc is either (case 1) not in any loop, (case 2) in the body of a loop, or (case 3) a loop head *)
    let new_loc, new_loc_ref, old_loc_ref, add_stmt_before =
      match (ref_by_name Name.(Iterate (0, Loc loc)) daig, ref_by_name (Name.Loc loc) daig) with
      | None, Some r ->
          let new_loc = Cfg.Loc.fresh () in
          let new_loc_ref = Ref.AState { state = None; name = Name.Loc new_loc } in
          (new_loc, new_loc_ref, r, Cfg.Loc.(equal exit loc))
      | Some r, None ->
          let new_loc = Cfg.Loc.fresh () in
          let new_loc_ref = Ref.AState { state = None; name = Name.(Iterate (0, Loc new_loc)) } in
          (new_loc, new_loc_ref, r, false)
      | Some r, Some _ ->
          let new_loc = Cfg.Loc.fresh () in
          let new_loc_ref = Ref.AState { state = None; name = Name.Loc new_loc } in
          (new_loc, new_loc_ref, r, true)
      | None, None -> failwith "can't add statement at non-existent location"
    in
    let daig = G.Node.insert new_loc_ref daig in
    if add_stmt_before then
      Seq.fold (G.Node.inputs old_loc_ref daig) ~init:daig ~f:(fun daig edge ->
          G.Edge.remove edge daig
          |> G.Edge.insert (G.Edge.create (G.Edge.src edge) new_loc_ref (G.Edge.label edge)))
      |> add_stmt new_loc loc stmt
    else
      Seq.fold (G.Node.outputs old_loc_ref daig) ~init:daig ~f:(fun daig edge ->
          G.Edge.remove edge daig
          |> G.Edge.insert (G.Edge.create new_loc_ref (G.Edge.dst edge) (G.Edge.label edge)))
      |> add_stmt loc new_loc stmt

  let drop_cache d =
    match ref_by_name (Name.Loc Cfg.Loc.entry) d with
    | Some r -> dirty_from r d
    | None -> failwith "no reference found for entry location"
end

module Dom = Incr.Make (Itv)
module Daig = Make (Dom)

let%test "build daig and dump dot: arith_syntax.js" =
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      "/Users/benno/Documents/CU/code/d1a/test_cases/arith_syntax.js"
  in
  let daig = Daig.of_js_cfg_unsafe cfg in
  Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/arith_daig.dot";
  true

let%test "build daig and issue queries: while_syntax.js" =
  let cfg =
    Cfg_parser.(json_of_file >> cfg_of_json)
      "/Users/benno/Documents/CU/code/d1a/test_cases/while_syntax.js"
  in
  let l1 = Name.Loc (Cfg.Loc.of_int_unsafe 1) in
  let daig = Daig.of_js_cfg_unsafe cfg in
  Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/while_daig.dot";
  let _, daig = Daig.get_by_name Name.(Prod (Iterate (0, l1), Iterate (1, l1))) daig in
  Daig.dump_dot daig
    ~filename:"/Users/benno/Documents/CU/code/d1a/while_daig_straightline_demand.dot";
  let _, daig = Daig.get_by_name (Name.Loc Cfg.Loc.exit) daig in
  Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/while_daig_loop_demand.dot";
  let daig = Daig.add_stmt_at (Cfg.Loc.of_int_unsafe 2) Dom.Stmt.skip daig in
  Daig.dump_dot daig ~filename:"/Users/benno/Documents/CU/code/d1a/w_e1.dot";
  true
