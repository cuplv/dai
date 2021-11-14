open Dai.Import
open Domain
open Syntax

module type Sig = sig
  type absstate

  type t

  module Name : sig
    type t [@@deriving compare, hash, sexp_of]

    val pp : t pp
  end

  val of_cfg : entry_state:absstate -> cfg:Cfg.t -> fn:Cfg.Fn.t -> t

  val apply_edit :
    daig:t ->
    cfg_edit:Frontend.Tree_diff.cfg_edit_result ->
    fn:Cfg.Fn.t ->
    Frontend.Tree_diff.edit ->
    t

  val dirty : Name.t -> t -> t

  val dump_dot : filename:string -> ?loc_labeller:(Cfg.Loc.t -> string option) -> t -> unit

  val is_solved : Cfg.Loc.t -> t -> bool

  type 'a or_summary_query =
    | Result of 'a
    | Summ_qry of { callsite : Ast.Stmt.t; caller_state : absstate }

  type summarizer = callsite:Ast.Stmt.t * Name.t -> absstate -> absstate option

  val get_by_loc : ?summarizer:summarizer -> Cfg.Loc.t -> t -> absstate or_summary_query * t

  val get_by_name : ?summarizer:summarizer -> Name.t -> t -> absstate or_summary_query * t

  val read_by_loc : Cfg.Loc.t -> t -> absstate option

  val read_by_name : Name.t -> t -> absstate option

  val write_by_name : Name.t -> absstate -> t -> t

  val pred_state_exn : Name.t -> t -> absstate
end

module Make (Dom : Abstract.Dom) = struct
  module Comp = struct
    type t = [ `Transfer | `Join | `Widen | `Fix | `Transfer_after_fix of Cfg.Loc.t ]
    [@@deriving compare, equal, hash, sexp_of]

    let pp fs = function
      | `Transfer -> Format.fprintf fs "transfer"
      | `Join -> Format.fprintf fs "join"
      | `Widen -> Format.fprintf fs "widen"
      | `Fix -> Format.fprintf fs "fix"
      | `Transfer_after_fix lh ->
          Format.fprintf fs "transfer (after %a-loop converges)" Cfg.Loc.pp lh

    let to_string c =
      pp Format.str_formatter c;
      Format.flush_str_formatter ()
  end

  module Name = struct
    module T = struct
      type t =
        | Loc of Cfg.Loc.t
        | Edge of Cfg.Loc.t * Cfg.Loc.t
        | Idx of int
        (* Iteration context is a list of loop-head / iteration-count pairs, tracking the abstract iteration count of each nested loop *)
        | Iterate of (int * Cfg.Loc.t) list * t
        | Prod of t * t
      [@@deriving compare, equal, hash, sexp_of]

      let rec hash : t -> int = function
        | Loc l -> Cfg.Loc.hash l
        | Edge (src, dst) -> seeded_hash (Cfg.Loc.hash src) dst
        (*| Fn fn -> Comp.hash fn*)
        | Idx i -> Hashtbl.hash (Hashtbl.hash i)
        (* doubly hashing to avoid collisions with Loc's, since both are isomorphic to int *)
        | Prod (n1, n2) -> seeded_hash (hash n1) n2
        | Iterate (iter_ctx, n) ->
            (* ctx_hash sums a hash of each (i,loc) pair in ctx, so doesn't care about list order *)
            let ctx_hash =
              List.fold iter_ctx ~init:0 ~f:(fun acc (i, loc) ->
                  acc + seeded_hash (Hashtbl.hash i) loc)
            in
            seeded_hash ctx_hash n

      let rec pp fs = function
        | Loc l -> Cfg.Loc.pp fs l
        | Edge (src, dst) -> Format.fprintf fs "(%a->%a)" Cfg.Loc.pp src Cfg.Loc.pp dst
        (*| Fn fn -> Comp.pp fs fn*)
        | Idx i -> Format.fprintf fs "%i" i
        | Iterate (iter_ctx, n) ->
            let pp_ctx fs (i, loc) = Format.fprintf fs "%a^%i" Cfg.Loc.pp loc i in
            Format.fprintf fs "%a%a" pp n (List.pp ~pre:"{" ~suf:"}" ";" pp_ctx) iter_ctx
        | Prod (n1, n2) -> Format.fprintf fs "(%a . %a)" pp n1 pp n2

      let to_string n =
        pp Format.str_formatter n;
        Format.flush_str_formatter ()

      let _is_iterate = function Iterate _ -> true | _ -> false

      let iter_ctx = function Iterate (ic, _) -> Some ic | _ -> None

      let wrap_iterate i loop_head = function
        | Iterate (iter_ctx, n) ->
            assert (not @@ List.exists iter_ctx ~f:(snd >> Cfg.Loc.equal loop_head));
            Iterate ((i, loop_head) :: iter_ctx, n)
        | n -> Iterate ([ (i, loop_head) ], n)

      let set_iterate i loop_head = function
        | Iterate (iter_ctx, n) ->
            let rec set_ic i lh = function
              | [] ->
                  failwith
                    "setting iteration count for loop_head not in iteration context -- did you \
                     mean wrap_iterate?"
              | (j, l) :: rest ->
                  if Cfg.Loc.equal l lh then (i, l) :: rest else (j, l) :: set_ic i lh rest
            in
            Iterate (set_ic i loop_head iter_ctx, n)
        | n -> failwith (Format.asprintf "can't set_iterate on non-Iterate name %a" pp n)

      let unset_iterate loop_head = function
        | Iterate (iter_ctx, n) -> (
            let rec unset_ic = function
              | [] -> failwith "unsetting iteration count for loop_head not in iteration context"
              | (j, l) :: rest ->
                  if Cfg.Loc.equal l loop_head then rest else (j, l) :: unset_ic rest
            in
            match unset_ic iter_ctx with [] -> n | ic -> Iterate (ic, n))
        | n -> failwith (Format.asprintf "can't unset_iterate on non-Iterate name %a" pp n)
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

      let _empty = Set.empty (module T_comparator)

      let _singleton = Set.singleton (module T_comparator)
    end

    module Map = struct
      include (
        Map : module type of Map with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Map.t)

      type 'v t = 'v Map.M(T_comparator).t

      let empty = Map.empty (module T_comparator)
    end
  end

  module Ref = struct
    module T = struct
      type t =
        | Stmt of { mutable stmt : Ast.Stmt.t; name : Name.t }
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
        | Stmt { stmt; name } -> Format.fprintf fs "%a[%a]" Name.pp name Ast.Stmt.pp stmt
        | AState { state = Some s; name } -> Format.fprintf fs "%a[%a]" Name.pp name Dom.pp s
        | AState { state = None; name } -> Format.fprintf fs "%a[???]" Name.pp name

      let to_string r =
        pp Format.str_formatter r;
        Format.flush_str_formatter ()

      let stmt_exn = function
        | Stmt { stmt; name = _ } -> stmt
        | _ -> failwith "Error: stmt_exn called on reference cell with no statement"

      let _astate = function AState { state; name = _ } -> state | _ -> None

      let astate_exn = function
        | AState { state = Some phi; name = _ } -> phi
        | r ->
            failwith
              (Format.asprintf
                 "Error: astate_exn called on reference cell %a with no abstract state" pp r)

      let is_astate = function AState _ -> true | _ -> false

      let is_stmt = is_astate >> not

      let is_empty = function AState { state = None; name = _ } -> true | _ -> false

      let dirty = function
        | AState a -> a.state <- None
        | Stmt _ -> failwith "dirty should only be called on AStates in a well-formed DAIG"
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

  type absstate = Dom.t

  type t = G.t

  type 'a or_summary_query =
    | Result of 'a
    | Summ_qry of { callsite : Ast.Stmt.t; caller_state : absstate }

  type summarizer = callsite:Ast.Stmt.t * Name.t -> absstate -> absstate option

  module Or_summary_query_with_daig = struct
    (** provide infix monadic operations over ['a or_summary_query * t] *)
    module Monad_infix = struct
      let ( >>= ) (x : 'a or_summary_query * t) (f : 'a -> t -> 'b or_summary_query * t) :
          'b or_summary_query * t =
        match x with Result r, daig -> f r daig | (Summ_qry _ as sq), daig -> (sq, daig)

      let ( >>| ) (x : 'a or_summary_query * t) (f : 'a -> 'b) : 'b or_summary_query * t =
        match x with
        | Result a, daig -> (Result (f a), daig)
        | (Summ_qry _ as sq), daig -> (sq, daig)
    end
  end

  let _edges_btwn (daig : G.t) ~(src : G.node) ~(dst : G.node) : G.Edge.Set.t =
    let rec edges_btwn_impl (frontier : G.node list) (accum : G.Edge.Set.t) =
      match frontier with
      | [] -> accum
      | n :: frontier ->
          let f, a =
            Sequence.fold (G.Node.outputs n daig) ~init:(frontier, accum) ~f:(fun (f, a) e ->
                if G.Edge.Set.mem a e then (f, a)
                else if Ref.equal dst (G.Edge.dst e) then (f, G.Edge.Set.add a e)
                else (G.Edge.dst e :: f, G.Edge.Set.add a e))
          in
          edges_btwn_impl f a
    in
    edges_btwn_impl [ src ] G.Edge.Set.empty

  (* remove all nodes and edges between ~src and ~dst, including associated statements (which are not strictly-speaking on path between src and dst) *)
  let remove_daig_region (daig : G.t) ~(src : G.node) ~(dst : G.node) : G.t =
    (* traverse graph from [frontier] until reaching [dst], gathering all non-[dst] nodes along the way as well as associated statement nodes. *)
    let rec daig_region_impl (frontier : G.node list) (accum : G.Node.Set.t) =
      match frontier with
      | [] -> accum
      | n :: frontier ->
          let f, a =
            Sequence.fold (G.Node.outputs n daig) ~init:(frontier, accum) ~f:(fun (f, a) e ->
                if Set.mem a (G.Edge.dst e) then (f, a)
                else
                  let a =
                    Sequence.find (G.Node.preds (G.Edge.dst e) daig) ~f:Ref.is_stmt
                    |> Option.fold ~init:a ~f:G.Node.Set.add
                  in
                  if Ref.equal dst (G.Edge.dst e) then (f, a)
                  else (G.Edge.dst e :: f, G.Node.Set.add a (G.Edge.dst e)))
          in
          daig_region_impl f a
    in
    (* removing a node also removes all incident edges.  Therefore we also remove edges only when they are directly from [src] to [dst], since neither node is to be removed in that case.*)
    let nodes_to_remove = daig_region_impl [ src ] G.Node.Set.empty in
    Set.fold nodes_to_remove ~init:daig ~f:(flip G.Node.remove) |> fun daig ->
    Sequence.fold (G.Node.inputs dst daig) ~init:daig ~f:(fun daig e ->
        assert (G.Node.equal src @@ G.Edge.src e);
        G.Edge.remove e daig)

  type _edge = Ref.t * Ref.t * Comp.t

  let ref_by_name nm = G.nodes >> Seq.find ~f:(Ref.name >> Name.equal nm)

  let ref_by_name_exn nm g =
    match ref_by_name nm g with
    | Some r -> r
    | None -> failwith (Format.asprintf "No reference exists with name: %s" (Name.to_string nm))

  let dump_dot ~filename ?(loc_labeller = fun _ -> None) daig =
    let output_fd = Unix.openfile ~mode:[ Unix.O_WRONLY ] "/dev/null" in
    let grey = 0xdedede in
    let green = 0xaaffaa in
    let blue = 0xaaaaff in
    let string_of_node = function
      | Ref.AState { state = _; name = Name.Loc l } as r when Option.is_some (loc_labeller l) ->
          let label_prefix = Option.value_exn (loc_labeller l) in
          "\"" ^ label_prefix ^ Ref.to_string r ^ "\""
      | r -> "\"" ^ Ref.to_string r ^ "\""
    in
    Graph.to_dot
      (module G)
      daig ~filename
      ~channel:(Unix.out_channel_of_descr output_fd)
      ~string_of_node ~string_of_edge:(G.Edge.label >> Comp.to_string)
      ~node_attrs:(function
        | Ref.AState _ as n
          when Seq.exists (G.Node.outputs n daig) ~f:(G.Edge.label >> Comp.equal `Fix) ->
            [ `Color blue; `Shape `Box ]
        | Ref.AState { state = Some _; name = _ } -> [ `Color green; `Shape `Box ]
        | Ref.AState { state = None; name = _ } -> [ `Shape `Box; `Style `Dashed ]
        | Ref.Stmt _ -> [ `Color grey ])

  (** Directly implements the DAIG Encoding procedure of PLDI'21; OCaml variables are labelled by LaTeX equivalents where applicable
      [extra_back_edges] and [loop_iteration_ctx] are extensions to support DAIG construction for partial programs;
      [extra_back_edges] specifies some edges to be treated as back edges regardless of the structure of [cfg] to support modification of loop headers without re-building the corresponding loop body, while
      [loop_iteration_ctx] specifies an iteration context to wrap all abstract-state ref-cell names with, to support construction of DAIG snippets within loop bodies.
*)
  let of_region_cfg ?(extra_back_edges = []) ~(entry_ref : Ref.t) ~(cfg : Cfg.t)
      ~(entry : Cfg.Loc.t) ~(loop_iteration_ctx : (int * Cfg.Loc.t) list option)
      ?(exit : (Ref.t * Cfg.Loc.t) option) () =
    let open List.Monad_infix in
    let name_of_edge e = Name.Edge (Cfg.src e, Cfg.dst e) in
    (* E_b *)
    let back_edges = Cfg.back_edges cfg @ extra_back_edges in
    let loop_heads = Cfg.loop_heads cfg in
    let loop_head_map = Cfg.containing_loop_heads cfg in
    let containing_loop_heads l =
      Option.value (Map.find loop_head_map l) ~default:[] |> Cfg.Loc.Set.of_list
    in

    (* L_\not\sqcup, L_\sqcup *)
    let nonjoin_locs, join_locs = Cfg.locs_by_forward_indegree cfg in

    (* Optional argument is used at loop heads, where there's a separate location for the 0th iterate and the fixpoint.
       When [dst] flag is set, yields the 0th iterate name, otherwise yield the fixpoint name.  This is such that flow
       into the loop head goes to the 0th iterate and flow out is from the fixpoint.
    *)
    let name_of_loc ?(dst = false) l =
      (match (loop_iteration_ctx, Map.find loop_head_map l) with
      | None, None -> Name.Loc l
      | None, Some loopheads ->
          let ic = List.map loopheads ~f:(pair 0) in
          Name.(Iterate (ic, Loc l))
      | Some ic, None -> Name.(Iterate (ic, Loc l))
      | Some ic, Some loopheads ->
          let ic = List.fold loopheads ~init:ic ~f:(fun acc curr -> (0, curr) :: acc) in
          Name.(Iterate (ic, Loc l)))
      |> fun n ->
      if dst && List.mem loop_heads l ~equal:Cfg.Loc.equal then Name.wrap_iterate 0 l n else n
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
      backedge @ straightline @ disambiguated
    in
    (* R_{\Sigma^\sharp} *)
    let astate_refs : Ref.t list =
      let at_locs =
        Seq.to_list (Cfg.G.nodes cfg) >>| fun l ->
        if Cfg.Loc.equal l entry then entry_ref
          (* if an exit-location refcell is provided, use it instead of instantiating a new refcell*)
        else if Option.exists exit ~f:(snd >> Cfg.Loc.equal l) then
          match exit with Some (refcell, _) -> refcell | _ -> failwith "unreachable"
        else Ref.AState { state = None; name = name_of_loc l }
      and pre_joins =
        join_locs >>= fun l ->
        forward_edges_to l >>| fun (i, _) ->
        Ref.AState { state = None; name = Name.(Prod (Idx i, name_of_loc l)) }
      and pre_widens =
        back_edges >>| fun e ->
        let l = Cfg.dst e in
        let l_bar =
          Option.fold loop_iteration_ctx ~init:(name_of_loc l) ~f:(fun nm ic ->
              List.fold ic ~init:nm ~f:(fun acc (i, lh) -> Name.wrap_iterate i lh acc))
        in
        let l_bar_zero = Name.wrap_iterate 0 l l_bar in
        let l_bar_one = Name.wrap_iterate 1 l l_bar in
        Ref.AState { state = None; name = Name.Prod (l_bar_zero, l_bar_one) }
      in
      pre_widens @ pre_joins @ at_locs
    in
    (* R_\circlearrowleft *)
    let cycle_refs : Ref.t list =
      back_edges >>= fun e ->
      let l = Cfg.dst e in
      let l_bar =
        Option.fold loop_iteration_ctx ~init:(name_of_loc l) ~f:(fun nm ic ->
            List.fold ic ~init:nm ~f:(fun acc (i, lh) -> Name.wrap_iterate i lh acc))
      in
      let l_bar_zero = Name.wrap_iterate 0 l l_bar in
      let l_bar_one = Name.wrap_iterate 1 l l_bar in
      [
        Ref.AState { state = None; name = l_bar_zero };
        Ref.AState { state = None; name = l_bar_one };
      ]
    in
    let all_refs = cycle_refs @ stmt_refs @ astate_refs in
    let ref_map : Ref.t Name.Map.t =
      List.fold all_refs ~init:Name.Map.empty ~f:(fun m r ->
          match Map.add m ~key:(Ref.name r) ~data:r with
          | `Ok m -> m
          | `Duplicate ->
              if Ref.equal r @@ Map.find_exn m (Ref.name r) then m
              else
                failwith
                  (Format.asprintf "Non-uniquely-named references %a violates well-formedness"
                     Ref.pp r))
    in
    let lookup_name nm =
      match Name.Map.find ref_map nm with
      | Some r -> r
      | None ->
          let nm = Name.to_string nm in
          failwith (Format.sprintf "Name %s is not a vertex of the DCG under construction" nm)
    in
    (* C_{\denote\cdot^\sharp}, except for the last component, which is in [loop_comps] *)
    let transfer_comps =
      let straightline_transfers =
        nonjoin_locs >>= fun l ->
        let dst_astate = lookup_name (name_of_loc l ~dst:true) in
        List.fold (forward_edges_to l) ~init:[] ~f:(fun acc_edges (_idx, cfg_edge) ->
            let stmt = lookup_name (name_of_edge cfg_edge) in
            let is_into_loop_body =
              match Map.find loop_head_map (Cfg.dst cfg_edge) with
              | None -> false
              | Some heads -> List.exists heads ~f:Cfg.(Loc.equal (src cfg_edge))
            in
            (* if there is control flow (arising from return,break,do-while,exceptions, etc.) out of a loop but not out of the loop head, we must note that to ensure a fixed-point is reached *)
            let shortcircuited_loop : Cfg.Loc.t option =
              let src_loop_heads = containing_loop_heads (Cfg.src cfg_edge) in
              let dst_loop_heads = containing_loop_heads (Cfg.dst cfg_edge) in
              Cfg.Loc.Set.symmetric_diff src_loop_heads dst_loop_heads |> Sequence.to_list
              |> function
              | [ Either.First l ] when not Cfg.(Loc.equal l (dst cfg_edge)) -> Some l
              | _ -> None
            in
            let src_astate =
              lookup_name
              @@
              if is_into_loop_body then
                let l = Cfg.src cfg_edge in
                Name.wrap_iterate 0 l (name_of_loc l)
              else name_of_loc (Cfg.src cfg_edge)
            in
            let fn_symbol =
              match shortcircuited_loop with None -> `Transfer | Some lh -> `Transfer_after_fix lh
            in
            (stmt, dst_astate, fn_symbol) :: (src_astate, dst_astate, fn_symbol) :: acc_edges)
      in
      let prejoin_transfers =
        join_locs >>= fun l ->
        List.fold (forward_edges_to l) ~init:[] ~f:(fun acc (idx, edge) ->
            let stmt = lookup_name @@ Name.Prod (Idx idx, name_of_edge edge) in
            let dst = lookup_name @@ Name.Prod (Idx idx, name_of_loc l) in
            let src_astate = lookup_name @@ name_of_loc (Cfg.src edge) in
            (stmt, dst, `Transfer) :: (src_astate, dst, `Transfer) :: acc)
      in
      prejoin_transfers @ straightline_transfers
    in

    (* C_\sqcup *)
    let join_comps =
      join_locs >>= fun l ->
      let l_bar = name_of_loc l in
      let n =
        lookup_name
        @@ if List.mem loop_heads l ~equal:Cfg.Loc.equal then Name.wrap_iterate 0 l l_bar else l_bar
      in
      let ith_input i = lookup_name (Name.Prod (Name.Idx i, l_bar)) in
      forward_edges_to l >>| fun (i, _) -> (ith_input i, n, `Join)
    in

    (* C_\textsf{fix}  AND C_\nabla AND last component of C_{\denote\cdot^\sharp} *)
    let loop_comps =
      back_edges >>= fun e ->
      let l = Cfg.dst e in
      let l_bar = name_of_loc (Cfg.dst e) in
      let l0 = Name.wrap_iterate 0 l l_bar in
      let l1 = Name.wrap_iterate 1 l l_bar in
      let iter0 = lookup_name l0 in
      let iter1 = lookup_name l1 in
      let pre_widen = lookup_name (Name.Prod (l0, l1)) in
      let fixpoint = lookup_name l_bar in
      [
        (iter0, fixpoint, `Fix);
        (iter1, fixpoint, `Fix);
        (iter0, iter1, `Widen);
        (pre_widen, iter1, `Widen);
        (lookup_name @@ name_of_edge e, pre_widen, `Transfer);
        (lookup_name @@ name_of_loc (Cfg.src e), pre_widen, `Transfer);
      ]
    in
    let edges = join_comps @ loop_comps @ transfer_comps in
    Graph.create (module G) ~nodes:all_refs ~edges ()

  let of_cfg ~(entry_state : absstate) ~(cfg : Cfg.t) ~(fn : Cfg.Fn.t) : t =
    let entry_ref = Ref.AState { state = Some entry_state; name = Name.Loc fn.entry } in
    of_region_cfg ~entry_ref ~cfg ~entry:fn.entry ~loop_iteration_ctx:None ()

  let ref_at_loc_exn ~loc =
    G.nodes
    >> Seq.filter ~f:(function
         | Ref.AState { state = _; name = Name.Loc l } -> Cfg.Loc.equal l loc
         | Ref.AState { state = _; name = Name.(Iterate (iter_ctx, Loc l)) } ->
             Cfg.Loc.equal l loc && List.for_all iter_ctx ~f:(fst >> Int.equal 0)
         | _ -> false)
    >> Seq.to_list
    >> function
    | [ r ] -> r
    | [] -> failwith "error: no refs found at given location"
    | [ r1; r2 ] -> (
        match Ref.(name r1, name r2) with
        | Name.Loc _, Name.Iterate _ -> r1
        | Name.Iterate _, Name.Loc _ -> r2
        | _ ->
            failwith
              (Format.asprintf "error: malformed ref names at loop-head location %a" Cfg.Loc.pp loc)
        )
    | _ -> failwith "error: multiple refs found at given location"

  let is_solved loc daig = not @@ Ref.is_empty @@ ref_at_loc_exn ~loc daig

  let increment_iteration loop_head =
    let rec incr_iter_ctx = function
      | [] -> failwith "loop_head not found in iter_ctx"
      | (i, l) :: ctx when Cfg.Loc.equal l loop_head -> (i + 1, l) :: ctx
      | (i, l) :: ctx -> (i, l) :: incr_iter_ctx ctx
    in
    function
    | Name.Iterate (iter_ctx, n) -> Name.Iterate (incr_iter_ctx iter_ctx, n)
    | Name.Prod (Name.Iterate (ic, n), Name.Iterate (ic_prime, n_prime)) ->
        (* validate that this is a properly constructed-pre-widen name before incrementing its indices *)
        assert (Name.equal n n_prime);
        Name.Prod (Name.Iterate (incr_iter_ctx ic, n), Name.Iterate (incr_iter_ctx ic_prime, n))
    | n -> n

  (* Transform the DAIG [g] by unrolling one step further the loop whose current abstract iterate is at [curr_iter] *)
  let unroll_loop (daig : t) (curr_iter : Ref.t) =
    (* First, find the entire previous unrolling [loop_edges]: all DAIG edges backwards-reachable from [curr_iter] without going through the previous iteration *)
    let curr_idx, loop_head =
      match Ref.name curr_iter with
      | Name.(Iterate (ic, Loc l)) -> (List.find_exn ic ~f:(snd >> Cfg.Loc.equal l) |> fst, l)
      | _ -> failwith "Current iterate [curr_iter] must be an iterate."
    in
    let fixpoint = ref_by_name_exn (Name.unset_iterate loop_head (Ref.name curr_iter)) daig in
    let prev_iter =
      ref_by_name_exn (Name.set_iterate (curr_idx - 1) loop_head (Ref.name curr_iter)) daig
    in
    let next_iter =
      Ref.AState { state = None; name = increment_iteration loop_head (Ref.name curr_iter) }
    in
    let curr_pre_widen =
      ref_by_name_exn (Name.Prod (Ref.name prev_iter, Ref.name curr_iter)) daig
    in
    let next_pre_widen =
      Ref.AState { state = None; name = Name.Prod (Ref.name curr_iter, Ref.name next_iter) }
    in

    let rec get_loop frontier nodes edges =
      if Set.is_empty frontier then (nodes, edges)
      else
        let process_node acc n =
          Seq.fold (G.Node.inputs n daig) ~init:acc ~f:(fun (f, ns, es) e ->
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
      Set.fold curr_iter_loop_nodes ~init:[ next_iter; next_pre_widen ] ~f:(fun acc -> function
        | Ref.AState { state = _; name } ->
            Ref.AState { state = None; name = increment_iteration loop_head name } :: acc
        | _ -> acc)
    in
    let ref_of_name n = List.find_exn all_new_refs ~f:(Ref.name >> Name.equal n) in

    let daig = List.fold all_new_refs ~init:daig ~f:(fun acc n -> G.Node.insert n acc) in

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
    let daig_without_fix_edges =
      Seq.fold (G.Node.inputs fixpoint daig) ~init:daig ~f:(flip G.Edge.remove)
    in
    List.fold all_new_edges ~init:daig_without_fix_edges ~f:(flip G.Edge.insert)

  let dirty_from (r : Ref.t) (daig : t) =
    let change_prop_step acc n =
      let outgoing_edges = G.Node.outputs n daig in
      if Seq.is_empty outgoing_edges then (
        match Ref.name n with
        | Name.Loc _ -> (* procedure exit, all done*) acc
        | Name.Iterate (_, l) ->
            (* this is a hack -- graphlib doesn't seem to actually remove edges/nodes sometimes,
               so just jump to the loop head if we somehow end up in a dangling unrolling *)
            let lh_fp = ref_by_name_exn l daig in
            let f, lh_fps, sc_edges = acc in
            (lh_fp :: f, lh_fp :: lh_fps, sc_edges)
        | x ->
            dump_dot ~filename:"malformed.dot" daig;
            failwith
              (Format.asprintf
                 "outdegree-0 refs must be abstract state for a function or program exit location; \
                  [%a] is neither"
                 Name.pp x))
      else
        Seq.fold outgoing_edges ~init:acc ~f:(fun (frontier, lh_fps, sc_edges) e ->
            let succ = G.Edge.dst e in
            let sc_edges =
              match G.Edge.label e with `Transfer_after_fix _ -> e :: sc_edges | _ -> sc_edges
            in
            if Ref.is_empty succ then (frontier, lh_fps, sc_edges)
            else if
              Comp.equal `Fix (G.Edge.label e) && (not @@ List.mem lh_fps succ ~equal:Ref.equal)
            then
              match Ref.name succ with
              | Name.Loc l | Name.(Iterate (_, Loc l)) -> (
                  (* find the all-zero-iteration-context at this loop-head location, to dirty from there *)
                  let loop_head_ref =
                    Sequence.find_exn (G.nodes daig)
                      ~f:
                        (Ref.name >> function
                         | Name.(Iterate (ic, Loc l_prime)) when Cfg.Loc.equal l l_prime ->
                             List.for_all ic ~f:(fst >> Int.equal 0)
                             && List.exists ic ~f:(snd >> Cfg.Loc.equal l)
                         | _ -> false)
                  in
                  (* dirty from there iff it's non-empty *)
                  match loop_head_ref with
                  | Ref.AState { state = Some _; name = _ } as lh ->
                      Ref.dirty succ;

                      (lh :: succ :: frontier, succ :: lh_fps, sc_edges)
                  | _ ->
                      Ref.dirty succ;
                      (succ :: frontier, succ :: lh_fps, sc_edges))
              | _ ->
                  failwith
                    "malformed DAIG -- the destination of a `Fix edge is always the loop fixpoint"
            else (
              Ref.dirty succ;
              (succ :: frontier, lh_fps, sc_edges)))
    in
    let rec change_prop frontier loop_head_fixpoints loop_shortcircuit_edges =
      if List.is_empty frontier then (loop_head_fixpoints, loop_shortcircuit_edges)
      else
        (uncurry3 change_prop)
          (List.fold frontier
             ~init:([], loop_head_fixpoints, loop_shortcircuit_edges)
             ~f:change_prop_step)
    in
    let loop_head_fixpoints, loop_shortcircuit_edges = change_prop [ r ] [] [] in
    (* remove all edges that short-circuit loops, to be re-added after fixpoint computations are reset *)
    let daig = List.fold loop_shortcircuit_edges ~init:daig ~f:(flip G.Edge.remove) in
    (* For each encountered (and therefore dirtied) loop fixpoint, reset its fix edges to the 0th and 1st iterates,
       then remove any dangling unrollings *)
    List.fold loop_head_fixpoints ~init:daig ~f:(fun daig -> function
      | (Ref.AState { state = _; name = Name.Loc l as name } as loop_fp)
      | (Ref.AState { state = _; name = Name.(Iterate (_, Loc l)) as name } as loop_fp) -> (
          let n0, n1 = Name.(wrap_iterate 0 l name, wrap_iterate 1 l name) in
          let iter0 = ref_by_name_exn n0 daig in
          let iter1 = ref_by_name_exn n1 daig in
          let daig =
            Seq.fold (G.Node.inputs loop_fp daig) ~init:daig ~f:(flip G.Edge.remove)
            |> G.Edge.insert (G.Edge.create iter0 loop_fp `Fix)
            |> G.Edge.insert (G.Edge.create iter1 loop_fp `Fix)
          in
          match Seq.find (G.Node.succs iter1 daig) ~f:(Ref.name >> Name.equal n1) with
          | Some root_of_dangling_unrollings ->
              Graph.fold_reachable
                (module G)
                daig root_of_dangling_unrollings ~init:daig
                ~f:(fun daig n ->
                  Ref.dirty n;
                  Seq.fold ~init:daig ~f:(flip G.Edge.remove) (G.Node.inputs n daig)
                  |> G.Node.remove n)
          | _ -> daig)
      | _ ->
          failwith "malformed DAIG -- loop fixpoints are always named by their syntactic location")
    |> fun daig ->
    List.fold loop_shortcircuit_edges ~init:daig ~f:(fun daig edge ->
        let loop_head =
          match G.Edge.label edge with
          | `Transfer_after_fix l -> l
          | _ -> failwith "malformed short-circuiting edge"
        in
        let new_src_name =
          match Ref.name (G.Edge.src edge) with
          | Name.Iterate _ as n -> Name.set_iterate 0 loop_head n
          | _ -> failwith "malformed short-ciruiting edge"
        in
        match ref_by_name new_src_name daig with
        | Some src_ref -> G.Edge.(insert (create src_ref (dst edge) (label edge))) daig
        | None -> daig)

  let dirty (nm : Name.t) (daig : t) = dirty_from (ref_by_name_exn nm daig) daig

  (** IMPURE -- possibly mutates argument [g] by computing and filling empty ref cells
   * Return value is a pair, consisting of the query result (either that ref-cell, guaranteed to be nonempty, or a query for a requisite procedure summary), and a new daig [t] reflecting possible changes to the DAIG structure
   * [summarize] function is provided to this _intraprocedural_ DAIG by the orchestrating _interprocedural_ HODAIG
   *)
  let rec get (r : Ref.t) (summarizer : summarizer) (daig : t) : Ref.t or_summary_query * t =
    let open Or_summary_query_with_daig.Monad_infix in
    match r with
    | Stmt _ -> (Result r, daig)
    | AState { state = Some _; _ } -> (Result r, daig)
    | AState { state = None; _ } when Seq.is_empty (G.Node.inputs r daig) ->
        failwith "malformed DAIG: all empty cells have in-degree > 0"
    | AState phi ->
        (* recursively [get] all predecessors (or a summary query) *)
        ( Seq.fold (G.Node.preds r daig) ~init:(Result [], daig) ~f:(fun (acc, g) pred ->
              match acc with
              | Result ps -> (
                  match get pred summarizer g with
                  | Result p, g -> (Result (p :: ps), g)
                  | (Summ_qry _ as sq), g -> (sq, g))
              | _ -> (acc, g))
        (* apply analysis function (transfer, join, widen, etc.)  indicated by DAIG-edge label *)
        >>=
        fun preds daig ->
          let preds = List.sort preds ~compare:Ref.compare in
          match G.Edge.label (Seq.hd_exn (G.Node.inputs r daig)) with
          | `Transfer -> (
              match preds with
              | [ s; phi ] -> (
                  match Ref.stmt_exn s with
                  | (Ast.Stmt.Call _ as callsite) | (Ast.Stmt.Exceptional_call _ as callsite) ->
                      let res =
                        match summarizer ~callsite:(callsite, Ref.name r) (Ref.astate_exn phi) with
                        | Some phi_prime -> Result phi_prime
                        | None -> Summ_qry { callsite; caller_state = Ref.astate_exn phi }
                      in
                      (res, daig)
                  | stmt -> (Result (Dom.interpret stmt (Ref.astate_exn phi)), daig))
              | _ ->
                  failwith
                    "malformed DCG: transfer function must have one Stmt and one AState input")
          | `Join -> (Result (List.map preds ~f:Ref.astate_exn |> List.reduce_exn ~f:Dom.join), daig)
          | `Widen ->
              (Result (List.map preds ~f:Ref.astate_exn |> List.reduce_exn ~f:Dom.widen), daig)
          | `Fix -> (
              match preds with
              | [ p1; p2 ] ->
                  let iter1 = Ref.astate_exn p1 in
                  let iter2 = Ref.astate_exn p2 in
                  (* If fixpoint reached, return it.  Otherwise, unroll the loop and continue. *)
                  if Dom.equal iter1 iter2 then (Result iter1, daig)
                  else unroll_loop daig p2 |> get r summarizer >>| Ref.astate_exn
              | _ -> failwith "fix always has two inputs (by construction)")
          | `Transfer_after_fix loop_head -> (
              match preds with
              | [ s; phi ] ->
                  get_fixedpoint_wrt ~loop_head ~ref_cell:phi summarizer daig >>= fun phi_fp daig ->
                  let phi_edge =
                    G.Node.inputs r daig |> Sequence.find_exn ~f:(G.Edge.src >> Ref.is_astate)
                  in
                  let daig =
                    G.Edge.remove phi_edge daig
                    |> G.Edge.(insert (create phi_fp (dst phi_edge) (label phi_edge)))
                  in
                  (Result (Dom.interpret (Ref.stmt_exn s) (Ref.astate_exn phi_fp)), daig)
              | _ ->
                  failwith
                    "malformed DCG: transfer function must have one Stmt and one AState input") )
        (* write result to the queried ref-cell and return *)
        >>| fun result ->
        phi.state <- Some result;
        r

  (** get the fixed-point of some [ref_cell] in the body of [loop_head]'s natural loop *)
  and get_fixedpoint_wrt ~(ref_cell : Ref.t) ~(loop_head : Cfg.Loc.t) summarize daig =
    let open Or_summary_query_with_daig.Monad_infix in
    match Ref.name ref_cell with
    | Name.(Iterate (ic, l)) -> (
        let loop_head_fp =
          ref_by_name_exn Name.(Iterate (ic, Loc loop_head) |> unset_iterate loop_head) daig
        in
        get loop_head_fp summarize daig >>= fun lh_fp daig ->
        match G.Node.preds lh_fp daig |> Sequence.to_list |> List.map ~f:Ref.name with
        | [ Name.Iterate (ic1, _); Name.Iterate (ic2, _) ] ->
            let ic1_wrt_lh = List.find_exn ic1 ~f:(snd >> Cfg.Loc.equal loop_head) |> fst in
            let ic2_wrt_lh = List.find_exn ic2 ~f:(snd >> Cfg.Loc.equal loop_head) |> fst in
            let ic = if ic1_wrt_lh < ic2_wrt_lh then ic1 else ic2 in
            let ref_cell_fp = ref_by_name_exn Name.(Iterate (ic, l)) daig in
            get ref_cell_fp summarize daig
        | _ -> failwith (Format.asprintf "malformed DAIG for loop head %a" Cfg.Loc.pp loop_head))
    | n -> failwith (Format.asprintf "can't get fixedpoint of non-loop-body ref_cell %a" Name.pp n)

  and _bind_formals caller_state callsite callee =
    match callsite with
    | Ast.Stmt.Call { actuals; _ } ->
        List.fold
          (List.zip_exn (Cfg.Fn.formals callee) actuals)
          ~init:caller_state
          ~f:(fun astate (formal, actual) ->
            (* for each formal/actual pair, [[formal := actual]](astate) *)
            let binding = Ast.Stmt.Assign { lhs = formal; rhs = actual } in
            Dom.interpret binding astate)
    | _ -> failwith "malformed callsite"

  let get_by_ref_impl summarizer daig = function
    | Ref.AState { state = Some phi; name = _ } -> (Result phi, daig)
    | Ref.AState _ as r -> (
        match get r summarizer daig with
        | Result r, daig -> (Result (Ref.astate_exn r), daig)
        | (Summ_qry _ as sq), daig -> (sq, daig))
    | _ -> failwith "malformed daig: get_by_ref_impl is only used for abstract state queries"

  let get_by_loc ?(summarizer = fun ~callsite:_ _ -> None) loc daig =
    ref_at_loc_exn ~loc daig |> get_by_ref_impl summarizer daig

  let get_by_name ?(summarizer = fun ~callsite:_ _ -> None) nm daig =
    ref_by_name_exn nm daig |> get_by_ref_impl summarizer daig

  let read_by_loc loc daig =
    match ref_at_loc_exn ~loc daig with Ref.AState { state; name = _ } -> state | _ -> None

  let read_by_name nm daig =
    match ref_by_name_exn nm daig with Ref.AState { state; name = _ } -> state | _ -> None

  let write_by_name nm absstate daig =
    match ref_by_name_exn nm daig with
    | Ref.AState phi as r ->
        let daig = dirty_from r daig in
        phi.state <- Some absstate;
        daig
    | _ -> failwith (Format.asprintf "can't write to non-absstate ref-cell %a" Name.pp nm)

  let pred_state_exn nm daig =
    let r = ref_by_name_exn nm daig in
    G.Node.preds r daig |> Seq.filter ~f:Ref.is_astate |> Seq.to_list |> function
    | [ Ref.AState { state = Some phi; _ } ] -> phi
    | _ -> failwith (Format.asprintf "No predecessor state for %a" Name.pp nm)

  let apply_edit ~daig ~cfg_edit ~(fn : Cfg.Fn.t) =
    let open Frontend.Tree_diff in
    function
    | Add_function _ | Delete_function _ | Modify_function _ ->
        failwith "Can't apply interprocedural edit to intraprocedural DAIG"
    | Add_statements { method_id = _; at_loc; stmts = _ } ->
        let added_loc =
          match cfg_edit.added_loc with
          | Some l -> l
          | None -> failwith "error: Add_statements edit should always produce a fresh location"
        in
        let ref_at_loc = ref_at_loc_exn ~loc:at_loc daig in
        let daig = dirty_from ref_at_loc daig in
        let added_loc_ref =
          match Ref.name ref_at_loc with
          | Name.Loc _ -> Ref.AState { state = None; name = Name.Loc added_loc }
          | Name.(Iterate (i, Loc _)) ->
              Ref.AState { state = None; name = Name.(Iterate (i, Loc added_loc)) }
          | _ ->
              failwith
                "malformed absstate reference name -- must be either a Loc _ or an Iterate (i, Loc \
                 _)"
        in
        (* successor edges from [at_loc], to be rebased to [added_loc]*)
        let old_succ_edges = G.Node.outputs ref_at_loc daig |> Seq.to_list in
        (* rebased edges*)
        let new_succ_edges =
          List.map old_succ_edges ~f:(fun e -> G.Edge.(create added_loc_ref (dst e) (label e)))
        in
        (* statement edges previously from [at_loc], to be renamed to now be from [added_loc]*)
        let stmts_to_rename =
          List.map old_succ_edges ~f:(fun e ->
              Sequence.find_exn (G.Node.inputs (G.Edge.dst e) daig) ~f:(G.Edge.src >> Ref.is_stmt))
        in
        let rename_stmt daig stmt_edge =
          let old_stmt_node = G.Edge.src stmt_edge in
          (* two cases: old_stmt_node is named `l->l'` or `i.l->l'`;
             in each case, replace l by added_loc.
          *)
          let new_stmt_node =
            match old_stmt_node with
            | Ref.Stmt { name = Name.(Edge (_, dst_loc)); stmt } ->
                Ref.Stmt { name = Name.Edge (added_loc, dst_loc); stmt }
            | Ref.Stmt { name = Name.(Prod (Idx i, Edge (_, dst_loc))); stmt } ->
                Ref.Stmt { name = Name.(Prod (Idx i, Edge (added_loc, dst_loc))); stmt }
            | r ->
                failwith
                  (Format.asprintf "malformed name for statement ref-cell: %a" Name.pp (Ref.name r))
          in
          G.Node.remove (G.Edge.src stmt_edge) daig
          |> G.Edge.(insert (create new_stmt_node (dst stmt_edge) (label stmt_edge)))
        in
        let cfg = Graph.create (module Cfg.G) ~edges:cfg_edit.added_edges () in
        (* (1) construct a DAIG segment for the added region
           (2) union it with the existing DAIG
           (3) rebase old DAIG edges with src [at_loc], now with src [added_loc]
           (4) rename corresponding statement refs, replacing [at_loc] by [added_loc]
        *)
        let new_daig_segment =
          of_region_cfg ~cfg ~entry_ref:ref_at_loc ~entry:at_loc
            ~loop_iteration_ctx:(Name.iter_ctx (Ref.name ref_at_loc))
            ~extra_back_edges:[] ()
        in
        let unioned_daig = Graph.union (module G) daig new_daig_segment in
        let rebased_daig =
          List.fold old_succ_edges ~init:unioned_daig ~f:(flip G.Edge.remove) |> fun d ->
          List.fold new_succ_edges ~init:d ~f:(flip G.Edge.insert)
        in
        List.fold stmts_to_rename ~init:rebased_daig ~f:rename_stmt
    | Modify_statements { method_id = _; from_loc; to_loc; new_stmts = _ } ->
        let from_ref = ref_at_loc_exn ~loc:from_loc daig in
        let to_ref = ref_at_loc_exn ~loc:to_loc daig in
        Ref.dirty to_ref;
        let dirtied_daig = dirty_from to_ref daig in
        let removed_region_daig = remove_daig_region dirtied_daig ~src:from_ref ~dst:to_ref in
        let cfg = Graph.create (module Cfg.G) ~edges:cfg_edit.added_edges () in
        (* (1) remove DAIG region between [from_loc] and [to_loc]
           (2) construct a new DAIG segment for that region and union it into the old DAIG
        *)
        let new_daig_segment =
          of_region_cfg ~cfg ~entry_ref:from_ref ~entry:from_loc
            ~loop_iteration_ctx:(Name.iter_ctx (Ref.name from_ref))
            ~extra_back_edges:[] ~exit:(to_ref, to_loc) ()
        in
        Graph.union (module G) new_daig_segment removed_region_daig
    | Modify_header { method_id = _; prev_loc_ctx; next_stmt = _; loop_body_exit = _ } ->
        let at_loc_ref = ref_at_loc_exn ~loc:prev_loc_ctx.entry daig in
        let _daig = dirty_from at_loc_ref daig in
        let _extra_back_edges = Option.to_list cfg_edit.added_for_loop_backedge in
        let _cfg = Graph.create (module Cfg.G) ~edges:cfg_edit.added_edges () in
        failwith "todo: Modify_header edit"
    | Delete_statements { method_id = _; from_loc; to_loc } ->
        let from_ref = ref_at_loc_exn ~loc:from_loc daig in
        let to_ref = ref_at_loc_exn ~loc:to_loc daig in
        let old_succ_edges = G.Node.outputs to_ref daig |> Seq.to_list in
        let new_succ_edges =
          List.map old_succ_edges ~f:(fun e -> G.Edge.(create from_ref (dst e) (label e)))
        in
        (* statement edges previously from [to_loc], to be renamed to now be from [from_loc]*)
        let stmts_to_rename =
          List.map old_succ_edges ~f:(fun e ->
              Sequence.find_exn (G.Node.inputs (G.Edge.dst e) daig) ~f:(G.Edge.src >> Ref.is_stmt))
        in
        let rename_stmt daig stmt_edge =
          let old_stmt_node = G.Edge.src stmt_edge in
          (* two cases: old_stmt_node is named `l->l'` or `i.l->l'`;
             in each case, replace l by from_loc.
          *)
          let new_stmt_node =
            match old_stmt_node with
            | Ref.Stmt { name = Name.(Edge (_, dst_loc)); stmt } ->
                Ref.Stmt { name = Name.Edge (from_loc, dst_loc); stmt }
            | Ref.Stmt { name = Name.(Prod (Idx i, Edge (_, dst_loc))); stmt } ->
                Ref.Stmt { name = Name.(Prod (Idx i, Edge (from_loc, dst_loc))); stmt }
            | r ->
                failwith
                  (Format.asprintf "malformed name for statement ref-cell: %a" Name.pp (Ref.name r))
          in
          G.Node.remove (G.Edge.src stmt_edge) daig
          |> G.Edge.(insert (create new_stmt_node (dst stmt_edge) (label stmt_edge)))
        in
        (* (1) remove DAIG region between [from_loc] and [to_loc]
           (2) rebase old DAIG edges with src [to_loc], now with src [from_loc]
           (3) rename corresponding statement refs, replacing [to_loc] by [from_loc]
           NB: Special case when [to_loc] == [fn.exit], to preserve function-exit location: just replace the region by Skip instead of collapsing the two locations into one
        *)
        let dirtied_daig = dirty_from to_ref daig in
        let removed_daig =
          remove_daig_region dirtied_daig ~src:from_ref ~dst:to_ref
          |>
          if Cfg.Loc.equal to_loc fn.exit then
            let stmt_ref = Ref.Stmt { name = Name.Edge (from_loc, to_loc); stmt = Ast.Stmt.Skip } in
            G.Edge.(insert (create from_ref to_ref `Transfer))
            >> G.Edge.(insert (create stmt_ref to_ref `Transfer))
          else G.Node.remove to_ref
        in
        let rebased_daig =
          List.fold old_succ_edges ~init:removed_daig ~f:(flip G.Edge.remove) |> fun d ->
          List.fold new_succ_edges ~init:d ~f:(flip G.Edge.insert)
        in
        List.fold stmts_to_rename ~init:rebased_daig ~f:rename_stmt
end

module Dom = Domain.Unit_dom
module Daig = Make (Dom)
open Frontend

let%test "build daig, edit, and dump dot: HelloWorld.java" =
  Cfg.Loc.reset ();
  let ({ loc_map; cfgs; _ } : Cfg_parser.prgm_parse_result) =
    Frontend.Cfg_parser.parse_file_exn (abs_of_rel_path "test_cases/java/HelloWorld.java")
  in
  match Map.to_alist cfgs with
  | [ (fn, cfg) ] ->
      let daig = Daig.of_cfg ~entry_state:(Dom.init ()) ~cfg ~fn in
      Daig.dump_dot ~filename:(abs_of_rel_path "helloworld_daig.dot") daig;
      let edit =
        Frontend.Tree_diff.Delete_statements
          {
            method_id = fn.method_id;
            from_loc = Cfg.Loc.of_int_unsafe 1;
            to_loc = Cfg.Loc.of_int_unsafe 2;
          }
      in
      let cfg_edit = Frontend.Tree_diff.apply_edit edit loc_map cfg ~ret:fn.exit ~exc:fn.exc_exit in
      let edited_daig = Daig.apply_edit ~daig edit ~fn ~cfg_edit in
      Daig.dump_dot ~filename:(abs_of_rel_path "edited_helloworld_daig.dot") edited_daig;
      true
  | _ -> failwith "malformed cfg -- only one procedure in HelloWorld.java"

let%test "analyze nested loops" =
  let ({ cfgs; _ } : Cfg_parser.prgm_parse_result) =
    Frontend.Cfg_parser.parse_file_exn (abs_of_rel_path "test_cases/java/NestedLoops.java")
  in
  Map.to_alist cfgs
  |> List.iter ~f:(fun (fn, cfg) ->
         let daig = Daig.of_cfg ~entry_state:(Dom.init ()) ~cfg ~fn in
         Daig.dump_dot ~filename:(abs_of_rel_path (fn.method_id.method_name ^ ".dot")) daig;
         let _, analyzed_daig = Daig.get_by_loc fn.exit daig in
         Daig.dump_dot
           ~filename:(abs_of_rel_path ("analyzed_" ^ fn.method_id.method_name ^ ".dot"))
           analyzed_daig);
  true

let%test "analyze conditional at end of loop body" =
  let ({ cfgs; _ } : Cfg_parser.prgm_parse_result) =
    Frontend.Cfg_parser.parse_file_exn
      (abs_of_rel_path "test_cases/java/ConditionalAtLoopExit.java")
  in
  Map.to_alist cfgs
  |> List.iter ~f:(fun (fn, cfg) ->
         let daig = Daig.of_cfg ~entry_state:(Dom.init ()) ~cfg ~fn in
         Daig.dump_dot ~filename:(abs_of_rel_path (fn.method_id.method_name ^ ".dot")) daig;
         let _, analyzed_daig = Daig.get_by_loc fn.exit daig in
         Daig.dump_dot
           ~filename:(abs_of_rel_path ("analyzed_" ^ fn.method_id.method_name ^ ".dot"))
           analyzed_daig);
  true
