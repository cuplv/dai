open Dai.Import
open Option.Let_syntax

module Edge_type = struct
  type t = [ `Next_ptr | `List_seg ] [@@deriving equal]
end

module Pure = struct
  type pure_constraint = Eq of Memloc.t * Memloc.t | Neq of Memloc.t * Memloc.t
  [@@deriving equal, compare]

  let eq x y = Eq (x, y)

  let neq x y = Neq (x, y)

  type t = pure_constraint list [@@deriving equal, compare]

  (* let pp =
     let pp_constraint fs = function
       | Eq (x, y) -> Format.fprintf fs "%a=%a" Memloc.pp x Memloc.pp y
       | Neq (x, y) -> Format.fprintf fs "%a≠%a" Memloc.pp x Memloc.pp y
     in
     List.pp ~pre:"{" ~suf:"}" " ∧ " pp_constraint*)

  (* todo: be smarter, build equivalence classes *)
  let is_bot = List.exists ~f:(function Neq (a, b) when Memloc.equal a b -> true | _ -> false)

  let rec assume expr env pures =
    let open Syntax.Ast in
    let memloc_of_expr = function
      | Expr.Var v -> Env.find env v
      | Expr.Lit Lit.Null -> Some Memloc.null
      | _ -> None
    in
    match expr with
    | Expr.Binop { l; op = Binop.And; r } -> assume l env pures |> assume r env
    | Expr.Unop { op = Unop.Not; e = Expr.Unop { op = Unop.Not; e } } -> assume e env pures
    | Expr.Unop { op = Unop.Not; e = Expr.Binop { l; op; r } } ->
        if Binop.equal op Binop.Eq then assume (Expr.Binop { l; op = Binop.NEq; r }) env pures
        else if Binop.equal op Binop.NEq then assume (Expr.Binop { l; op = Binop.Eq; r }) env pures
        else pures
    | Expr.Binop { l; op = Binop.Eq; r } ->
        flip Option.cons pures
        @@ let%bind l = memloc_of_expr l in
           let%map r = memloc_of_expr r in
           eq l r
    | Expr.Binop { l; op = Binop.NEq; r } ->
        flip Option.cons pures
        @@ let%bind l = memloc_of_expr l in
           let%map r = memloc_of_expr r in
           neq l r
    | _ -> pures

  (* more inference definitely possible -- e.g. demorgans, nonnullness of assumed vars, etc.*)
end

module G = struct
  module G =
    Graph.Make
      (struct
        include Regular.Std.Opaque.Make (Memloc)

        type t = Memloc.t
      end)
      (Edge_type)

  include G

  let edge m g =
    let edges = Node.outputs m g in
    if Int.equal 1 @@ Seq.length edges then Seq.hd edges else None

  let _is_next_ptr e = Edge_type.equal `Next_ptr (Edge.label e)

  let is_list_seg e = Edge_type.equal `List_seg (Edge.label e)

  let _is_full_list e = is_list_seg e && Memloc.equal Memloc.null (Edge.dst e)

  let is_emp g = Seq.is_empty (edges g)

  let emp = Graph.create (module G) ~nodes:[] ~edges:[]

  (* Given a heap graph [g] and a memory location, split into two heap graphs:
      - fst: those nodes and edges of [g] UNreachable from the memloc
      - snd: those nodes and edges of [g] reachable from the memloc
      corresponds to M * M'(a ~~>) judgment of XISA paper; given a graph and an a, return (M,M')
  *)
  let _slice g start =
    Graph.fold_reachable
      (module G)
      g start
      ~init:(g, emp ())
      ~f:(fun (us, rs) n ->
        Node.outputs n g
        |> Seq.fold
             ~init:(Node.remove n us, Node.insert n rs)
             ~f:(fun (us, rs) e -> (Edge.remove e us, Edge.insert e rs)))

  (* Given a heap graph [g] and two memory locations [start] and [stop],
       split it into two heap graphs:
       - fst: those nodes and edges of [g] unreachable from [start] without passing through [stop]
       - snd: complement thereof
       corresponds to M * M'(a ~~> a') judgment of XISA paper; given a graph, a, and a', return (M,M')
  *)
  let _slice_region g start stop =
    let rec find_region frontier nodes edges =
      if Node.Set.is_empty frontier then (nodes, edges)
      else
        let process_node acc n =
          Seq.fold (Node.outputs n g) ~init:acc ~f:(fun (f, ns, es) e ->
              let succ = Edge.dst e in
              if not (Node.equal succ stop || Node.Set.mem ns succ) then
                (Node.Set.add f succ, Node.Set.add ns succ, Edge.Set.add es e)
              else (f, ns, Edge.Set.add es e))
        in
        (uncurry3 find_region)
          (Node.Set.fold frontier ~init:(Node.Set.empty, nodes, edges) ~f:process_node)
    in
    let slice_nodes, slice_edges =
      find_region (Node.Set.singleton start) Node.Set.empty G.Edge.Set.empty
    in
    Node.Set.fold slice_nodes
      ~init:(g, emp ())
      ~f:(fun (outs, ins) n -> (Node.remove n outs, Node.insert n ins))
    |> fun init ->
    Edge.Set.fold slice_edges ~init ~f:(fun (outs, ins) e ->
        (Edge.remove e outs, Edge.insert e ins))

  let get_linear_path g start stop =
    let rec impl acc_path curr_node =
      if Memloc.equal stop curr_node then Some acc_path
      else edge curr_node g >>= fun e -> impl (e :: acc_path) (Edge.dst e)
    in
    impl [] start

  let pp_edge fs e =
    match Edge.label e with
    | `Next_ptr -> Format.fprintf fs "%a.next = %a" Memloc.pp (Edge.src e) Memloc.pp (Edge.dst e)
    | `List_seg -> Format.fprintf fs "lseg(%a,%a)" Memloc.pp (Edge.src e) Memloc.pp (Edge.dst e)

  let pp fs g = (List.pp " * " pp_edge) fs (Seq.to_list (edges g))
end

module GP = struct
  module GP =
    Graph.Make
      (struct
        include Regular.Std.Opaque.Make (Memloc.Labelled_pair)

        type t = Memloc.t * Memloc.t * string
      end)
      (Edge_type)

  include GP

  let emp = Graph.create (module GP) ~nodes:[] ~edges:[]

  let pp_edge fs e =
    match Edge.label e with
    | `Next_ptr ->
        Format.fprintf fs "%a.next = %a" Memloc.Labelled_pair.pp (Edge.src e)
          Memloc.Labelled_pair.pp (Edge.dst e)
    | `List_seg ->
        Format.fprintf fs "lseg(%a,%a)" Memloc.Labelled_pair.pp (Edge.src e) Memloc.Labelled_pair.pp
          (Edge.dst e)

  let _pp fs g = (List.pp " * " pp_edge) fs (Seq.to_list (edges g))

  (* assign a new address to each pair, rename nodes according to environment of widen *)
  let to_non_pair_graph_and_env (g : t) =
    let nodes_with_indices = Seq.to_list (Seq.mapi (GP.nodes g) ~f:pair) in
    let new_node_of_pair (i, (a1, _a2, _v)) =
      if Memloc.(equal null a1) then Memloc.null else Memloc.of_int i
    in
    let nodes = List.map nodes_with_indices ~f:new_node_of_pair in
    let edges =
      Seq.map (GP.edges g) ~f:(fun e ->
          let src =
            List.find_exn nodes_with_indices ~f:(snd >> Node.equal (Edge.src e)) |> new_node_of_pair
          in
          let dst =
            List.find_exn nodes_with_indices ~f:(snd >> Node.equal (Edge.dst e)) |> new_node_of_pair
          in
          (src, dst, Edge.label e))
      |> Seq.to_list
    in
    let g = Graph.create (module G) ~nodes ~edges () in

    (* let e_alist = Env.to_alist e2 in
       let get_var_by_addr a =
         if Memloc.(equal null a) then "null" else
         List.find_exn e_alist ~f:(snd >> Memloc.equal a) |> fst
       in *)
    let e =
      List.map nodes_with_indices ~f:(fun (i, (_, a, v)) ->
          if Memloc.(equal null a) then ("null", Memloc.null) else (v, Memloc.of_int i))
      |> Env.of_alist_exn
    in
    (g, e)
end

module T = struct
  type t = G.t * Pure.t * Env.t [@@deriving equal, compare]

  let init =
    Graph.create (module G) ~nodes:[ Memloc.null ] ~edges:[] >> fun g ->
    (g, [], Env.empty |> Env.add_exn ~key:"null" ~data:Memloc.null)

  let bottom =
    Graph.create (module G) ~nodes:[ Memloc.null ] ~edges:[] >> fun g ->
    (g, [ Pure.neq (Memloc.of_int 0) (Memloc.of_int 0) ], Env.empty)

  let pp fs (g, _p, e) = Format.fprintf fs "MEM [%a] ; ENV [%a]" G.pp g Env.pp e

  let sanitize s = s

  let show x =
    pp Format.str_formatter x;
    Format.flush_str_formatter ()

  let _initial_phi e1 e2 =
    assert (List.equal String.equal (Env.keys e1) (Env.keys e2));
    Env.fold e2 ~init:Memloc.Map.empty ~f:(fun ~key ~data acc ->
        Memloc.Map.add_exn acc ~key:data ~data:(Env.find_exn e1 key))

  let initial_psi e1 e2 =
    assert (List.equal String.equal (Env.keys e1) (Env.keys e2));
    Env.fold e1 ~init:[] ~f:(fun ~key ~data acc -> (data, Env.find_exn e2 key, key) :: acc)

  let get_mapped_address a psi =
    if Memloc.(equal null a) then Memloc.null
    else List.find_exn psi ~f:(fst3 >> Memloc.equal a) |> snd3

  let get_var_label a psi =
    if Memloc.(equal null a) then "null"
    else List.find_exn psi ~f:(fun (a1, _a2, _v) -> Memloc.equal a a1) |> trd3

  (* apply w_partial as many times as possible; return Some(result) if a rewrite occurred and None otherwise *)
  let rec w_partial g1 g2 g3 psi =
    Seq.find (G.edges g1) ~f:(fun e ->
        Edge_type.equal (G.Edge.label e) `List_seg
        &&
        let src = G.Edge.src e in
        List.exists psi ~f:(fst3 >> Memloc.equal src)
        &&
        let dst = G.Edge.dst e in
        List.exists psi ~f:(fst3 >> Memloc.equal dst))
    >>= fun e ->
    let a1, a1_prime = (G.Edge.src e, G.Edge.dst e) in
    let a2, a2_prime = (get_mapped_address a1 psi, get_mapped_address a1_prime psi) in
    let v, v_prime = (get_var_label a1 psi, get_var_label a1_prime psi) in
    G.get_linear_path g2 a2 a2_prime >>| fun e2s ->
    let g1 = G.Edge.remove e g1 in
    let g2 = List.fold e2s ~init:g2 ~f:(flip G.Edge.remove) in
    let g3 =
      GP.Edge.insert (GP.Edge.create (a1, a2, v) (a1_prime, a2_prime, v_prime) `List_seg) g3
    in
    match w_partial g1 g2 g3 psi with Some x -> x | None -> (g1, g2, g3)

  (* apply w_aliases as many times as possible; return Some(result) if a rewrite occurred and None otherwise *)
  let rec w_aliases g1 g2 g3 psi =
    let rec find_alias = function
      | [] -> None
      | (a1, a2, v) :: psi -> (
          match List.find psi ~f:(fst3 >> Memloc.equal a1) with
          | Some (_, a2_prime, v_prime) -> Some (a1, a2, a2_prime, psi, v, v_prime)
          | None -> find_alias psi )
    in

    find_alias psi >>= fun (a1, a2, a2_prime, psi, v, v_prime) ->
    let process_alias a1 a2 a2_prime psi v v_prime =
      G.get_linear_path g2 a2 a2_prime >>| fun edges ->
      let g2 = List.fold edges ~init:g2 ~f:(flip G.Edge.remove) in
      let g3 = GP.Edge.insert (GP.Edge.create (a1, a2, v) (a1, a2_prime, v_prime) `List_seg) g3 in
      match w_aliases g1 g2 g3 psi with Some x -> x | None -> (g1, g2, g3)
    in
    (*try applying w_aliases in BOTH directions -- a2 to a2_prime and vice versa. *)
    match process_alias a1 a2 a2_prime psi v v_prime with
    | None -> process_alias a1 a2_prime a2 psi v_prime v
    | x -> x

  (* apply m_pointsto as many times as possible; return Some(result) if a rewrite occurred and None otherwise *)
  let m_pointsto g1 g2 g3 psi =
    let should_match e1 e2 =
      Memloc.(equal null (G.Edge.dst e1))
      && Memloc.(equal null (G.Edge.dst e2))
      && Edge_type.equal (G.Edge.label e1) (G.Edge.label e2)
      && Edge_type.equal (G.Edge.label e1) `Next_ptr
    in
    List.fold psi ~init:(g1, g2, g3, psi, false) ~f:(fun (g1, g2, g3, psi, rewrote) (a1, a2, v) ->
        match (G.edge a1 g1, G.edge a2 g2) with
        | Some e1, Some e2 when should_match e1 e2 ->
            let a1_prime, a2_prime = (G.Edge.dst e1, G.Edge.dst e2) in
            let dst = (a1_prime, a2_prime, get_var_label a1_prime psi) in
            ( G.Edge.remove e1 g1,
              G.Edge.remove e2 g2,
              GP.Edge.insert (GP.Edge.create (a1, a2, v) dst `Next_ptr) g3,
              dst :: psi,
              true )
        | _, _ -> (g1, g2, g3, psi, rewrote))
    |> function
    | g1, g2, g3, _, true -> Some (g1, g2, g3)
    | _ -> None

  (* apply m_checker as many times as possible; return Some(result) if a rewrite occurred and None otherwise *)
  let m_checker g1 g2 g3 psi =
    let should_match e1 e2 =
      Edge_type.equal (G.Edge.label e1) (G.Edge.label e2)
      && Edge_type.equal (G.Edge.label e1) `List_seg
    in
    List.fold psi ~init:(g1, g2, g3, psi, false) ~f:(fun (g1, g2, g3, psi, rewrote) (a1, a2, v) ->
        match (G.edge a1 g1, G.edge a2 g2) with
        | Some e1, Some e2 when should_match e1 e2 ->
            let a1_prime, a2_prime = (G.Edge.dst e1, G.Edge.dst e2) in
            let dst = (a1_prime, a2_prime, get_var_label a1_prime psi) in
            ( G.Edge.remove e1 g1,
              G.Edge.remove e2 g2,
              GP.Edge.insert (GP.Edge.create (a1, a2, v) dst `List_seg) g3,
              dst :: psi,
              true )
        | _, _ -> (g1, g2, g3, psi, rewrote))
    |> function
    | g1, g2, g3, _, true -> Some (g1, g2, g3)
    | _ -> None

  (** least common environment: materialize addresses such that environment domains are equal *)
  let lce g1 e1 g2 e2 =
    String.Map.fold2 e1 e2 ~init:(g1, e1, g2, e2) ~f:(fun ~key ~data (g1, e1, g2, e2) ->
        match data with
        | `Both _ -> (g1, e1, g2, e2)
        | `Left _ ->
            let a = Memloc.fresh () in
            (g1, e1, G.Node.insert a g2, Env.add_exn ~key ~data:a e2)
        | `Right _ ->
            let a = Memloc.fresh () in
            (G.Node.insert a g1, Env.add_exn ~key ~data:a e1, g2, e2))

  let widen (g1, _p1, e1) (g2, _p2, e2) =
    (* todo: proces pures, rename into new address-space *)
    let g1, e1, g2, e2 = lce g1 e1 g2 e2 in
    let psi = initial_psi e1 e2 in
    let g3 = GP.emp () in

    let rec widen_impl g1 g2 g3 psi =
      if G.is_emp g1 && G.is_emp g2 then Some g3
      else
        let rewrote = ref false in
        let g1, g2, g3 =
          match w_partial g1 g2 g3 psi with
          | Some x ->
              rewrote := true;
              x
          | None -> (g1, g2, g3)
        in
        let g1, g2, g3 =
          match m_checker g1 g2 g3 psi with
          | Some x ->
              rewrote := true;
              x
          | None -> (g1, g2, g3)
        in
        let g1, g2, g3 =
          match w_aliases g1 g2 g3 psi with
          | Some x ->
              rewrote := true;
              x
          | None -> (g1, g2, g3)
        in
        let g1, g2, g3 =
          match m_pointsto g1 g2 g3 psi with
          | Some x ->
              rewrote := true;
              x
          | None -> (g1, g2, g3)
        in
        if !rewrote then widen_impl g1 g2 g3 psi else None
    in

    match widen_impl g1 g2 g3 psi with
    | None -> init ()
    | Some upperbound ->
        let g, e = GP.to_non_pair_graph_and_env upperbound in
        (g, [], e)

  let join = widen

  let is_bot (_, p, _) = Pure.is_bot p

  let handle_return ~caller_state ~return_state ~callsite:_ ~callee_defs:_ =
    let _return_graph, _return_pures, return_env = return_state in
    let _caller_graph, _caller_pures, _caller_env = caller_state in
    match Env.find return_env Syntax.Cfg.retvar with
    | None -> return_state
    | Some _retval_memloc ->
        (* bind lhs of callsite to retval_memloc *)
        (* handle shadowing: keep caller_env bindings that were shadowed?  need that list of defs for the callee for that though*)
        failwith "todo"

  (*let process_pures (g,p,e) = build equivalence classes, collapse addresses known to be equal, go to bottom if contradiction; apply after each [interpret]?*)

  let interpret stmt (g, p, e) =
    let open Syntax.Ast in
    match stmt with
    | Stmt.Assign { lhs; rhs = Expr.Lit Lit.Null } ->
        (g, p, Env.update e lhs ~f:(fun _ -> Memloc.null))
    | Stmt.Assign { lhs; rhs = Expr.Var v } ->
        let is_ret = String.equal Syntax.Cfg.retvar in
        let new_e =
          match Env.find e v with
          | Some a -> Env.update e lhs ~f:(fun _ -> a)
          | None ->
              let a = Memloc.fresh () in
              let e = Env.add_exn e ~key:v ~data:a in
              Env.update e lhs ~f:(fun _ -> a)
        in
        if
          is_ret lhs
          && (Option.is_some @@ G.get_linear_path g (Env.find_exn new_e Syntax.Cfg.retvar) Memloc.null)
        then
          (* return value is a well-formed list -- project it out and forget locals *)
          let ret_addr = Env.find_exn new_e Syntax.Cfg.retvar in
          ( G.Edge.insert (G.Edge.create ret_addr Memloc.null `List_seg) (G.emp ()),
            p,
            Env.of_alist_exn [ (Syntax.Cfg.retvar, ret_addr) ] )
        else (g, p, new_e)
    | Stmt.Assign { lhs; rhs = Expr.Deref { rcvr = Expr.Var rcvr; field = "next" } } -> (
        let g, e, rcvr_addr =
          match Env.find e rcvr with
          | Some a -> (G.Node.insert a g, e, a)
          | None ->
              let a = Memloc.fresh () in
              (G.Node.insert a g, Env.add_exn e ~key:rcvr ~data:a, a)
        in
        let p = Pure.neq rcvr_addr Memloc.null :: p in
        match G.edge rcvr_addr g with
        | None ->
            (* pre-state has no edge from rcvr_addr;
               - materialize a new address
               - bind lhs variable to it
               - draw a next-ptr edge from rcvr_addr to it
            *)
            let next_addr = Memloc.fresh () in
            let next_edge = G.Edge.create rcvr_addr next_addr `Next_ptr in
            let env = Env.update e lhs ~f:(fun _ -> next_addr) in
            (G.Edge.insert next_edge g, p, env)
        | Some smry when Edge_type.equal `List_seg (G.Edge.label smry) ->
            (* pre-state has a summary edge from rcvr_addr;
               - materialize a new address
               - bind lhs variable to it
               - draw a next-ptr edge from rcvr_addr to it
               - draw a summary edge from it to target of original summary edge
            *)
            let next_addr = Memloc.fresh () in
            let next_edge = G.Edge.create rcvr_addr next_addr `Next_ptr in
            let new_smry = G.Edge.create next_addr (G.Edge.dst smry) `List_seg in
            let env = Env.update e lhs ~f:(fun _ -> next_addr) in
            let g = G.Edge.remove smry g |> G.Edge.insert next_edge |> G.Edge.insert new_smry in
            (g, p, env)
        | Some next ->
            (* pre-state has a next ptr edge from rcvr addr;
               - bind lhs variable to its destination
            *)
            (g, p, Env.update e lhs ~f:(fun _ -> G.Edge.dst next)) )
    | Stmt.Write { rcvr; field = "next"; rhs } -> (
        let g, e, rcvr_addr =
          match Env.find e rcvr with
          | Some a -> (G.Node.insert a g, e, a)
          | None ->
              let a = Memloc.fresh () in
              (G.Node.insert a g, Env.add_exn e ~key:rcvr ~data:a, a)
        in
        match rhs with
        | Expr.Var rhs -> (
            let g, e, dst_addr =
              match Env.find e rhs with
              | Some a -> (G.Node.insert a g, e, a)
              | None ->
                  let a = Memloc.fresh () in
                  (G.Node.insert a g, Env.add_exn e ~key:rhs ~data:a, a)
            in
            let new_edge = G.Edge.create rcvr_addr dst_addr `Next_ptr in
            match G.edge rcvr_addr g with
            | Some edge -> (G.Edge.remove edge g |> G.Edge.insert new_edge, p, e)
            | None -> (G.Edge.insert new_edge g, p, e) )
        | Expr.Lit Lit.Null -> (
            let new_edge = G.Edge.create rcvr_addr Memloc.null `Next_ptr in
            match G.edge rcvr_addr g with
            | Some edge -> (G.Edge.remove edge g |> G.Edge.insert new_edge, p, e)
            | None -> (G.Edge.insert new_edge g, p, e) )
        | _ -> (g, p, e) )
    | Stmt.Throw { exn = _ } -> bottom ()
    | Stmt.Assume expr -> (g, Pure.assume expr e p, e)
    | _ -> (g, p, e)

  let hash seed (g, p, e) = seeded_hash seed g |> flip seeded_hash p |> flip seeded_hash e

  let sexp_of_t _ = failwith "todo: sexp_of_t"

  let t_of_sexp _ = failwith "todo: t_of_sexp"

  let hash_fold_t _ _ = failwith "todo: hash_fold_t"

  (** implements appendix A of XISA TR*)
  let implies (g1, _p1, e1) (g2, _p2, e2) =
    (* todo: do something with pures? *)
    let phi =
      Env.fold e2 ~init:Memloc.Map.empty ~f:(fun ~key ~data acc ->
          if Env.mem e1 key then Memloc.Map.add_exn acc ~key:data ~data:(Env.find_exn e1 key)
          else acc)
    in
    Memloc.Map.fold phi
      ~init:(Some (g1, g2, phi))
      ~f:(fun ~key ~data acc ->
        acc >>= fun (g1, g2, phi) ->
        let a2 = key and a1 = data in
        let%bind e1 = G.edge a1 g1 in
        let%bind e2 = G.edge a2 g2 in
        match (G.Edge.label e1, G.Edge.label e2) with
        | `Next_ptr, `Next_ptr | `List_seg, `List_seg -> (
            match Memloc.Map.add phi ~key:(G.Edge.dst e2) ~data:(G.Edge.dst e1) with
            | `Ok phi -> Some (G.Edge.remove e1 g1, G.Edge.remove e2 g2, phi)
            | `Duplicate -> failwith "todo" )
        | `Next_ptr, `List_seg ->
            failwith "ask evan -- is implementing implies even necessary here? I suspect not."
        | `List_seg, `Next_ptr -> failwith "ask evan")
    |> fun _ -> failwith "todo"
end

include T
(*module IncrT = Dai.Context.MakeInsensitive (Domain.Incr.Make (T))
module Daig = Analysis.Daig.Make (IncrT)

let%test "build daig, analyze, dump dot: list_append.js" =
  let cfg =
    Dai.Cfg_parser.(json_of_file >> cfg_of_json) (abs_of_rel_path "test_cases/list_append.js")
  in
  let a0, a1 = (Memloc.of_int 0, Memloc.of_int 1) in
  let env = Env.of_alist_exn [ ("p", a0); ("q", a1) ] in
  let mem =
    Graph.create
      (module G)
      ~nodes:[ a0; a1; Memloc.null ]
      ~edges:[ (a1, Memloc.null, `List_seg); (a0, Memloc.null, `List_seg) ]
      ()
  in
  let init_state : IncrT.t = Obj.magic (mem, [], env) in
  let daig = Daig.of_cfg ~init_state cfg in
  Daig.dump_dot daig ~filename:(abs_of_rel_path "out/daig/list_append_pre.dot");
  let exit_loc = Daig.Name.Loc (Dai.Cfg.Loc.exit, IncrT.Ctx.init) in
  let _, daig = Daig.get_by_name exit_loc daig in
  Daig.dump_dot daig ~filename:(abs_of_rel_path "out/daig/list_append_post.dot");
  true
*)
