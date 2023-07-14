open Dai
open Import

module Loc = struct
  module T : sig
    type t [@@deriving equal, compare, hash, sexp]

    val fresh : unit -> t

    val sample : unit -> t
    (** Select at random a non-fresh location *)

    val of_int_unsafe : int -> t
    (** "Unsafe" in the sense that it can construct non-fresh names; future [fresh] names will still be fresh. *)

    val reset : unit -> unit

    val pp : t pp

    val to_string : t -> string
  end = struct
    type t = int [@@deriving equal, compare, hash, sexp]

    let next = ref 1

    let reset () = next := 1

    let of_int_unsafe i = i

    (** Use Splittable_random instead of Base.Random to avoid interfering with program edit generation for experiments  *)

    let rng_state = Splittable_random.State.of_int (Caml.Random.int 1000)

    let sample () = Splittable_random.int rng_state ~lo:0 ~hi:(pred !next)

    let fresh () =
      let curr = !next in
      next := curr + 1;
      curr

    let pp fs l = Format.fprintf fs "l%i" l

    let to_string l =
      pp Format.str_formatter l;
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

    let of_list = Set.of_list (module T_comparator)
  end

  module Map = struct
    include (
      Map : module type of Map with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Map.t)

    type 'v t = 'v Map.M(T_comparator).t

    let empty = Map.empty (module T_comparator)
  end
end

module G =
  Graph.Make
    (struct
      include Regular.Std.Opaque.Make (Loc)

      type t = Loc.t
    end)
    (Ast.Stmt)

module Fn = struct
  module T = struct
    type t = {
      method_id : Method_id.t;
      formals : string list;
      locals : string list;
      entry : Loc.t;
      exit : Loc.t;
      exc_exit : Loc.t;
    }
    [@@deriving compare, equal, hash, sexp_of]

    let method_id { method_id; _ } = method_id

    let formals { formals; _ } = formals

    let locals { locals; _ } = locals

    let entry { entry; _ } = entry

    let exit { exit; _ } = exit

    let exc_exit { exc_exit; _ } = exc_exit

    let pp fs { method_id; _ } = Format.fprintf fs "FN[%a]" Method_id.pp method_id

    let make ~method_id ~formals ~entry ~exit ~exc_exit ~body =
      let locals =
        List.fold body ~init:[] ~f:(fun locals (_, _, stmt) ->
            Option.cons (Ast.Stmt.def stmt) locals)
      in
      { method_id; formals; locals; entry; exit; exc_exit }

    let defs { formals; locals; _ } = List.append formals locals

    let is_same_class fn1 fn2 =
      String.equal fn1.method_id.class_name fn2.method_id.class_name
      && List.equal String.equal fn1.method_id.package fn2.method_id.package
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

    let of_list = Set.of_list (module T_comparator)
  end

  module Map = struct
    include (
      Map : module type of Map with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Map.t)

    type 'v t = 'v Map.M(T_comparator).t

    let empty = Map.empty (module T_comparator)

    let fn_by_method_id m_id = keys >> List.find ~f:(fun f -> Method_id.equal m_id f.method_id)

    let fn_by_method_id_exn m_id map =
      match fn_by_method_id m_id map with
      | Some fn -> fn
      | None ->
          failwith (Format.asprintf "no function available for method_id: %a" Method_id.pp m_id)
  end
end

type t = G.t

let src : G.Edge.t -> Loc.t = G.Edge.src

let dst : G.Edge.t -> Loc.t = G.Edge.dst

let empty () = Fn.Map.empty

let set_fn_cfg fn ~cfg prgm = Fn.Map.set prgm ~key:fn ~data:cfg

let add_fn fn ~edges prgm =
  let cfg = Graph.create (module G) ~edges () in
  assert (not @@ Fn.Map.mem prgm fn);
  set_fn_cfg fn ~cfg prgm

let remove_fn method_id prgm =
  match
    List.find (Fn.Map.keys prgm) ~f:(fun (f : Fn.t) -> Method_id.equal method_id f.method_id)
  with
  | Some fn -> Fn.Map.remove prgm fn
  | None ->
      failwith
        (Format.asprintf "Can't remove function %a: does not exist in CFG" Method_id.pp method_id)

let retvar = "RET_VAR"

let exc_retvar = "EXCEPTIONAL_RET_VAR"

let containing_fn loc (cfg, fns) =
  Fn.Set.find fns ~f:(fun fn -> Graph.is_reachable (module G) cfg (Fn.entry fn) loc)

(** Algorithm given in 2nd ed. Dragon book section 9.6.6, MODIFIED TO EXCLUDE THE LOOP HEAD ITSELF *)
let natural_loop backedge cfg =
  let rec nat_loop_impl (wl, loop) =
    if Set.is_empty wl then loop
    else
      let process_node acc loc =
        Seq.fold (G.Node.preds loc cfg) ~init:acc ~f:(fun (acc_wl, acc_loop) pred ->
            if
              not
                (Loc.equal pred (src backedge)
                || Loc.equal pred (dst backedge)
                || Set.mem acc_loop pred)
            then (Set.add acc_wl pred, Set.add acc_loop pred)
            else (acc_wl, acc_loop))
      in
      nat_loop_impl (Set.fold wl ~init:(Loc.Set.empty, loop) ~f:process_node)
  in
  let loop_tail_singleton = Loc.Set.singleton (src backedge) in
  nat_loop_impl @@ pair loop_tail_singleton loop_tail_singleton

let back_edges =
  Graph.depth_first_search
    (module G)
    ~init:[]
    ~leave_edge:(fun kind e acc -> match kind with `Back -> e :: acc | _ -> acc)

let loop_heads = back_edges >> List.map ~f:dst

(** Map from locations to loop heads of containing loops.  Locations not in any loop are not in the domain, so [find_exn] is unsafe! *)
let containing_loop_heads cfg : Loc.t list Loc.Map.t =
  let natural_loops =
    back_edges cfg |> List.map ~f:(fun e -> (natural_loop e cfg, e))
    (*    |> List.sort ~compare:(fun (nat_loop_1,_) (nat_loop_2,_) -> Int.compare (Set.length nat_loop_1) (Set.length nat_loop_2))*)
  in

  List.fold natural_loops ~init:Loc.Map.empty ~f:(fun map (loop, edge) ->
      let head = dst edge in
      Set.fold loop ~init:map ~f:(fun map l ->
          match Map.find map l with
          | None -> Map.set map ~key:l ~data:[ head ]
          | Some heads -> Map.set map ~key:l ~data:(head :: heads)))

(** Returns a partition of [cfg]'s nodes into non-join and join locations;
      the [fst] list returned contains all nodes with at most 1 incoming forward edge,
      and the [snd] contains those with 2 or more.
   *)
let locs_by_forward_indegree cfg : G.Node.t list * G.Node.t list =
  let back_edges = back_edges cfg in
  Graph.depth_first_search
    (module G)
    ~init:([], [])
    ~leave_node:(fun _order n (nj_acc, j_acc) ->
      let forward_indegree =
        Seq.count (G.Node.inputs n cfg) ~f:(List.mem back_edges ~equal:G.Edge.equal >> not)
      in
      if forward_indegree <= 1 then (n :: nj_acc, j_acc) else (nj_acc, n :: j_acc))
    cfg

let reachable_subgraph (cfg : G.t) ~(from : G.node) : G.t =
  let edges =
    Graph.fold_reachable
      (module G)
      cfg from ~init:[]
      ~f:(fun es n ->
        Seq.fold (G.Node.outputs n cfg) ~init:es ~f:(fun es e ->
            (G.Edge.src e, G.Edge.dst e, G.Edge.label e) :: es))
  in
  Graph.create (module G) ~edges ()

let edges_btwn (cfg : G.t) ~(src : G.node) ~(dst : G.node) : G.Edge.Set.t =
  let rec edges_btwn_impl (frontier : G.node list) (accum : G.Edge.Set.t) =
    match frontier with
    | [] -> accum
    | n :: frontier ->
        let f, a =
          Sequence.fold (G.Node.outputs n cfg) ~init:(frontier, accum) ~f:(fun (f, a) e ->
              if G.Edge.Set.mem a e then (f, a)
              else if Loc.equal dst (G.Edge.dst e) then (f, G.Edge.Set.add a e)
              else (G.Edge.dst e :: f, G.Edge.Set.add a e))
        in
        edges_btwn_impl f a
  in
  edges_btwn_impl [ src ] G.Edge.Set.empty

let dump_dot_intraproc (exc_exit_node : Loc.t) ?print ~filename (cfg : t) =
  let cfg = G.Node.remove exc_exit_node cfg in
  let output_fd =
    if Option.is_some print then Unix.stdout else Unix.openfile ~mode:[ Unix.O_WRONLY ] "/dev/null"
  in
  Graph.to_dot
    (module G)
    cfg ~filename
    ~channel:(Unix.out_channel_of_descr output_fd)
    ~string_of_node:Loc.to_string
    ~string_of_edge:(G.Edge.label >> Ast.Stmt.to_string);
  if Option.is_none print then Unix.close output_fd

let dump_dot_interproc ?print ~filename (cfg : t Fn.Map.t) =
  let unioned_cfg =
    match Map.data cfg with
    | [] -> G.empty
    | g :: gs -> List.fold gs ~init:g ~f:(Graph.union (module G))
  in
  let fns = Map.keys cfg |> Fn.Set.of_list in
  let output_fd =
    if Option.is_some print then Unix.stdout else Unix.openfile ~mode:[ Unix.O_WRONLY ] "/dev/null"
  in
  Graph.to_dot
    (module G)
    unioned_cfg ~filename
    ~channel:(Unix.out_channel_of_descr output_fd)
    ~string_of_node:Loc.to_string
    ~string_of_edge:(G.Edge.label >> Ast.Stmt.to_string)
    ~node_attrs:(fun n ->
      match Set.find fns ~f:(Fn.entry >> Loc.equal n) with
      | Some fn ->
          let label =
            Format.fprintf Format.str_formatter "%a entry: %a" Fn.pp fn Loc.pp n;
            Format.flush_str_formatter ()
          in
          [ `Label label ]
      | None -> []);
  if Option.is_none print then Unix.close output_fd

(** Given a program [cfg] and a location [loc] therein, return those sequences of callsites such that [loc] is reachable from the program entry with that sequence on the stack *)
let call_chains_to ~loc:_ ~cfg:_ : G.edge list list = failwith "todo"

(* match containing_fn loc cfg with
   | None -> [ [] ] (* in main function, reachable with empty stack *)
   | Some fn ->
       (* in other function; recursively get chains to each of its callers, appending that caller to each chain*)
       Seq.filter
         (G.edges (fst cfg))
         ~f:(fun _e ->
           (*Ast.Stmt.callee (G.Edge.label e)*)
           match failwith "todo" with
           | Some edge_callee -> String.equal (Fn.name fn) edge_callee
           | None -> false)
       |> Seq.to_list
       |> List.bind ~f:(fun caller ->
              call_chains_to ~loc:(G.Edge.src caller) ~cfg |> List.(map ~f:(cons caller)))
*)
(* module Interpreter (Dom : sig
     include Abstract.Dom
   end) =
     struct
       module G = Cfg(Dom.Stmt)
     module State = struct
       module T = struct
         type t = Loc.t * Dom.t [@@deriving compare, sexp]
       end

       include T
       include Comparable.Make (T)

       let pp fs (l, d) = Format.fprintf fs "%a -> %a" Loc.pp l Dom.pp d
     end

     let step cfg : State.t -> Set.M(State).t =
      fun (l, s) ->
       let interpret edge = (dst edge, Dom.interpret (G.Edge.label edge) s) in
       Seq.map ~f:interpret (G.Node.outputs l cfg)
       |> Seq.fold ~init:(Set.empty (module State)) ~f:Set.add

     (* fully path-sensitive; terminates iff cfg's state space is finite *)
     let collect (cfg : G.t) : Set.M(State).t =
       let worklist = ref (Set.singleton (module State) (Loc.entry, Dom.init ())) in
       let accum = ref !worklist in
       (* make a (nondeterministic) small step from [state];
        *  - add new non-bottom states to both the worklist and the collecting semantics
        *  - discard previously-seen/bottom states
        *)
       let process_state wl state =
         Set.fold (step cfg state) ~init:wl ~f:(fun wl s ->
             if Set.mem !accum s || Dom.is_bot (snd s) then wl
             else (
               accum := Set.add !accum s;
               Set.add wl s ))
       in
       while not @@ Set.is_empty !worklist do
         (* step all states in the worklist *)
         let new_worklist = Set.fold !worklist ~init:(Set.empty (module State)) ~f:process_state in
         worklist := new_worklist
       done;
       !accum
   end*)
