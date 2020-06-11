open Import

module Loc : sig
  type t [@@deriving equal, compare, hash, sexp]

  val entry : t

  val exit : t

  val fresh : unit -> t

  val reset : unit -> unit

  val pp : t pp

  val to_string : t -> string
end = struct
  type t = int [@@deriving equal, compare, hash, sexp]

  let entry = 0

  let exit = -1

  let next = ref 1

  let reset () = next := 1

  let fresh () =
    let curr = !next in
    next := curr + 1;
    curr

  let pp fs l =
    match l with
    | 0 -> Format.fprintf fs "entry"
    | -1 -> Format.fprintf fs "exit"
    | _ -> Format.fprintf fs "l%i" l

  let to_string l =
    pp Format.str_formatter l;
    Format.flush_str_formatter ()
end

module G =
  Graph.Make
    (struct
      include Regular.Std.Opaque.Make (Loc)

      type t = Loc.t
    end)
    (Ast.Stmt)

type t = G.t

let src : G.Edge.t -> Loc.t = G.Edge.src >> G.Node.label

let dst : G.Edge.t -> Loc.t = G.Edge.dst >> G.Node.label

let dump_dot ?print ~filename cfg =
  let output_fd =
    if Option.is_some print then Unix.stdout
    else Unix.openfile ~mode:[ Unix.O_WRONLY ] "/dev/null"
  in
  Graph.to_dot
    (module G)
    cfg ~filename
    ~channel:(Unix.out_channel_of_descr output_fd)
    ~string_of_node:(G.Node.label >> Loc.to_string)
    ~string_of_edge:(G.Edge.label >> Ast.Stmt.to_string);
  if Option.is_none print then Unix.close output_fd

module Interpreter (Dom : Abstract.Dom) = struct
  module State = struct
    module T = struct
      type t = Loc.t * Dom.t [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)

    let pp fs (l, d) = Format.fprintf fs "%a -> %a" Loc.pp l Dom.pp d
  end

  let step (cfg : G.t) : State.t -> Set.M(State).t =
   fun (l, s) ->
    let interpret edge = (dst edge, Dom.interpret (G.Edge.label edge) s) in
    Seq.map ~f:interpret (G.Node.outputs l cfg)
    |> Seq.fold ~init:(Set.empty (module State)) ~f:Set.add

  (* fully path-sensitive; terminates iff cfg's state space is finite *)
  let collect (cfg : G.t) : Set.M(State).t =
    let worklist = ref (Set.singleton (module State) (Loc.entry, Dom.init)) in
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
      let new_worklist =
        Set.fold !worklist ~init:(Set.empty (module State)) ~f:process_state
      in
      worklist := new_worklist
    done;
    !accum
end
