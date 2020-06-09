open Import

module Loc : sig
  type t

  val compare : t -> t -> int

  val hash : t -> int

  val init : t

  val fresh : unit -> t
end = struct
  type t = int

  let compare = Int.compare

  let hash i = Hashtbl.hash i

  let init = 0

  let next = ref 0

  let fresh () =
    let curr = !next in
    next := curr + 1;
    curr
end

module G =
  Graph.Make
    (struct
      include Regular.Std.Opaque.Make (Loc)

      type t = Loc.t
    end)
    (Ast.Stmt)

type t = G.t

let rec of_ast ?entry ?exit : Ast.Stmt.t -> t =
  let entry_loc = Option.value entry ~default:(Loc.fresh ()) in
  let exit_loc = Option.value exit ~default:(Loc.fresh ()) in
  function
  | Ast.Stmt.Skip as s ->
      Graph.create (module G) ~edges:[ (entry_loc, exit_loc, s) ] ()
  | x ->
      ignore @@ of_ast x;
      failwith "todo"

let dump_dot ?print ~filename cfg =
  let output_fd =
    if Option.is_some print then Unix.stdout
    else Unix.openfile ~mode:[ Unix.O_WRONLY ] "/dev/null"
  in
  Graph.to_dot
    (module G)
    cfg ~filename
    ~channel:(Unix.out_channel_of_descr output_fd)
    ~string_of_edge:(G.Edge.label >> Ast.Stmt.to_string);
  if Option.is_none print then Unix.close output_fd
