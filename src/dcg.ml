let seeded_hash = Hashtbl.seeded_hash

(* stashing because Import shadows Hashtbl *)

open Import

module Make (Dom : Domain_intf.Dom) = struct
  module Comp = struct
    type t = [ `Transfer | `Join | `Widen | `Fix ] [@@deriving compare, hash]
  end

  module Name = struct
    type t =
      | Loc of Cfg.Loc.t
      | Fn of Comp.t
      | Idx of int
      | Prod of t * t
      | Iterate of t * int
    [@@deriving compare]

    let rec hash : t -> int = function
      | Loc l -> Cfg.Loc.hash l
      | Fn fn -> Comp.hash fn
      | Idx i ->
          Hashtbl.hash (Hashtbl.hash i)
          (* doubly hashing to avoid collisions with Loc's *)
      | Prod (n1, n2) -> seeded_hash (hash n1) n2
      | Iterate (n, idx) -> seeded_hash idx n
  end

  module Ref = struct
    type t =
      | Stmt of { mutable stmt : Ast.Stmt.t option; name : Name.t }
      | AState of { mutable state : Dom.t option; name : Name.t }

    let name = function
      | Stmt { stmt = _; name } -> name
      | AState { state = _; name } -> name

    let hash = name >> Name.hash

    let compare r1 r2 = Name.compare (name r1) (name r2)
  end

  module Opaque_ref = struct
    include Regular.Std.Opaque.Make (Ref)

    type t = Ref.t
  end

  let of_cfg (_cfg : Cfg.t) = failwith "todo"

  module G = Graph.Make (Opaque_ref) (Comp)
end
