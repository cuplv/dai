open Dai.Import

module type Sig = sig
  type state

  type t = { pre : state; post : state } [@@deriving compare, sexp_of]

  val implies : t -> t -> bool

  include Comparator.S with type t := t

  module Set : sig
    type rel := t

    type t = (rel, comparator_witness) Set.t

    val empty : t

    val singleton : rel -> t

    val of_list : rel list -> t
  end
end

(** binary relation domain functor over state domains -- to be interpreted as Hoare triples *)
module Make (State : Abstract.Dom) : Sig with type state := State.t = struct
  module T = struct
    type t = { pre : State.t; post : State.t } [@@deriving compare, sexp_of]

    (** Standard Hoare-logic rule of consequence *)
    let implies l r = State.implies r.pre l.pre && State.implies l.post r.post
  end

  module T_comparator = struct
    include Comparator.Make (T)
    include T
  end

  module Set = struct
    include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

    type t = Set.M(T_comparator).t [@@deriving compare]

    let empty = Set.empty (module T_comparator)

    let singleton = Set.singleton (module T_comparator)

    let of_list = Set.of_list (module T_comparator)
  end

  include T_comparator
end
