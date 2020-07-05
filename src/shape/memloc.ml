open D1a.Import

module T : sig
  type t [@@deriving equal, compare, hash, sexp]

  val fresh : unit -> t

  val pp : t pp

  val null : t

  val of_int : int -> t

  val to_string : t -> string
end = struct
  type t = int [@@deriving equal, compare, hash, sexp]

  let next = ref 0

  let fresh () =
    let curr = !next in
    next := curr + 1;
    curr

  let null = -1

  let of_int i =
    next := Int.max !next (succ i);
    i

  let pp fs a = if equal a null then Format.fprintf fs "null" else Format.fprintf fs "a%i" a

  let to_string a =
    Format.fprintf Format.str_formatter "%a" pp a;
    Format.flush_str_formatter ()
end

include T

module T_comp : sig
  type t = T.t

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end = struct
  include T
  include Comparable.Make (T)
end

module Map = struct
  include (Map : module type of Map with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Map.t)

  type 'v t = 'v Map.M(T_comp).t

  let empty = Map.empty (module T_comp)
end

module Set = struct
  include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

  type t = Set.M(T_comp).t [@@deriving compare]

  let empty = Set.empty (module T_comp)

  let singleton = Set.singleton (module T_comp)
end

module Labelled_pair = struct
  type t = T.t * T.t * string [@@deriving equal, compare, hash, sexp]

  let pp = pp_triple T.pp T.pp String.pp
end
