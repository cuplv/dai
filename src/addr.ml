open Import

module T : sig
  type t [@@deriving compare, equal, hash, sexp]

  include Adapton.Data.S with type t := t

  val pp : t pp

  val of_alloc_site : int * int -> t

  val hash : int -> t -> int
end = struct
  type t = int * int [@@deriving compare, equal, hash, sexp]

  let of_alloc_site i = i

  let pp fs (line, col) = Format.fprintf fs "(%i,%i)" line col

  let show a =
    pp Format.str_formatter a;
    Format.flush_str_formatter ()

  let sanitize a = a

  let hash = seeded_hash
end

include T

module T_comparator : sig
  type t = T.t [@@deriving sexp]

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end = struct
  include T
  include Comparator.Make (T)
end

module Set = struct
  include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

  type t = Set.M(T_comparator).t [@@deriving compare, sexp]

  let empty = Set.empty (module T_comparator)

  let singleton = Set.singleton (module T_comparator)

  let sanitize addrs = addrs

  let pp fs = to_list >> (List.pp ", " ~pre:"{" ~suf:"}" T.pp) fs

  let show x =
    pp Format.str_formatter x;
    Format.flush_str_formatter ()
end

module Map = struct
  include (
    Base.Map :
      module type of Base.Map with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Base.Map.t )

  type 'v t = 'v Base.Map.M(T_comparator).t

  let empty = Base.Map.empty (module T_comparator)
end

module Abstract = struct
  include Set [@@deriving sexp]

  let hash = seeded_hash
end
