open Dai.Import

module T : sig
  type t [@@deriving equal, compare, hash, sexp]

  include Adapton.Data.S with type t := t

  val hash : int -> t -> int

  val fresh : unit -> t

  val reset : unit -> unit

  val sanitize : t -> t

  val pp : t pp

  val of_int : int -> t

  val of_varargs : Method_id.t -> t
end = struct
  type t = int [@@deriving equal, compare, hash, sexp]

  let next = ref 0

  let hash = seeded_hash

  let fresh () =
    let curr = !next in
    next := curr + 1;
    curr

  let pp fs a = Format.fprintf fs "alloc_%i" a

  let reset () = next := 0

  let sanitize x = x

  let show x =
    Format.(
      pp str_formatter x;
      flush_str_formatter ())

  let of_int = Fn.id

  let varargs_alloc_sites : t Method_id.Map.t ref = ref Method_id.Map.empty

  let of_varargs method_ =
    match Map.find !varargs_alloc_sites method_ with
    | Some alloc_site -> alloc_site
    | None ->
        let alloc_site = fresh () in
        varargs_alloc_sites := Map.add_exn !varargs_alloc_sites ~key:method_ ~data:alloc_site;
        alloc_site
end

module T_comparator = struct
  include T
  include Comparator.Make (T)
end

include T_comparator

module Set = struct
  include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

  type t = Set.M(T_comparator).t [@@deriving compare, equal, hash, sexp]

  let empty = Set.empty (module T_comparator)

  let singleton = Set.singleton (module T_comparator)

  let sanitize xs = xs

  let pp fs = to_list >> (List.pp ", " ~pre:"{" ~suf:"}" T.pp) fs

  let show x =
    pp Format.str_formatter x;
    Format.flush_str_formatter ()
end

module Map = struct
  include (
    Base.Map :
      module type of Base.Map with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Base.Map.t)

  type 'v t = 'v Base.Map.M(T_comparator).t

  let empty = Base.Map.empty (module T_comparator)
end
