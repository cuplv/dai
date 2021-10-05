open Dai.Import

module T = struct
  type t = {
    package : string list;
    class_name : string;
    method_name : string;
    static : bool;
    arg_types : string list;
  }
  [@@deriving compare, equal, hash, sexp_of]
  (** NB:
   * [class_name] is "Foo$Bar" for inner class "Bar" of outer class "Foo"
   * [arg_types] are _unqualified_: always "String", never "java.lang.String"
  *)

  let pp fs { package; class_name; method_name; static; arg_types } =
    Format.fprintf fs "%a.%s%s%s(%a)" (List.pp "." String.pp) package class_name
      (if static then "." else "#")
      method_name (List.pp "," String.pp) arg_types
end

module T_comparator : sig
  type t = T.t [@@deriving compare, equal, hash, sexp_of]

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t

  val pp : t pp
end = struct
  include T
  include Comparator.Make (T)
end

module Set = struct
  include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

  type t = Set.M(T_comparator).t
end

module Map = struct
  include (Map : module type of Map with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) Map.t)

  type 'v t = 'v Map.M(T_comparator).t
end

include T_comparator
