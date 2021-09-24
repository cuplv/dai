open Dai.Import
open Domain
open Syntax

module type Sig = sig
  type state

  type t = { fn : Cfg.Fn.t; is_exc : bool; entry_state : state } [@@deriving compare, sexp_of]

  val pp : t pp

  val subsumes : t -> t -> bool

  val exit_loc : t -> Cfg.Loc.t

  include Comparator.S with type t := t

  module Set : sig
    type qry := t

    type t = (qry, comparator_witness) Set.t
  end
end

module Make (Dom : Abstract.Dom) : Sig with type state := Dom.t = struct
  module T = struct
    type t = { fn : Cfg.Fn.t; is_exc : bool; entry_state : Dom.t } [@@deriving compare, sexp_of]

    let pp fs { fn; is_exc; entry_state } =
      Format.fprintf fs "QRY[%s(%a) with entry_state %a]"
        (if is_exc then "exc_exit" else "exit")
        Cfg.Fn.pp fn Dom.pp entry_state

    let subsumes new_qry old_qry =
      Cfg.Fn.equal new_qry.fn old_qry.fn
      && Bool.equal new_qry.is_exc old_qry.is_exc
      && Dom.(old_qry.entry_state <= new_qry.entry_state)

    let exit_loc { fn; is_exc; _ } = if is_exc then fn.exc_exit else fn.exit
  end

  module T_comparator = struct
    include Comparator.Make (T)
    include T
  end

  module Set = struct
    include (Set : module type of Set with type ('a, 'cmp) t := ('a, 'cmp) Set.t)

    type t = Set.M(T_comparator).t [@@deriving compare]
  end

  include T_comparator
end
