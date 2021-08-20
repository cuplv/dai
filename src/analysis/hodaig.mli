open Domain
open Frontend

(** Higher-order DAIG: per-procedure DAIGs interoperating to perform summary-based interprocedural analysis *)
module Make (Dom : Abstract.Dom) : sig
  type t (* internally: Daig.Make(Dom).t Cfg.Fn.Map.t *)

  val apply : diff:Tree_diff.t -> Loc_map.t -> t -> Loc_map.t * t
end
