open Dai.Import
open Syntax
open Domain

(** Get the callee [f] of a callsite [stmt] of the form `y = f(x_1,...,x_k)`*)
let get_callee_unsafe stmt =
  match stmt with
  | Ast.Stmt.Call { meth; _ } -> meth
  | _ -> failwith "can't get callee of non-call statement"

module type CtxFunctor = functor (Dom : Abstract.Dom) -> sig
  include Abstract.CtxSensitiveDom with type t := Dom.t

  type t = Dom.t
end

module MakeInsensitive (Dom : Abstract.Dom) : sig
  include Abstract.CtxSensitiveDom with type t := Dom.t

  type t = Dom.t
end = struct
  include Dom

  module Ctx = struct
    type dom = t

    type t = unit [@@deriving compare, equal, hash, sexp_of]

    let pp fs () = Format.fprintf fs "()"

    let sanitize () = ()

    let show () = "()"

    let hash = seeded_hash

    let init = ()

    let callee_ctx ~caller_state:_ ~callsite:_ ~ctx:() = ()

    let is_feasible_callchain _ _ = true
  end
end

module Make1CFA (Dom : Abstract.Dom) : sig
  include Abstract.CtxSensitiveDom with type t := Dom.t

  type t = Dom.t
end = struct
  include Dom

  module Ctx = struct
    type dom = t

    type t = Ast.Stmt.t option [@@deriving compare, equal, hash, sexp_of]

    let pp fs = function
      | Some caller -> Format.fprintf fs "[%a]" Ast.Stmt.pp caller
      | None -> Format.fprintf fs "[]"

    let sanitize x = x

    let show = Format.asprintf "%a" pp

    let hash = seeded_hash

    let init = None

    let callee_ctx ~caller_state:_ ~callsite ~ctx:_ = Some callsite

    let is_feasible_callchain ctx chain =
      match (ctx, chain) with
      | None, [] -> true
      | Some ctx_caller, chain_caller :: _ -> Ast.Stmt.equal ctx_caller chain_caller
      | _ -> false
  end
end

module Make2CFA (Dom : Abstract.Dom) : sig
  include Abstract.CtxSensitiveDom with type t := Dom.t

  type t = Dom.t
end = struct
  include Dom

  module Ctx = struct
    type dom = t

    type t = Ast.Stmt.t list [@@deriving compare, equal, hash, sexp_of]

    let pp fs = function
      | [] -> Format.fprintf fs "[]"
      | [ caller ] -> Format.fprintf fs "[%a]" Ast.Stmt.pp caller
      | [ caller; callers_caller ] ->
          Format.fprintf fs "[%a :: %a]" Ast.Stmt.pp caller Ast.Stmt.pp callers_caller
      | _ -> failwith "callstring length capped at 2"

    let sanitize x = x

    let show = Format.asprintf "%a" pp

    let hash = seeded_hash

    let init = []

    let callee_ctx ~caller_state:_ ~callsite ~ctx =
      match ctx with [] -> [ callsite ] | caller :: _ -> [ callsite; caller ]

    let is_feasible_callchain ctx chain =
      match (ctx, chain) with
      | [], [] -> true
      | [ ctx_caller ], [ chain_caller ] -> Ast.Stmt.equal ctx_caller chain_caller
      | [ ctx1; ctx2 ], chain1 :: chain2 :: _ -> Ast.Stmt.(equal ctx1 chain1 && equal ctx2 chain2)
      | _ -> false
  end
end
