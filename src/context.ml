open Import

(** Get the callee [f] of a callsite [stmt] of the form `y = f(x_1,...,x_k)`*)
let get_callee_unsafe stmt =
  match stmt with
  | Ast.Stmt.Call { fn; _ } -> fn
  | _ -> failwith "can't get callee of non-call statement"

module MakeInsensitive (Dom : Abstract.DomNoCtx) : sig
  include Abstract.Dom with type t := Dom.t

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

module Make1CFA (Dom : Abstract.DomNoCtx) : Abstract.Dom = struct
  include Dom

  module Ctx = struct
    type dom = t

    type t = Ast.Stmt.t option [@@deriving compare, equal, hash, sexp_of]

    let pp fs = function
      | Some caller -> Format.fprintf fs "[%a]" Ast.Stmt.pp caller
      | None -> Format.fprintf fs "[]"

    let sanitize x = x

    let show x =
      pp Format.str_formatter x;
      Format.flush_str_formatter ()

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
