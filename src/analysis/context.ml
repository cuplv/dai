open Dai.Import
open Syntax

(** Get the callee [f] of a callsite [stmt] of the form `y = f(x_1,...,x_k)`*)
let get_callee_unsafe stmt =
  match stmt with
  | Ast.Stmt.Call { meth; _ } -> meth
  | _ -> failwith "can't get callee of non-call statement"

module type Sig = sig
  type t [@@deriving compare, equal, sexp_of]

  type ctx = t

  val pp : t pp

  val init : unit -> t

  val callee_ctx : callsite:Ast.Stmt.t -> caller_ctx:t -> t

  include Comparator.S with type t := t

  module Map : sig
    type 'v t = (ctx, 'v, comparator_witness) Map.t

    val empty : 'v t

    val singleton : ctx -> 'v -> 'v t
  end
end

module ZeroCFA : Sig = struct
  module T = struct
    type t = unit [@@deriving compare, equal, sexp_of]

    type ctx = t

    let init () = ()

    let pp fs () = Format.fprintf fs "()"

    let callee_ctx ~callsite:_ ~caller_ctx:_ = ()
  end

  module T_comp = struct
    include Comparator.Make (T)
    include T
  end

  include T_comp

  module Map = struct
    include (Map : module type of Map with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) Map.t)

    type 'v t = 'v Map.M(T_comp).t

    let empty = Map.empty (module T_comp)

    let singleton k v = Map.of_alist_exn (module T_comp) [ (k, v) ]
  end
end

module OneCFA : Sig = struct
  module T = struct
    type t = Ast.Stmt.t option [@@deriving compare, equal, sexp_of]

    type ctx = t

    let init () = None

    let pp fs = function
      | Some caller -> Format.fprintf fs "[%a]" Ast.Stmt.pp caller
      | None -> Format.fprintf fs "[]"

    let callee_ctx ~callsite ~caller_ctx:_ = Some callsite
  end

  module T_comp = struct
    include Comparator.Make (T)
    include T
  end

  include T_comp

  module Map = struct
    include (Map : module type of Map with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) Map.t)

    type 'v t = 'v Map.M(T_comp).t

    let empty = Map.empty (module T_comp)

    let singleton k v = Map.of_alist_exn (module T_comp) [ (k, v) ]
  end
end
