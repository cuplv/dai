open Import
open Ast

type truthiness = [ `T | `F | `Either ]

module type Domain = sig
  type t [@@derivin1g equal, hash]

  val pp : t pp

  val init : t

  val join : t -> t -> t

  val exec_stmt : t -> Stmt.t -> t option

  val truth_value : t -> Expr.t -> truthiness
end

module type Abstract_value = sig
  type t [@@deriving equal, hash]

  val pp : t pp

  val join : t -> t -> t

  val eval_binop : t -> Binop.t -> t -> t

  val eval_unop : Unop.t -> t -> t

  val truthiness : t -> truthiness
end

module Set_of_concrete : Abstract_value = struct
  type t = Set.M(Lit).t [@@deriving equal, hash]

  let pp = Set.pp Lit.pp

  let join = Set.union

  let eval_binop _ _ _ = failwith "unimplemented"

  let eval_unop _ _ = failwith "unimplemented"

  let truthiness _vals = failwith "unimplemented"
end

module Env (Val : Abstract_value) : Domain = struct
  type t = Val.t Map.M(String).t [@@deriving equal, hash]

  let pp fs env =
    let pp_string_color color str =
      Format.fprintf fs "%s%s%s" color str Colors.reset
    in
    Format.open_hovbox 0;
    pp_string_color Colors.cyan "{";
    Map.iteri env ~f:(fun ~key ~data ->
        Format.fprintf fs "%s %s->%s %a %s;%s@ " key Colors.blue Colors.reset
          Val.pp data Colors.red Colors.reset);
    pp_string_color Colors.cyan "}";
    Format.close_box ()

  let init = Map.empty (module String)

  let join =
    Map.fold2 ~init ~f:(fun ~key ~data acc ->
        match data with
        | `Both (x, y) -> Map.add_exn acc ~key ~data:(Val.join x y)
        | `Left x -> Map.add_exn acc ~key ~data:x
        | `Right x -> Map.add_exn acc ~key ~data:x)

  let eval_expr _state _expr = failwith "unimplemented"

  let truth_value _env _expr = failwith ""

  let rec exec_stmt st stmt =
    let open Stmt in
    match stmt with
    | Seq (l, r) -> exec_stmt st l >>= flip exec_stmt r
    | If { cond; then_body; else_body } -> (
        match truth_value st cond with
        | `T -> exec_stmt st then_body
        | `F -> exec_stmt st else_body
        | `Either ->
            let then_res = exec_stmt st then_body in
            let else_res = exec_stmt st else_body in
            Option.merge then_res else_res join )
    | Assign { lhs; rhs } ->
        let rhs = eval_expr st rhs in
        Map.change st lhs ~f:(fun _ -> rhs) |> Option.some
    | Throw { exn = _ } -> None
    | Expr _ | Skip -> Some st
end

module Make (Dom : Domain) = struct end
