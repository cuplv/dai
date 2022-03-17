open Dai
open Import
open Syntax
open Null_val

(* We need an environment map from var names to null_val * (abstract.addr option)
 * We also want a heap as a map from abstract.addr * field name: string to null_val * (abstract.addr option) *)

module Env = struct
  type t = (Addr.Abstract.t * Null_val.t) Map.M(String).t

  (* why specify module string? *)
  let empty = Map.empty (module String)

  let pp fs env =
    let pp_binding fs (k, (v, a)) =
      Format.fprintf fs "%s -> (%a, %a)" k Null_val.pp v Addr.Abstract.pp a
    in
    List.pp ~pre:"{" ~suf:"}" ";@ " pp_binding fs (Map.to_alist env)
end

(* type t [@@deriving compare, equal, hash, sexp] *)
type t = Env.t option

let pp fs env =
  match env with
  | Some env -> Format.fprintf fs "%a" Env.pp env
  | None -> Format.print_string "bottom"

let bottom () = None

let top () = Some Env.empty

let init = top

let is_bot = is_some >> not

(* keys s2 subset keys s1 
 * AND s1[k] implies s2[k] for all keys k in keys s2 *)
let implies s1 s2 =
  Map.for_alli s2 ~f:(fun ~key ~data ->
      Map.mem s1 key && match data with v, _addr -> Null_val.implies (Map.find_exn s1 key) v)
(* TODO(archerd): missing any checks on the address (should use subset?)*)

let ( <= ) = implies

let sanitize = Fn.id

let join =
  Map.merge_skewed ~combine:(fun ~key:_ (v1, addr1) (v2, addr2) ->
      (Null_val.join v1 v2, Addr.Abstract.union addr1 addr2))
(* use Map.merge_skewed *)

let widen = join (* TODO(archerd): double check this, but I think it still applies *)

let call = failwith "unimplemented"

let return = failwith "unimplemented"

let approximate_missing_callee = failwith "unimplemented"

let interpret = failwith "unimplemented"
