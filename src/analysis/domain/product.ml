open Dai
open Import

module Make (L : Abstract.Dom) (R : Abstract.Dom) : Abstract.Dom = struct
  type t = L.t * R.t [@@deriving compare, equal, hash, sexp]

  let pp fs (l, r) = Format.fprintf fs "(%a, %a)" L.pp l R.pp r

  let top () = (L.top (), R.top ())

  let bottom () = (L.bottom (), R.bottom ())

  let init () = (L.init (), R.init ())

  let sanitize (l, r) = (L.sanitize l, R.sanitize r)

  let show x = Format.asprintf "%a" pp x

  let hash seed (l, r) = seeded_hash (seeded_hash seed l) r

  let hash_fold_t h x = Ppx_hash_lib.Std.Hash.fold_int h (hash 31 x)

  let interpret stmt (l, r) =
    let l = L.interpret stmt l in
    let r = R.interpret stmt r in
    (l, r)

  let implies (l1, r1) (l2, r2) = L.implies l1 l2 && R.implies r1 r2

  let ( <= ) = implies

  let join (l1, r1) (l2, r2) = (L.join l1 l2, R.join r1 r2)

  let widen (l1, r1) (l2, r2) = (L.widen l1 l2, R.widen r1 r2)

  let is_bot (l, r) = L.is_bot l || R.is_bot r

  let call ~callee ~callsite ~caller_state:(l, r) ~fields =
    ( L.call ~callee ~callsite ~caller_state:l ~fields,
      R.call ~callee ~callsite ~caller_state:r ~fields )

  let return ~callee ~caller ~callsite ~caller_state ~return_state ~fields =
    ( L.return ~callee ~caller ~callsite ~caller_state:(fst caller_state)
        ~return_state:(fst return_state) ~fields,
      R.return ~callee ~caller ~callsite ~caller_state:(snd caller_state)
        ~return_state:(snd return_state) ~fields )

  let approximate_missing_callee ~caller_state:(l, r) ~callsite =
    ( L.approximate_missing_callee ~caller_state:l ~callsite,
      R.approximate_missing_callee ~caller_state:r ~callsite )
end
