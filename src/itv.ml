(* Copyright (c) Benno Stein, 2020
 * me@bennostein.org
 * 
 * This source code is derived in part from the Interval domain of
 * Sledge (github.com/facebook/infer ./sledge directory), which is MIT Licensed.
 * As such, this source code is licensed under the same conditions:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.

 *)

open Import
open Apron

type t = Box.t Abstract1.t

let man = lazy (Box.manager_alloc ())

(* Do not eta-reduce!  Will break lazy manager allocation *)
let join l r = Abstract1.join (Lazy.force man) l r

(* Do not eta-reduce!  Will break lazy manager allocation *)
let equal l r = Abstract1.is_eq (Lazy.force man) l r

(* Do not eta-reduce!  Will break lazy manager allocation *)
let is_bot itv = Abstract1.is_bottom (Lazy.force man) itv

(* Do not eta-reduce!  Will break lazy manager allocation *)
let implies l r = Abstract1.is_leq (Lazy.force man) l r

let bindings (itv : t) =
  let itv = Abstract1.minimize_environment (Lazy.force man) itv in
  let box = Abstract1.to_box (Lazy.force man) itv in
  let vars = Environment.vars box.box1_env |> uncurry Array.append in
  Array.zip_exn vars box.interval_array

let pp fs =
  let pp_pair a_pp b_pp fs (a, b) = Format.fprintf fs "@[(%a@,%a)@]" a_pp a b_pp b in
  bindings >> Array.pp "@," (pp_pair Var.print Interval.print) fs

let sexp_of_t (itv : t) =
  let sexps =
    Array.fold (bindings itv) ~init:[] ~f:(fun acc (v, { inf; sup }) ->
        Sexp.List
          [
            Sexp.Atom (Var.to_string v);
            Sexp.Atom (Scalar.to_string inf);
            Sexp.Atom (Scalar.to_string sup);
          ]
        :: acc)
  in
  Sexp.List sexps

let t_of_sexp = function
  | Sexp.List sexps ->
      let constraint_of_sexp = function
        | Sexp.List [ Sexp.Atom v; Sexp.Atom inf; Sexp.Atom sup ] ->
            ( Var.of_string v,
              (Scalar.Float (Float.of_string inf), Scalar.Float (Float.of_string sup)) )
        | _ -> failwith "malformed interval sexp contents"
      in
      let vars, itvs =
        List.fold sexps ~init:([], []) ~f:(fun (v_acc, i_acc) sexp ->
            let v, (inf, sup) = constraint_of_sexp sexp in
            (v :: v_acc, Interval.of_infsup inf sup :: i_acc))
      in
      let vars = Array.of_list vars in
      let itvs = Array.of_list itvs in
      let env = Environment.make [||] vars in
      Abstract1.of_box (Lazy.force man) env vars itvs
  | _ -> failwith "malformed interval sexp"

let init () = Abstract1.top (Lazy.force man) (Environment.make [||] [||])

let widen _l _r = failwith "todo"

let interpret _stmt _itv = failwith "todo"

let sanitize itv = itv

let show itv =
  pp Format.std_formatter itv;
  Format.flush_str_formatter ()

let hash seed itv = seeded_hash seed @@ Abstract1.hash (Lazy.force man) itv

let compare _l _r = failwith "todo"

let hash_fold_t h itv = Ppx_hash_lib.Std.Hash.fold_int h (hash 0 itv)
