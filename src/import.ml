let seeded_hash = Hashtbl.seeded_hash

let systime = Sys.time

include Core

type 'a pp = Formatter.t -> 'a -> unit

let ( >> ) f g = Fn.compose g f

let flip = Fn.flip

let ( $> ) x f =
  f x;
  x

let fst3 (x, _, _) = x

let snd3 (_, x, _) = x

let trd3 (_, _, x) = x

let uncurry f (x, y) = f x y

let curry f x y = f (x, y)

let uncurry3 f (x, y, z) = f x y z

let curry3 f x y z = f (x, y, z)

let pair x y = (x, y)

let range i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let rec apply_n_times ~n ~init ~f =
  if n <= 0 then init else apply_n_times ~n:(pred n) ~init:(f init n) ~f

let time ~f ~x fs descr =
  let st = systime () in
  f x $> fun _ -> Format.fprintf fs "%s%.3f\n" descr (1000. *. (systime () -. st))

let abs_of_rel_path rel_path =
  match Sys.getenv "DAI_ROOT" with
  | Some prefix -> prefix ^ rel_path
  | None ->
      failwith
        "environment variable DAI_ROOT is unset; either use `make` or set manually to project root"

(* "com.example.MyClass.Inner" -> ["com" ; "example"] *)
let deserialize_package =
  String.split ~on:'.'
  >> List.filter ~f:(String.length >> ( <> ) 0)
  >> List.take_while ~f:(flip String.get 0 >> Char.is_lowercase)

(* "com.example.MyClass.Inner" -> "MyClass$Inner" *)
let deserialize_class =
  String.split ~on:'.'
  >> List.filter ~f:(String.length >> ( <> ) 0)
  >> List.drop_while ~f:(flip String.get 0 >> Char.is_lowercase)
  >> String.concat ~sep:"$"

module Option = struct
  include Base.Option

  let pp ?(default = "None") pp_elt fs = function
    | Some x -> pp_elt fs x
    | None -> String.pp fs default

  let cons xo xs = match xo with Some x -> x :: xs | None -> xs

  let merge l r f =
    match (l, r) with
    | None, None -> None
    | Some _, None -> l
    | None, Some _ -> r
    | Some x, Some y -> Some (f x y)

  let ( let* ) = ( >>= )

  let ( let+ ) = ( >>| )
end

include Option.Monad_infix

module List = struct
  include Base.List

  let rec pp ?pre ?suf sep pp_elt fs = function
    | [] -> ()
    | x :: xs ->
        Option.iter pre ~f:(Format.fprintf fs);
        pp_elt fs x;
        (match xs with [] -> () | xs -> Format.fprintf fs "%( %)%a" sep (pp sep pp_elt) xs);
        Option.iter suf ~f:(Format.fprintf fs)
end

module Result = struct
  include Base.Result

  let pp fmt pp_elt fs = function Ok x -> Format.fprintf fs fmt pp_elt x | Error _ -> ()
end

module Set = struct
  include Base.Set

  type ('elt, 'cmp) tree = ('elt, 'cmp) Using_comparator.Tree.t

  let equal_m__t (module Elt : Compare_m) = equal

  let pp pp_elt fs x = Format.fprintf fs "{%a}" (List.pp ~pre:"" ~suf:"" ",@ " pp_elt) (to_list x)

  let disjoint x y = is_empty (inter x y)

  let add_option yo x = Option.fold ~f:add ~init:x yo

  let add_list ys x = List.fold ~f:add ~init:x ys

  let to_tree = Using_comparator.to_tree

  let union x y =
    let xy = union x y in
    let xy_tree = to_tree xy in
    if phys_equal xy_tree (to_tree x) then x else if phys_equal xy_tree (to_tree y) then y else xy
end

module Array = struct
  include Base.Array

  let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
end

module Map = struct
  include Base.Map

  let pp pp_key pp_data fs m =
    Format.fprintf fs "{@[<hv 2>";
    iteri m ~f:(fun ~key ~data -> Format.fprintf fs " %a : %a;@," pp_key key pp_data data);
    Format.fprintf fs "@]}"
end

type ('a, 'b) fmt = ('a, Formatter.t, unit, 'b) format4

module Colors = struct
  let red = "\x1b[91m"

  let green = "\x1b[92m"

  let yellow = "\x1b[93m"

  let blue = "\x1b[94m"

  let magenta = "\x1b[95m"

  let cyan = "\x1b[96m"

  let reset = "\x1b[0m"
end

module Engine = Adapton.Engine.Make (Adapton.Engine.Default_params)
module DefaultArtLib = Engine.ArtLib

module Name = struct
  include Adapton.Name

  let extend nm str = pair nm (of_string str)
end

module Graph = Graphlib.Std.Graphlib
module Seq = Regular.Std.Seq

let ( = ) = Stdlib.( = )

let ( < ) = Stdlib.( < )

let ( > ) = Stdlib.( > )

let ( <= ) = Stdlib.( <= )

let ( >= ) = Stdlib.( >= )
