include Core

let ( >> ) f g x = g (f x)

let flip f x y = f y x

let ( $> ) x f =
  f x;
  x

module Option = struct
  include Base.Option

  let pp fmt pp_elt fs = function
    | Some x -> Format.fprintf fs fmt pp_elt x
    | None -> ()

  let cons xo xs = match xo with Some x -> x :: xs | None -> xs

  let merge l r f =
    match (l, r) with
    | None, None -> None
    | Some _, None -> l
    | None, Some _ -> r
    | Some x, Some y -> Some (f x y)
end

include Option.Monad_infix

module List = struct
  include Base.List

  let rec pp ?pre ?suf sep pp_elt fs = function
    | [] -> ()
    | x :: xs ->
        Option.iter pre ~f:(Format.fprintf fs);
        pp_elt fs x;
        ( match xs with
        | [] -> ()
        | xs -> Format.fprintf fs "%( %)%a" sep (pp sep pp_elt) xs );
        Option.iter suf ~f:(Format.fprintf fs)
end

module Result = struct
  include Base.Result

  let pp fmt pp_elt fs = function
    | Ok x -> Format.fprintf fs fmt pp_elt x
    | Error _ -> ()
end

module Set = struct
  include Base.Set

  type ('elt, 'cmp) tree = ('elt, 'cmp) Using_comparator.Tree.t

  let equal_m__t (module Elt : Compare_m) = equal

  let pp pp_elt fs x = List.pp ~pre:"{" ~suf:"}" ",@ " pp_elt fs (to_list x)

  let disjoint x y = is_empty (inter x y)

  let add_option yo x = Option.fold ~f:add ~init:x yo

  let add_list ys x = List.fold ~f:add ~init:x ys

  let to_tree = Using_comparator.to_tree

  let union x y =
    let xy = union x y in
    let xy_tree = to_tree xy in
    if phys_equal xy_tree (to_tree x) then x
    else if phys_equal xy_tree (to_tree y) then y
    else xy
end

module Array = struct
  include Base.Array

  let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
end

type 'a pp = Formatter.t -> 'a -> unit

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
