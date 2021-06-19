open Dai.Import

type t = {
  prev_start_line : int;
  prev_length : int;
  next_start_line : int;
  new_lines : string array
}
(** A single contiguous edit, removing [prev_size] lines at line [prev_start_line] and adding [new_lines] in their place. *)

val btwn : prev:string array -> next:string array -> t list
(** Compute the text diff between [prev] and [next], at the granularity of lines *)

val pp : t pp
