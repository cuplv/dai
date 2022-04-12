module G : module type of Analysis.Dsg.Make (Domain.Array_bounds)

val init : unit -> G.t

val random_edit : G.t -> G.t

val random_query : G.t -> G.t

val exit_query : G.t -> G.t
