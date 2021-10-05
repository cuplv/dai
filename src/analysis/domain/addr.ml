open Dai
open Import
include Syntax.Alloc_site

module Abstract = struct
  include Set [@@deriving sexp]

  let hash = seeded_hash

  let of_alloc_site = singleton
end
