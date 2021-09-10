open Dai
open Import
include Syntax.Alloc_site

module Abstract = struct
  include Set [@@deriving sexp]

  let hash = seeded_hash
end
