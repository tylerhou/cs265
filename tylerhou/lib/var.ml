open! Core

module T = struct
  type t = string [@@deriving compare, sexp_of, hash]
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)
