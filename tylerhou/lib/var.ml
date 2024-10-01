open! Core

module T = struct
  type t = string [@@deriving compare, sexp_of]
end

include T
include Comparable.Make_plain (T)
