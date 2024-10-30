open! Core

module T = struct
  type t =
    { block : string
    ; instruction : int
    }
  [@@deriving compare, equal, sexp]
end

include T
include Comparable.Make (T)
