open! Core

type t =
  { block : string
  ; instruction : int
  }
[@@deriving compare, equal, sexp]

include Comparable.S with type t := t
