open! Core

type t = string [@@deriving compare, sexp_of, hash]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
