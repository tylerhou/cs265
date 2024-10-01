open! Core

type t = string [@@deriving compare, sexp_of]

include Comparable.S_plain with type t := t
