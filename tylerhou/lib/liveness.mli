module Liveness : sig
  type value =
    | Live
    | Dead
  [@@deriving compare, equal, sexp_of]

  type t =
    { default : value
    ; values : value Var.Map.t
    }
  [@@deriving compare, equal, sexp_of]
end

module Analysis : Dataflow.S with type lattice := Liveness.t

val analyze : Bril.Func.t -> Analysis.t
