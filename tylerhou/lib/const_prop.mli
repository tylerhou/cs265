open! Core

module Lattice : sig
  module Value : sig
    type t =
      | Uninitialized (* Bottom *)
      | Constant of Bril.Const.t
      | Multiple_values (* Top *)
    [@@deriving compare, equal, sexp_of]

    val join : t -> t -> t
  end

  type t = Value.t Var.Map.t [@@deriving compare, equal, sexp_of]

  val join : t -> t -> t
  val bottom : t
  val init : t
end

val run : Bril.Func.t -> Bril.Func.t
