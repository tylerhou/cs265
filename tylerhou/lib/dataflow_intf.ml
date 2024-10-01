open! Core

module type Lattice = sig
  type t [@@deriving compare, equal, sexp_of]

  val bottom : t
  val join : t -> t -> t
end

module type Transfer = sig
  module Lattice : Lattice

  val transfer : Lattice.t -> Bril.Instr.t -> Lattice.t
  val direction : [ `Forwards | `Backwards ]
end

module type Intf = sig
  module type Lattice = Lattice
  module type Transfer = Transfer

  module Make (Transfer : Transfer) : sig
    type t [@@deriving sexp_of]
    type state [@@deriving sexp_of]

    module Block : sig
      type t [@@deriving compare, equal, sexp_of]

      type instr_with_lattice =
        { before : Transfer.Lattice.t
        ; instr : Bril.Instr.t
        ; after : Transfer.Lattice.t
        }

      val to_list : t -> instr_with_lattice list
    end

    val of_func : Bril.Func.t -> state
    val update_one : state -> [ `Keep_going of state | `Done of t ]
    val run : Bril.Func.t -> Block.t String.Map.t
  end
end
