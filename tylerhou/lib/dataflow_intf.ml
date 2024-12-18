open! Core

module type Lattice = sig
  type t [@@deriving compare, equal, sexp_of]

  val bottom : t
  val init : t
  val join : t -> t -> t
end

module type Transfer = sig
  module Lattice : Lattice

  val transfer : Lattice.t -> point:Program_point.t -> instr:Bril.Instr.body -> Lattice.t
  val direction : [ `Forwards | `Backwards ]
end

module type S = sig
  type lattice
  type t [@@deriving sexp_of]
  type state [@@deriving sexp_of]

  module Block : sig
    type t [@@deriving compare, equal, sexp_of]

    val before : t -> lattice
    val after : t -> lattice
    val instrs : t -> Bril.Instr.body list

    type instr_with_lattice =
      { before : lattice
      ; instr : Bril.Instr.body
      ; point : Program_point.t
      ; after : lattice
      }
    [@@deriving compare, equal, sexp_of]

    val instrs_with_lattice : t -> instr_with_lattice list
  end

  val of_func : Bril.Func.t -> state
  val update_one : state -> [ `Keep_going of state | `Done of t ]
  val run : Bril.Func.t -> Block.t String.Map.t
end

module type Intf = sig
  module type Lattice = Lattice
  module type Transfer = Transfer
  module type S = S

  module Make (Transfer : Transfer) : S with type lattice := Transfer.Lattice.t
end
