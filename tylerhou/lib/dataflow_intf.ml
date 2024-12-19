open! Core

module type Lattice = sig
  type value [@@deriving compare, equal, sexp_of]

  val bottom : value (* init is always bottom *)
  val top : value
  val join_value : value -> value -> value
  val boundary : value

  type t =
    { default : value
    ; values : value Var.Map.t
    }
  [@@deriving compare, equal, sexp_of]
end

module type Transfer = sig
  module Lattice : Lattice

  val transfer
    :  Lattice.t
    -> point:Program_point.t
    -> instr:[ `Body of Bril.Instr.body | `Terminator of Bril.Instr.terminator ]
    -> Lattice.t

  val direction : [ `Forwards | `Backwards ]
end

module type S = sig
  type lattice
  type state [@@deriving sexp_of]

  module Block : sig
    type t [@@deriving compare, equal, sexp_of]

    val before : t -> lattice
    val after : t -> lattice
    val header : t -> Bril.Instr.header
    val instrs : t -> Bril.Instr.body list
    val terminator : t -> Bril.Instr.terminator

    type instr_with_lattice =
      { before : lattice
      ; instr : Bril.Instr.body
      ; point : Program_point.t
      ; after : lattice
      }
    [@@deriving compare, equal, sexp_of]

    val instrs_with_lattice : t -> instr_with_lattice list
  end

  type t = Block.t String.Map.t [@@deriving sexp_of]

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
