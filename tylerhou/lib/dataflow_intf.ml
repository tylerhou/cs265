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

  module Make (Transfer : Transfer) : sig
    type t

    module Block : sig
      type t =
        { instructions : (Bril.Instr.t * Transfer.Lattice.t) list
            (* An instruction and the lattice before the transfer function has run. *)
        ; block_end : Transfer.Lattice.t
        (* The state after (before) the last (first) instruction in a forward (backward) analysis *)
        }
      [@@deriving compare, equal, sexp_of]
    end

    val of_func : Bril.Func.t -> t
    val update_one : t -> [ `Keep_going of t | `Done of Block.t String.Map.t ]
  end
end
