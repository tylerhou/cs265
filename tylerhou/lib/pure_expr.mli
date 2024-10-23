open! Core

type t =
  | Const of Bril.Bril_type.t * Bril.Const.t
  | Unary of Bril.Bril_type.t * Bril.Op.Unary.t * Var.t
  | Binary of Bril.Bril_type.t * Var.t * Bril.Op.Binary.t * Var.t
[@@deriving compare, equal, sexp_of]

include Comparable.S_plain with type t := t

val of_instr : Bril.Instr.t -> t option
val to_instr : Var.t -> t -> Bril.Instr.t
