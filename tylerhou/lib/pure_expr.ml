open! Core

module T = struct
  type t =
    | Const of Bril.Bril_type.t * Bril.Const.t
    | Unary of Bril.Bril_type.t * Bril.Op.Unary.t * Var.t
    | Binary of Bril.Bril_type.t * Var.t * Bril.Op.Binary.t * Var.t
  [@@deriving compare, equal, sexp_of]
end

include T
include Comparable.Make_plain (T)

let of_instr (instr : Bril.Instr.t) : t option =
  match instr with
  | Const ((_, t), c) -> Some (Const (t, c))
  | Unary ((_, t), op, arg) -> Some (Unary (t, op, arg))
  | Binary ((_, t), op, l, r) -> Some (Binary (t, l, op, r))
  | _ -> None
;;

let to_instr (var : Var.t) (t : t) : Bril.Instr.t =
  match t with
  | Const (t, c) -> Const ((var, t), c)
  | Unary (t, op, arg) -> Unary ((var, t), op, arg)
  | Binary (t, l, op, r) -> Binary ((var, t), op, l, r)
;;
