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

let of_instr (instr : Bril.Instr.t) : (Var.t * Bril.Bril_type.t * t) option =
  match instr with
  | Const ((dest, t), c) -> Some (dest, t, Const (t, c))
  | Unary ((dest, t), op, arg) -> Some (dest, t, Unary (t, op, arg))
  | Binary ((dest, t), op, l, r) -> Some (dest, t, Binary (t, l, op, r))
  | _ -> None
;;

let to_instr (var : Var.t) (t : t) : Bril.Instr.t =
  match t with
  | Const (t, c) -> Const ((var, t), c)
  | Unary (t, op, arg) -> Unary ((var, t), op, arg)
  | Binary (t, l, op, r) -> Binary ((var, t), op, l, r)
;;
