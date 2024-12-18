open! Core
open! Common

type label = string [@@deriving compare, equal, sexp_of]
type arg = string [@@deriving compare, equal, sexp_of]

type header = Label of label * Dest.t list [@@deriving compare, equal, sexp_of]

type body =
  | Const of Dest.t * Const.t
  | Binary of Dest.t * Op.Binary.t * arg * arg
  | Unary of Dest.t * Op.Unary.t * arg
  | Call of Dest.t option * string * arg list
  | Print of arg list
  | Nop
  | Phi of Dest.t * (label * arg) list
  | Speculate
  | Commit
  | Guard of arg * (label * arg list)
  | Alloc of (Dest.t * arg)
  | Free of arg
  | Store of (arg * arg)
  | Load of (Dest.t * arg)
  | PtrAdd of (Dest.t * arg * arg)
[@@deriving compare, equal, sexp_of]

type terminator =
  | Jmp of label * arg list
  | Br of arg * (label * arg list) * (label * arg list)
  | Ret of arg option
[@@deriving compare, equal, sexp_of]

type t =
  | Header of header
  | Body of body
  | Terminator of terminator
[@@deriving compare, equal, sexp_of]

val dest : t -> Dest.t option
val set_dest : Dest.t option -> body -> body
val args : t -> arg list option
val set_args : arg list -> t -> t
val of_json : Yojson.Basic.t -> t
val to_json : t -> Yojson.Basic.t
val to_string : t -> string
