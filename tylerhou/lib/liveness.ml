open! Core

module Liveness = struct
  type value =
    | Live
    | Dead
  [@@deriving compare, equal, sexp_of]

  let bottom = Dead
  let top = Live

  let join_value (v1 : value) (v2 : value) =
    match v1, v2 with
    | Dead, Dead -> Dead
    | _ -> Live
  ;;

  let boundary = Dead

  type t =
    { default : value
    ; values : value Var.Map.t
    }
  [@@deriving compare, equal, sexp_of]
end

module Transfer = struct
  module Lattice = Liveness

  let transfer ({ default; values } : Liveness.t) ~point:_ ~(instr : Bril.Instr.body)
    : Liveness.t
    =
    let killed =
      match Bril.Instr.dest (Body instr) with
      | None -> values
      | Some (dest, _) -> Map.remove values dest
    in
    let args = Bril.Instr.args (Body instr) in
    let values : Liveness.value Var.Map.t =
      match args with
      | None -> killed
      | Some args ->
        List.fold args ~init:killed ~f:(fun values arg ->
          Map.set values ~key:arg ~data:Live)
    in
    { default; values }
  ;;

  let direction = `Backwards
end

module Analysis = Dataflow.Make (Transfer)

let analyze func =
  let blocks = Analysis.run func in
  eprint_s [%message "liveness" (blocks : Analysis.Block.t String.Map.t)];
  func
;;
