open! Core

module Points_to = struct
  type target =
    | Exactly of Var.Set.t
    | All_memory_locations
  [@@deriving compare, equal, sexp_of]

  type t = target Var.Map.t [@@deriving compare, equal, sexp_of]

  let bottom : t = Var.Map.empty
  let init : t = Var.Map.empty

  let join (t1 : t) (t2 : t) : t =
    Map.merge t1 t2 ~f:(fun ~key:_ data ->
      match data with
      | `Right target | `Left target -> Some target
      | `Both (l, r) ->
        if not (equal_target l r) then failwith "program not in ssa form" else Some l)
  ;;
end

module Transfer = struct
  module Lattice = Points_to

  let transfer (points_to : Points_to.t) ~label:_ ~(instr : Bril.Instr.t) : Points_to.t =
    match instr with
    | Unary ((dest, _), Id, arg) ->
      (match Map.find points_to arg with
       | Some existing -> Map.set points_to ~key:dest ~data:existing
       | None -> points_to)
    | PtrAdd ((dest, _), base, _) ->
      (match Map.find points_to base with
       | Some existing -> Map.set points_to ~key:dest ~data:existing
       | _ -> points_to)
    | Alloc ((dest, _), _) ->
      Map.set points_to ~key:dest ~data:(Exactly (Var.Set.singleton dest))
    | Load ((dest, _), _) -> Map.set points_to ~key:dest ~data:All_memory_locations
    | _ -> points_to
  ;;

  let direction = `Forwards
end

module Analysis = Dataflow.Make (Transfer)

let run func =
  let blocks = Analysis.run func in
  eprint_s [%message "points_to" (blocks : Analysis.Block.t String.Map.t)];
  func