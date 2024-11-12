open! Core

module Location = struct
  module T = struct
    type t =
      | First of Var.t
      | Rest of Var.t
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module Points_to = struct
  type target =
    | Exactly of Location.Set.t
    | All_memory_locations
  [@@deriving compare, equal, sexp_of]

  type t = target Var.Map.t [@@deriving compare, equal, sexp_of]

  let bottom : t = Var.Map.empty
  let init : t = Var.Map.empty

  let join (t1 : t) (t2 : t) : t =
    Map.merge t1 t2 ~f:(fun ~key:var data ->
      match data with
      | `Right target | `Left target -> Some target
      | `Both (l, r) ->
        if not (equal_target l r)
        then
          Error.raise_s
            [%message "program not in ssa form" (var : string) (l : target) (r : target)]
        else Some l)
  ;;

  let union (tgt1 : target) (tgt2 : target) : target =
    match tgt1, tgt2 with
    | All_memory_locations, _ | _, All_memory_locations -> All_memory_locations
    | Exactly s1, Exactly s2 -> Exactly (Set.union s1 s2)
  ;;
end

module Transfer = struct
  module Lattice = Points_to

  let transfer (points_to : Points_to.t) ~point:_ ~(instr : Bril.Instr.t) : Points_to.t =
    match instr with
    | Unary ((dest, _), Id, arg) ->
      (match Map.find points_to arg with
       | Some existing -> Map.set points_to ~key:dest ~data:existing
       | None -> points_to)
    | PtrAdd ((dest, _), base, _) ->
      (match Map.find points_to base with
       | Some existing ->
         let target : Points_to.target =
           match existing with
           | Exactly locations ->
             Exactly
               (Location.Set.map locations ~f:(fun loc ->
                  match loc with
                  | First var | Rest var -> Rest var))
           | All_memory_locations -> All_memory_locations
         in
         Map.set points_to ~key:dest ~data:target
       | _ -> points_to)
    | Alloc ((dest, _), _) ->
      Map.set points_to ~key:dest ~data:(Exactly (Location.Set.singleton (First dest)))
    | Load ((dest, _), _) -> Map.set points_to ~key:dest ~data:All_memory_locations
    | Phi ((dest, PtrType _), args) ->
      let targets =
        args
        |> List.map ~f:(fun (_, arg) ->
          match Map.find points_to arg with
          | Some s -> s
          | None -> Exactly Location.Set.empty)
        |> List.fold ~init:(Lattice.Exactly Location.Set.empty) ~f:Points_to.union
      in
      Map.set points_to ~key:dest ~data:targets
    | _ -> points_to
  ;;

  let direction = `Forwards
end

module Analysis = Dataflow.Make (Transfer)

module Analysis_by_program_point = struct
  type t = Analysis.Block.instr_with_lattice Program_point.Map.t
  [@@deriving compare, equal, sexp_of]
end

let analyze func =
  let blocks = Analysis.run func in
  (* TODO: Put this in a reusable location. Likely put the type of blocks
     inside a Dataflow_result module. *)
  let by_point =
    blocks
    |> Map.to_alist
    |> List.concat_map ~f:(fun (block, analysis) ->
      Analysis.Block.to_list analysis
      |> List.mapi ~f:(fun idx v -> Program_point.{ block; instruction = idx }, v))
    |> Program_point.Map.of_alist_exn
  in
  eprint_s
    [%message
      "points_to" (by_point : Analysis.Block.instr_with_lattice Program_point.Map.t)];
  by_point
;;
