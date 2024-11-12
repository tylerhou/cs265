open! Core

module Pointer_contains = struct
  module Value = struct
    type t =
      | Bottom
      | Value of Var.t
      | Multiple_values (* Top *)
    [@@deriving compare, equal, sexp_of]

    let join (left : t) (right : t) : t =
      match left, right with
      | Bottom, v | v, Bottom -> v
      | Value l, Value r -> if Var.equal l r then left else Multiple_values
      | Multiple_values, _ -> Multiple_values
      | _, Multiple_values -> Multiple_values
    ;;
  end

  type t = Value.t Points_to.Location.Map.t [@@deriving compare, equal, sexp_of]

  let join (t1 : t) (t2 : t) =
    Map.merge t1 t2 ~f:(fun ~key:_ elt ->
      Some
        (match elt with
         | `Both (v1, v2) -> Value.join v1 v2
         | `Left v | `Right v -> v))
  ;;

  let bottom = Points_to.Location.Map.empty
  let init = Points_to.Location.Map.empty
end

module Transfer (Points_to_analysis : sig
    val analysis_by_point
      : Points_to.Analysis.Block.instr_with_lattice Program_point.Map.t
  end) =
struct
  module Lattice = Pointer_contains

  let transfer (before : Pointer_contains.t) ~point ~(instr : Bril.Instr.t) =
    match instr with
    | Store (ptr, arg) ->
      let analysis = Map.find_exn Points_to_analysis.analysis_by_point point in
      (match Map.find analysis.before ptr with
       | Some targets ->
         let targets =
           match targets with
           | Exactly targets -> Set.to_list targets
           | All_memory_locations ->
             (* For simplicity, only join memory locations that we have seen
                before. It is sound / more aggressive to join on all memory
                locations, but it's more effort... *)
             Map.keys before
         in
         List.fold targets ~init:before ~f:(fun contains target ->
           Map.update contains target ~f:(fun existing ->
             match existing, target with
             | None, _ -> Value arg
             | Some _, First _ -> Value arg
             | Some existing, Rest _ -> Pointer_contains.Value.join existing (Value arg)))
       | None -> before)
    | Call _ ->
      (* Clobber all memory values. *)
      Pointer_contains.bottom
    | _ -> before
  ;;

  let direction = `Forwards
end

let run func =
  let analysis_by_point = Points_to.analyze func in
  let module Analysis =
    Dataflow.Make (Transfer (struct
      let analysis_by_point = analysis_by_point
    end))
  in
  let analysis = Analysis.run func in
  eprint_s [%message "store_to_load" (analysis : Analysis.Block.t String.Map.t)];
  let optimized_instrs =
    List.concat_map func.order ~f:(fun label ->
      let block = Map.find_exn analysis label in
      let module Block = Analysis.Block in
      let instrs =
        block
        |> Block.to_list
        |> List.map
             ~f:
               (fun
                 ({ before; instr; point; _ } : Block.instr_with_lattice)
                 : Bril.Instr.t
               ->
               match instr with
               | Load (dest, ptr) ->
                 (match
                    let%tydi { before; _ } = Map.find_exn analysis_by_point point in
                    Map.find before ptr
                  with
                  | None -> instr
                  | Some points_to ->
                    let targets =
                      match points_to with
                      | Exactly locations -> Set.to_list locations
                      | All_memory_locations -> Map.keys before
                    in
                    let value =
                      List.fold
                        targets
                        ~init:Pointer_contains.Value.Bottom
                        ~f:(fun value target ->
                          match Map.find before target with
                          | None -> value
                          | Some existing -> Pointer_contains.Value.join value existing)
                    in
                    (match value with
                     | Value v -> Unary (dest, Id, v)
                     | _ -> instr))
               | _ -> instr)
      in
      instrs)
  in
  Bril.Func.set_instrs func optimized_instrs
;;
