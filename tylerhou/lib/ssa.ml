open! Core

module Dominance = struct
  (* Set of blocks. Bottom represents "all blocks." *)
  type t =
    | Bottom
    | Some of String.Set.t
  [@@deriving compare, equal, sexp]

  let bottom : t = Bottom
  let init = Some String.Set.empty

  let join (t1 : t) (t2 : t) : t =
    match t1, t2 with
    | Bottom, t | t, Bottom -> t
    | Some s1, Some s2 -> Some (Set.inter s1 s2)
  ;;
end

module Transfer = struct
  module Lattice = Dominance

  let transfer (dominance : Dominance.t) ~label ~instr:_ : Dominance.t =
    match dominance with
    | Bottom -> Bottom
    | Some set -> Some (Set.add set label)
  ;;

  let direction = `Forwards
end

module Analyze_dominance = Dataflow.Make (Transfer)

let run func =
  let blocks = Analyze_dominance.run func in
  let dominance : string list String.Map.t =
    Map.fold blocks ~init:String.Map.empty ~f:(fun ~key:subordinate ~data dominance ->
      match Analyze_dominance.Block.after data with
      | Some dominators ->
        Set.fold dominators ~init:dominance ~f:(fun dominance dominator ->
          Map.add_multi dominance ~key:dominator ~data:subordinate)
      | Bottom -> failwith "dominance is still bottom!")
  in
  let dominance_frontier =
    Map.mapi dominance ~f:(fun ~key:block ~data:subordinates ->
      subordinates
      |> List.concat_map ~f:(Map.find_exn func.succs)
      |> String.Set.of_list
      |> Set.filter ~f:(fun sub ->
        String.equal sub block || not (List.mem subordinates sub ~equal:String.equal)))
  in
  let results =
    let worklist = Array.of_list func.order in
    let phis_by_block =
      Map.map func.blocks ~f:(fun instrs : Var.Hash_set.t -> Var.Hash_set.create ())
    in
    Map.iteri func.blocks ~f:(fun ~key:block ~data:instrs ->
      let frontier = Map.find_exn dominance_frontier block in
      List.iter instrs ~f:(fun instr ->
        match Bril.Instr.dest instr with
        | Some (dest, _) ->
          Set.iter frontier ~f:(fun frontier_block ->
            let phis = Map.find_exn phis_by_block frontier_block in
            Hash_set.add phis dest)
        | None -> ()));

  in
  eprint_s [%message "" ~_:(dominance_frontier : String.Set.t String.Map.t)];
  func
;;
