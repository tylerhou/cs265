open! Core

module Dominance_lattice = struct
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

module Dominance_transfer = struct
  module Lattice = Dominance_lattice

  let transfer (lat : Lattice.t) ~label ~instr:_ : Lattice.t =
    match lat with
    | Bottom -> Bottom
    | Some set -> Some (Set.add set label)
  ;;

  let direction = `Forwards
end

module Analyze_dominance = Dataflow.Make (Dominance_transfer)

type value =
  (* var and defining block *)
  | Value of Var.t * string
  | Multiple_values
[@@deriving compare, equal, sexp_of]

module Renamed_by_original (Data : sig
    val arguments : value Var.Map.t
  end) =
struct
  type t = value Var.Map.t [@@deriving compare, equal, sexp_of]

  let bottom : t = Var.Map.empty
  let init : t = Data.arguments

  let join (t1 : t) (t2 : t) : t =
    Map.merge t1 t2 ~f:(fun ~key:_ ->
        function
        | `Left v | `Right v -> Some v
        | `Both (Value (l_var, l_block), Value (r_var, r_block)) ->
          if String.equal l_var r_var && String.equal l_block r_block
          then Some (Value (l_var, l_block))
          else Some Multiple_values
        | _ -> Some Multiple_values)
  ;;
end

module Reaching_defn_transfer (Data : sig
    val arguments : value Var.Map.t
    val original_by_renamed : (Var.t * string) Var.Map.t
  end) =
struct
  module Lattice = Renamed_by_original (Data)

  let transfer (renamed_by_original : Lattice.t) ~label:_ ~instr : Lattice.t =
    match Bril.Instr.dest instr with
    | Some (renamed, _) ->
      let original, defining_block = Map.find_exn Data.original_by_renamed renamed in
      Map.set renamed_by_original ~key:original ~data:(Value (renamed, defining_block))
    | None -> renamed_by_original
  ;;

  let direction = `Forwards
end

let run func =
  (* Our reaching definition calculation is a bit buggy for programs already in
     SSA form since we treat phis as an actual instruction; subsequent phis might
     see reaching definitions from a previous phi in the block rather than
     previous blocks. So assume any function with a Phi node is already in SSA. *)
  if List.exists (Bril.Func.instrs func) ~f:(fun instr ->
       match instr with
       | Phi _ -> true
       | _ -> false)
  then func
  else (
    (* Add an entry block for function arguments. This is so that phi
       instructions for arguments are properly placed when arguments are
       mutated and the entry basic block has incoming control flow. *)
    let instrs = Bril.Instr.Label "_special_entry_for_ssa" :: Bril.Func.instrs func in
    let func = Bril.Func.set_instrs func instrs in
    let blocks = Analyze_dominance.run func in
    eprint_s [%message "dominance" (blocks : Analyze_dominance.Block.t String.Map.t)];
    let dominance : string list String.Map.t =
      Map.fold blocks ~init:String.Map.empty ~f:(fun ~key:subordinate ~data dominance ->
        let dominators =
          match Analyze_dominance.Block.after data with
          | Some dominators -> dominators
          | Bottom -> String.Set.empty
        in
        Set.fold dominators ~init:dominance ~f:(fun dominance dominator ->
          Map.add_multi dominance ~key:dominator ~data:subordinate))
    in
    let dominance_frontier =
      Map.mapi dominance ~f:(fun ~key:block ~data:subordinates ->
        subordinates
        |> List.concat_map ~f:(Map.find_exn func.succs)
        |> String.Set.of_list
        |> Set.filter ~f:(fun sub ->
          String.equal sub block || not (List.mem subordinates sub ~equal:String.equal)))
    in
    eprint_s
      [%message "dominance frontier" (dominance_frontier : String.Set.t String.Map.t)];
    let with_empty_phis =
      let phi_vars_by_block =
        Map.map
          func.blocks
          ~f:(fun (_ : Bril.Instr.t list) : Bril.Bril_type.t Var.Table.t ->
            Var.Table.create ())
      in
      let rec place ~block =
        let instrs = Map.find_exn func.blocks block in
        let frontier =
          match Map.find dominance_frontier block with
          | Some frontier -> frontier
          | None -> String.Set.empty
        in
        let instrs =
          List.filter_map instrs ~f:Bril.Instr.dest
          @ Hashtbl.to_alist (Map.find_exn phi_vars_by_block block)
        in
        List.iter instrs ~f:(fun (dest, dest_type) ->
          Set.iter frontier ~f:(fun frontier_block ->
            let phi_vars = Map.find_exn phi_vars_by_block frontier_block in
            match Hashtbl.add phi_vars ~key:dest ~data:dest_type with
            | `Ok -> place ~block:frontier_block
            | `Duplicate ->
              let existing = Hashtbl.find_exn phi_vars dest in
              if not (Bril.Bril_type.equal existing dest_type)
              then failwith "types do not match!"))
      in
      List.iter func.order ~f:(fun block -> place ~block);
      Map.mapi func.blocks ~f:(fun ~key:block ~data:instrs ->
        match instrs with
        | label :: rest ->
          let phi_vars = Map.find_exn phi_vars_by_block block in
          let phis =
            phi_vars
            |> Hashtbl.to_alist
            |> List.map ~f:(fun (var, var_type) : Bril.Instr.t ->
              Phi ((var, var_type), []))
          in
          label :: (phis @ rest)
        | _ -> failwith "no label!")
    in
    let _, original_by_renamed, with_renamed_dests =
      List.fold
        func.order
        ~init:(Var.Map.empty, Var.Map.empty, String.Map.empty)
        ~f:(fun (counter_by_var, original_by_renamed, instrs_by_block) block ->
          let instrs = Map.find_exn with_empty_phis block in
          let counter_by_var, original_by_renamed, rev_instrs =
            List.fold
              instrs
              ~init:(counter_by_var, original_by_renamed, [])
              ~f:(fun (counter_by_var, original_by_renamed, output) instr ->
                match Bril.Instr.dest instr with
                | Some (dest, dest_type) ->
                  let next_id =
                    match Map.find counter_by_var dest with
                    | Some id -> id
                    | None -> 0
                  in
                  let renamed = dest ^ "." ^ Int.to_string next_id in
                  let counter_by_var =
                    Map.set counter_by_var ~key:dest ~data:(next_id + 1)
                  in
                  let original_by_renamed =
                    Map.set original_by_renamed ~key:renamed ~data:(dest, block)
                  in
                  ( counter_by_var
                  , original_by_renamed
                  , Bril.Instr.set_dest (Some (renamed, dest_type)) instr :: output )
                | None -> counter_by_var, original_by_renamed, instr :: output)
          in
          let instrs_by_block =
            Map.set instrs_by_block ~key:block ~data:(List.rev rev_instrs)
          in
          counter_by_var, original_by_renamed, instrs_by_block)
    in
    let func =
      Bril.Func.set_instrs
        func
        (List.map func.order ~f:(Map.find_exn with_renamed_dests) |> List.concat)
    in
    let module Reaching_defn_transfer =
      Reaching_defn_transfer (struct
        let original_by_renamed = original_by_renamed

        let arguments =
          List.map func.args ~f:(fun (var, _) -> var, Value (var, List.hd_exn func.order))
          |> Var.Map.of_alist_exn
        ;;
      end)
    in
    let module Renamed_by_original = Reaching_defn_transfer.Lattice in
    let module Analyze_renamed_by_original = Dataflow.Make (Reaching_defn_transfer) in
    let blocks = Analyze_renamed_by_original.run func in
    eprint_s [%message "" ~_:(blocks : Analyze_renamed_by_original.Block.t String.Map.t)];
    let instrs_with_renamed_args =
      List.concat_map func.order ~f:(fun label ->
        let block = Map.find_exn blocks label in
        let module Block = Analyze_renamed_by_original.Block in
        let instrs =
          block
          |> Block.to_list
          |> List.map
               ~f:
                 (fun
                   ({ instr; before = renamed_by_original; _ } : Block.instr_with_lattice)
                   : Bril.Instr.t
                 ->
                 match instr with
                 | Phi (((renamed, _) as dest), _) ->
                   let original, _ = Map.find_exn original_by_renamed renamed in
                   let phi_args =
                     let preds = Map.find_exn func.preds label in
                     List.filter_map func.order ~f:(fun pred ->
                       if List.mem ~equal:String.equal preds pred
                       then (
                         let pred_end_renamed_by_original =
                           Map.find_exn blocks pred |> Block.after
                         in
                         let value =
                           match Map.find pred_end_renamed_by_original original with
                           | Some (Value (renamed, _)) -> renamed
                           | Some Multiple_values ->
                             Error.raise_s
                               [%message "multiple reaching values" (original : string)]
                           | None -> "__undefined"
                         in
                         Some (pred, value))
                       else None)
                   in
                   Phi (dest, phi_args)
                 | (Nop | Speculate | Commit | Label _ | Const (_, _) | Jmp _) as instr ->
                   (* Can't call args on these... I wish [Bril.Instr.args] returned an option. *)
                   instr
                 | _ ->
                   (match Bril.Instr.args instr with
                    | [] -> instr
                    | args ->
                      let renamed =
                        List.map args ~f:(fun original ->
                          let renamed =
                            match Map.find renamed_by_original original with
                            | Some (Value (renamed, _)) -> renamed
                            | Some Multiple_values ->
                              Error.raise_s
                                [%message
                                  "multiple reaching values" (instr : Bril.Instr.t)]
                            | None ->
                              Error.raise_s
                                [%message
                                  "no reaching definition"
                                    (original : string)
                                    (renamed_by_original : Renamed_by_original.t)]
                          in
                          renamed)
                      in
                      Bril.Instr.set_args renamed instr))
        in
        instrs)
    in
    Bril.Func.set_instrs func instrs_with_renamed_args)
;;
