open! Core
include Dataflow_intf

module Make (Transfer : Transfer) = struct
  module Lattice = Transfer.Lattice

  module Block = struct
    type t =
      { header : Bril.Instr.header
      ; body : (Bril.Instr.body * Lattice.t) list
      ; terminator : Bril.Instr.terminator
          (* An instruction and the state before (after) the instruction executes in a forward (backward) analysis *)
      ; lattice_after : Lattice.t
          (* The state after (before) the last (first) instruction in a forward (backward) analysis *)
      ; label : string
      }
    [@@deriving compare, equal, sexp_of]

    type instr_with_lattice =
      { before : Transfer.Lattice.t
      ; instr : Bril.Instr.body
      ; point : Program_point.t
      ; after : Transfer.Lattice.t
      }
    [@@deriving compare, equal, sexp_of]

    let before (t : t) =
      t.body
      |> (match Transfer.direction with
        | `Forwards -> List.hd_exn
        | `Backwards -> List.last_exn)
      |> snd
    ;;

    let after (t : t) = t.lattice_after
    let instrs (t : t) = List.map t.body ~f:fst

    let instrs_with_lattice (t : t) : instr_with_lattice list =
      let output, _ =
        t.body
        |> List.mapi ~f:Tuple2.create
        |> List.rev
        |> List.fold
             ~init:([], t.lattice_after)
             ~f:(fun (output, after) (i, (instr, before)) ->
               ( { before; instr; point = { block = t.label; instruction = i }; after }
                 :: output
               , before ))
      in
      output
    ;;
  end

  type state =
    { worklist : string list
    ; blocks : Block.t String.Map.t
    ; preds : string list String.Map.t
    ; succs : string list String.Map.t
    }
  [@@deriving sexp_of]

  type t = Block.t String.Map.t [@@deriving sexp_of]

  let initial_lattice : Lattice.t = { default = Lattice.bottom; values = Var.Map.empty }

  let of_func (func : Bril.Func.t) =
    { worklist =
        (match Transfer.direction with
         | `Forwards -> func.order
         | `Backwards -> List.rev func.order)
    ; blocks =
        Map.mapi func.blocks ~f:(fun ~key:label ~data:block : Block.t ->
          { header = block.header
          ; body = List.map block.body ~f:(fun ins -> ins, initial_lattice)
          ; terminator = block.terminator
          ; lattice_after = initial_lattice
          ; label
          })
    ; preds =
        (match Transfer.direction with
         | `Forwards -> func.preds
         | `Backwards -> func.succs)
    ; succs =
        (match Transfer.direction with
         | `Forwards -> func.succs
         | `Backwards -> func.preds)
    }
  ;;

  let fold_instructions instrs ~init ~f =
    let instrs = List.mapi instrs ~f:(fun i x -> i, x) in
    let before, after =
      match Transfer.direction with
      | `Forwards -> Fn.id, List.rev
      | `Backwards -> List.rev, Fn.id
    in
    let analysis, instrs = List.fold (before instrs) ~init ~f in
    analysis, after instrs
  ;;

  let predecessors (state : state) (label : string) = Map.find_exn state.preds label
  let successors (state : state) (label : string) = Map.find_exn state.succs label

  let lattice_join
    ({ default = d1; values = vs1 } : Lattice.t)
    ({ default = d2; values = vs2 } : Lattice.t)
    : Lattice.t
    =
    let vs =
      Map.merge vs1 vs2 ~f:(fun ~key:_ elt ->
        Some
          (match elt with
           | `Both (l, r) -> Lattice.join_value l r
           | `Left l -> l
           | `Right r -> r))
    in
    { default = Lattice.join_value d1 d2; values = vs }
  ;;

  let update_one (state : state) : [ `Keep_going of state | `Done of t ] =
    let run_block (label : string) (block : Block.t) : Block.t =
      let block_begin : Lattice.t =
        let pred_lattices, pred_block_arg_lattice_values =
          predecessors state label
          |> List.map ~f:(fun pred ->
            let pred_block = Map.find_exn state.blocks pred in
            let block_arg_list : Var.t list list =
              match pred_block.terminator with
              | Jmp (_, args) -> [ args ]
              | Br (_, (l1, args1), (l2, args2)) ->
                (if String.equal pred l1 then [ args1 ] else [])
                @ if String.equal pred l2 then [ args2 ] else []
              | Ret _ -> []
            in
            let lattice_values =
              List.map
                block_arg_list
                ~f:
                  (List.map ~f:(fun arg ->
                     Map.find pred_block.lattice_after.values arg
                     |> Option.value ~default:pred_block.lattice_after.default))
            in
            pred_block.lattice_after, lattice_values)
          |> List.unzip
        in
        let init : Lattice.t =
          let default =
            if List.is_empty (predecessors state label)
            then Lattice.boundary
            else Lattice.bottom
          in
          { default; values = Var.Map.empty }
        in
        let before_block_args = pred_lattices |> List.fold ~init ~f:lattice_join in
        let block_args =
          let lattice_value_per_arg =
            pred_block_arg_lattice_values
            |> List.concat
            |> List.transpose_exn
            |> List.map ~f:(List.fold ~init:Lattice.bottom ~f:Lattice.join_value)
          in
          let (Label (_, arguments)) = block.header in
          let arguments, _ = List.unzip arguments in
          List.zip_exn arguments lattice_value_per_arg |> Var.Map.of_alist_exn
        in
        { default = before_block_args.default
        ; values =
            Map.merge before_block_args.values block_args ~f:(fun ~key:_ elt ->
              Some
                (match elt with
                 | `Both (_l, r) -> r
                 | `Left l -> l
                 | `Right r -> r))
        }
      in
      let lattice_after, body =
        fold_instructions
          block.body
          ~init:(block_begin, [])
          ~f:(fun (before, instrs) (i, (instr, _block_end)) ->
            (* eprint_s [%message "before transfer" (before : Lattice.t)]; *)
            (* eprint_s [%message "    " (instr : Bril.Instr.body)]; *)
            let after =
              Transfer.transfer before ~point:{ block = label; instruction = i } ~instr
            in
            (* eprint_s [%message "after transfer" (after : Lattice.t)]; *)
            after, (instr, before) :: instrs)
      in
      { header = block.header; body; terminator = block.terminator; lattice_after; label }
    in
    match state.worklist with
    | [] -> `Done state.blocks
    | label :: rest ->
      (* eprint_s [%message "processing" (label : string)]; *)
      let before = Map.find_exn state.blocks label in
      let after = run_block label before in
      let blocks = Map.set state.blocks ~key:label ~data:after in
      let worklist =
        (* We just ran the analysis on label, remove instances of it from the
           rest of the list. Make sure to do this before we append, to avoid
           self-cycles. *)
        let without_label =
          List.filter rest ~f:(fun other -> not (String.equal label other))
        in
        if Block.equal before after
        then (* eprint_s [%message "reached fixpoint" (label : string)]; *)
          without_label
        else (
          let adding = successors state label in
          (* eprint_s [%message "adding to worklist" (label : string) (adding : string list)]; *)
          without_label @ adding)
      in
      `Keep_going { state with worklist; blocks }
  ;;

  let run (func : Bril.Func.t) =
    let rec loop (state : state) =
      match update_one state with
      | `Done state -> state
      | `Keep_going state -> loop state
    in
    loop (of_func func)
  ;;
end
