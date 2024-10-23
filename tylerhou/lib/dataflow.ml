open! Core
include Dataflow_intf

module Make (Transfer : Transfer) = struct
  module Lattice = Transfer.Lattice

  module Block = struct
    type t =
      { instructions : (Bril.Instr.t * Lattice.t) list
          (* An instruction and the state before (after) the instruction executes in a forward (backward) analysis *)
      ; block_end : Lattice.t
      (* The state after (before) the last (first) instruction in a forward (backward) analysis *)
      }
    [@@deriving compare, equal, sexp_of]

    type instr_with_lattice =
      { before : Transfer.Lattice.t
      ; instr : Bril.Instr.t
      ; after : Transfer.Lattice.t
      }

    let before (t : t) =
      t.instructions
      |> (match Transfer.direction with
        | `Forwards -> List.hd_exn
        | `Backwards -> List.last_exn)
      |> snd
    ;;

    let after (t : t) = t.block_end
    let instrs (t : t) = List.map t.instructions ~f:fst

    (* TODO: Rename to "instrs_with_lattice" *)
    let to_list (t : t) : instr_with_lattice list =
      let output, _ =
        List.fold
          (List.rev t.instructions)
          ~init:([], t.block_end)
          ~f:(fun (output, after) (instr, before) ->
            { before; instr; after } :: output, before)
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

  let of_func (func : Bril.Func.t) =
    { worklist =
        (match Transfer.direction with
         | `Forwards -> func.order
         | `Backwards -> List.rev func.order)
    ; blocks =
        Map.map func.blocks ~f:(fun instrs : Block.t ->
          { instructions = List.map instrs ~f:(fun ins -> ins, Lattice.bottom)
          ; block_end = Lattice.bottom
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

  let update_one (state : state) : [ `Keep_going of state | `Done of t ] =
    let run_block (label : string) (block : Block.t) : Block.t =
      let block_begin =
        let pred_analyses =
          predecessors state label
          |> List.map ~f:(fun pred -> (Map.find_exn state.blocks pred).block_end)
        in
        let init =
          if List.is_empty (predecessors state label)
          then Lattice.init
          else Lattice.bottom
        in
        pred_analyses |> List.fold ~init ~f:Lattice.join
      in
      let block_end, instructions =
        fold_instructions
          block.instructions
          ~init:(block_begin, [])
          ~f:(fun (before, instrs) (instr, _) ->
            (* eprint_s [%message "before transfer" (before : Lattice.t)]; *)
            (* eprint_s [%message "    " (instr : Bril.Instr.t)]; *)
            let after = Transfer.transfer before ~label ~instr in
            (* eprint_s [%message "after transfer" (after : Lattice.t)]; *)
            after, (instr, before) :: instrs)
      in
      { instructions; block_end }
    in
    match state.worklist with
    | [] -> `Done state.blocks
    | label :: rest ->
      eprint_s [%message "processing" (label : string)];
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
        then (
          eprint_s [%message "reached fixpoint" (label : string)];
          without_label)
        else (
          let adding = successors state label in
          eprint_s [%message "adding to worklist" (label : string) (adding : string list)];
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
