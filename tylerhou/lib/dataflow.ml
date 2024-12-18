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

  let of_func (func : Bril.Func.t) =
    { worklist =
        (match Transfer.direction with
         | `Forwards -> func.order
         | `Backwards -> List.rev func.order)
    ; blocks =
        Map.mapi func.blocks ~f:(fun ~key:label ~data:block : Block.t ->
          { header = block.header
          ; body = List.map block.body ~f:(fun ins -> ins, Lattice.bottom)
          ; terminator = block.terminator
          ; lattice_after = Lattice.bottom
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

  let update_one (state : state) : [ `Keep_going of state | `Done of t ] =
    let run_block (label : string) (block : Block.t) : Block.t =
      let block_begin =
        let pred_analyses =
          predecessors state label
          |> List.map ~f:(fun pred -> (Map.find_exn state.blocks pred).lattice_after)
        in
        let init =
          if List.is_empty (predecessors state label)
          then Lattice.init
          else Lattice.bottom
        in
        pred_analyses |> List.fold ~init ~f:Lattice.join
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
