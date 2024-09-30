open! Core
include Dataflow_intf

module Make (Transfer : Transfer) = struct
  module Lattice = Transfer.Lattice

  module Block = struct
    type t =
      { block_begin : Lattice.t
          (* The state before (after) the first (last) instruction in a forward (backward) analysis *)
      ; instructions : (Bril.Instr.t * Lattice.t) list
          (* An instruction and the state before (after) the instruction executes in a forward (backward) analysis *)
      ; block_end : Lattice.t
      (* The state after (before) the last (first) instruction in a forward (backward) analysis *)
      }
    [@@deriving compare, equal, sexp_of]
  end

  type t =
    { worklist : string list
    ; blocks : Block.t String.Map.t
    ; preds : string list String.Map.t
    ; succs : string list String.Map.t
    }
  [@@deriving sexp_of]

  let of_func (func : Bril.Func.t) =
    { worklist =
        (match Transfer.direction with
         | `Forwards -> func.order
         | `Backwards -> List.rev func.order)
    ; blocks =
        Map.map func.blocks ~f:(fun instrs : Block.t ->
          { block_begin = Lattice.bottom
          ; instructions = List.map instrs ~f:(fun ins -> ins, Lattice.bottom)
          ; block_end = Lattice.bottom
          })
    ; preds =
        (match Transfer.direction with
         | `Forwards -> func.preds
         | `Backwards -> func.succs)
    ; succs =
        (match Transfer.direction with
         | `Forwards -> func.preds
         | `Backwards -> func.succs)
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

  let predecessors (t : t) (label : string) = Map.find_exn t.preds label
  let successors (t : t) (label : string) = Map.find_exn t.succs label

  let update_one (t : t) : [ `Keep_going of t | `Done of Block.t String.Map.t ] =
    let run_block (label : string) (block : Block.t) : Block.t =
      let block_begin =
        let pred_analyses =
          predecessors t label
          |> List.map ~f:(fun pred -> (Map.find_exn t.blocks pred).block_end)
        in
        pred_analyses |> List.fold ~init:Lattice.bottom ~f:Lattice.join
      in
      let block_end, instructions =
        fold_instructions
          block.instructions
          ~init:(block_begin, [])
          ~f:(fun (analysis, instrs) (instr, _) ->
            (* eprint_s [%message "before transfer" (analysis : Lattice.t)]; *)
            (* eprint_s [%message "    " (instr : Bril.Instr.t)]; *)
            let analysis = Transfer.transfer analysis instr in
            (* eprint_s [%message "after transfer" (analysis : Lattice.t)]; *)
            analysis, (instr, analysis) :: instrs)
      in
      { block_begin; instructions; block_end }
    in
    match t.worklist with
    | [] -> `Done t.blocks
    | label :: rest ->
      let block = Map.find_exn t.blocks label in
      let updated = run_block label block in
      let blocks = Map.set t.blocks ~key:label ~data:updated in
      let worklist =
        if Block.equal block updated then rest else rest @ successors t label
      in
      `Keep_going { t with worklist; blocks }
  ;;
end
