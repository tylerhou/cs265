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
    let header (t : t) = t.header
    let instrs (t : t) = List.map t.body ~f:fst
    let terminator (t : t) = t.terminator

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
    let preds, succs =
      match Transfer.direction with
      | `Forwards -> func.preds, func.succs
      | `Backwards -> func.succs, func.preds
    in
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
    ; preds
    ; succs
    }
  ;;

  let fold_instructions (block : Block.t) ~before ~f =
    let body = List.mapi block.body ~f:(fun i (body, _lattice) -> i, body) in
    let terminator = List.length block.body, `Terminator block.terminator in
    match Transfer.direction with
    | `Forwards ->
      let analysis_before_terminator, rev_body =
        List.fold body ~init:(before, []) ~f:(fun (before, rev_body) (i, instr) ->
          let after = f before (i, `Body instr) in
          after, (instr, before) :: rev_body)
      in
      let analysis = f analysis_before_terminator terminator in
      let body = List.rev rev_body in
      analysis, body
    | `Backwards ->
      let analysis_before_body = f before terminator in
      let analysis, body =
        List.fold
          (List.rev body)
          ~init:(analysis_before_body, [])
          ~f:(fun (before, body) (i, instr) ->
            let after = f before (i, `Body instr) in
            after, (instr, before) :: body)
      in
      analysis, body
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

  let join_predecessors (state : state) (block : Block.t) : Lattice.t =
    let block_args_of_terminator (terminator : Bril.Instr.terminator)
      : (string * string list) list
      =
      match terminator with
      | Jmp (label, args) -> [ label, args ]
      | Br (_, (l1, args1), (l2, args2)) -> [ l1, args1; l2, args2 ]
      | Ret _ -> []
    in
    let block_params_of_header (Label (_, params_and_type) : Bril.Instr.header)
      : string list
      =
      List.map ~f:fst params_and_type
    in
    let find_lattice_value_or_default (pred_block : Block.t) arg =
      Map.find pred_block.lattice_after.values arg
      |> Option.value ~default:pred_block.lattice_after.default
    in
    let lattices_and_block_argument_values
      : (Lattice.t * (Var.t * Lattice.value) list) list
      =
      match Transfer.direction with
      | `Forwards ->
        (* This is a forwards analysis, so the logical predecessors of a block A
           transfer A lattice values via block arguments.
        *)
        predecessors state block.label
        |> List.map ~f:(fun pred ->
          let pred_block = Map.find_exn state.blocks pred in
          let params = block_params_of_header block.header in
          let lattice_values : (Var.t * Lattice.value) list =
            let edges_to_this_block =
              block_args_of_terminator pred_block.terminator
              |> List.filter_map ~f:(fun (label, args) ->
                eprint_s [%message "filtering" (label : string) (block.label : string)];
                if String.equal label block.label then Some args else None)
            in
            edges_to_this_block
            |> List.concat_map ~f:(List.zip_exn params)
            |> List.map ~f:(fun (param, arg) ->
              param, find_lattice_value_or_default pred_block arg)
          in
          pred_block.lattice_after, lattice_values)
      | `Backwards ->
        (* This is a backwards analysis, so the logical predecessors (physical
           successors) of a block A transfer A lattice values if A jumps to
           that predecessor. *)
        block_args_of_terminator block.terminator
        |> List.map ~f:(fun (pred, args) ->
          let pred_block = Map.find_exn state.blocks pred in
          let lattice_values =
            block_params_of_header pred_block.header
            |> List.map ~f:(find_lattice_value_or_default pred_block)
          in
          pred_block.lattice_after, List.zip_exn args lattice_values)
    in
    let lattices, block_arg_values = List.unzip lattices_and_block_argument_values in
    let block_arg_values = List.concat block_arg_values in
    let init : Lattice.t =
      let default =
        if List.is_empty (predecessors state block.label)
        then Lattice.boundary
        else Lattice.bottom
      in
      { default; values = Var.Map.empty }
    in
    let before_block_args = lattices |> List.fold ~init ~f:lattice_join in
    let block_args =
      List.fold
        block_arg_values
        ~init:Lattice.{ default = Lattice.bottom; values = Var.Map.empty }
        ~f:
          (fun
            ({ default; values } : Lattice.t) ((var, value) : Var.t * Lattice.value) ->
          let before =
            match Map.find values var with
            | None -> default
            | Some existing -> existing
          in
          { default
          ; values = Map.set values ~key:var ~data:(Lattice.join_value before value)
          })
    in
    { default = before_block_args.default
    ; values =
        Map.merge before_block_args.values block_args.values ~f:(fun ~key:_ ->
            function
            | `Both (_, r) -> Some r
            | `Left l -> Some l
            | `Right r -> Some r)
    }
  ;;

  let update_one (state : state) : [ `Keep_going of state | `Done of t ] =
    let run_block (block : Block.t) : Block.t =
      let block_begin : Lattice.t = join_predecessors state block in
      let lattice_after, body =
        fold_instructions block ~before:block_begin ~f:(fun before (i, instr) ->
          (* eprint_s [%message "before transfer" (before : Lattice.t)]; *)
          (* eprint_s [%message "    " (instr : Bril.Instr.body)]; *)
          let after =
            Transfer.transfer
              before
              ~point:{ block = block.label; instruction = i }
              ~instr
          in
          (* eprint_s [%message "after transfer" (after : Lattice.t)]; *)
          after)
      in
      { block with body; lattice_after }
    in
    match state.worklist with
    | [] -> `Done state.blocks
    | label :: rest ->
      (* eprint_s [%message "processing" (label : string)]; *)
      let before = Map.find_exn state.blocks label in
      let after = run_block before in
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
