open! Core

module Var = struct
  module T = struct
    type t = string [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module Live_vars = struct
  type t = Var.Set.t [@@deriving compare, equal, sexp_of]

  let join (t1 : t) (t2 : t) : t = Set.union t1 t2

  let is_root (instr : Bril.Instr.t) =
    match instr with
    | Label _ -> true
    | Const _ -> false
    | Binary _ -> false
    | Unary _ -> false
    | Jmp _ -> true
    | Br _ -> true
    | Call _ -> true
    | Ret _ -> true
    | Print _ -> true
    | Nop -> false
    | Phi _ -> false
    | Speculate -> true
    | Commit -> true
    | Guard _ -> true
    | Alloc _ -> true
    | Free _ -> true
    | Store _ -> true
    | Load _ -> false
    | PtrAdd _ -> false
  ;;

  let transfer (t : t) (instr : Bril.Instr.t) : t =
    let dest = Bril.Instr.dest instr in
    let will_args_be_used =
      let will_dest_be_used =
        match dest with
        | None -> false
        | Some (dest, _) -> Set.mem t dest
      in
      will_dest_be_used || is_root instr
    in
    let after_kill =
      match dest with
      | None -> t
      | Some (dest, _) -> Set.remove t dest
    in
    if will_args_be_used
    then
      List.fold
        ~init:after_kill
        ~f:Set.add
        (match instr with
         (* Can't call args on these instructions *)
         | Nop | Speculate | Commit | Label _ | Const _ | Jmp _ -> []
         | other -> Bril.Instr.args other)
    else after_kill
  ;;

  (* Upwards analysis *)
  let bottom : t = Var.Set.empty
end

module Block_with_analysis = struct
  type t =
    { block_begin : Live_vars.t
        (* The state before (after) the first (last) instruction in a forward (backward) analysis *)
    ; instructions : (Bril.Instr.t * Live_vars.t) list
        (* An instruction and the state before (after) the instruction executes in a forward (backward) analysis *)
    ; block_end : Live_vars.t
    (* The state after (before) the last (first) instruction in a forward (backward) analysis *)
    }
  [@@deriving compare, equal, sexp_of]
end

module State = struct
  type t =
    { worklist : string list
    ; blocks : Block_with_analysis.t String.Map.t
    ; edges : string list String.Map.t
    }
  [@@deriving sexp_of]

  (* List.rev since backwards *)
  let of_func (func : Bril.Func.t) =
    { worklist = List.rev func.order
    ; blocks =
        Map.map func.blocks ~f:(fun instrs : Block_with_analysis.t ->
          { block_begin = Live_vars.bottom
          ; instructions = List.map instrs ~f:(fun ins -> ins, Live_vars.bottom)
          ; block_end = Live_vars.bottom
          })
    ; edges = func.succs
    }
  ;;

  let update_one (t : t) : [ `Keep_going of t | `Done of t ] =
    let predecessors (label : string) = Map.find_exn t.edges label in
    let run_block (label : string) (block : Block_with_analysis.t) : Block_with_analysis.t
      =
      let block_begin =
        let pred_analyses =
          predecessors label
          |> List.map ~f:(fun pred -> (Map.find_exn t.blocks pred).block_end)
        in
        pred_analyses |> List.fold ~init:Live_vars.bottom ~f:Live_vars.join
      in
      let block_end, instructions =
        List.fold
          (List.rev block.instructions)
          ~init:(block_begin, [])
          ~f:(fun (analysis, instrs) (instr, _) ->
            (* eprint_s [%message "before transfer" (analysis : Live_vars.t)]; *)
            (* eprint_s [%message "    " (instr : Bril.Instr.t)]; *)
            let analysis = Live_vars.transfer analysis instr in
            (* eprint_s [%message "after transfer" (analysis : Live_vars.t)]; *)
            analysis, (instr, analysis) :: instrs)
      in
      { block_begin; instructions; block_end }
    in
    match t.worklist with
    | [] -> `Done t
    | label :: rest ->
      let block = Map.find_exn t.blocks label in
      let updated = run_block label block in
      let blocks = Map.set t.blocks ~key:label ~data:updated in
      let worklist =
        if Block_with_analysis.equal block updated
        then rest
        else rest @ predecessors label
      in
      `Keep_going { t with worklist; blocks }
  ;;
end

let run func =
  let rec loop (state : State.t) =
    match State.update_one state with
    | `Done state -> state
    | `Keep_going state -> loop state
  in
  let analysis = loop (State.of_func func) in
  let optimized_instrs =
    List.concat_map func.order ~f:(fun label ->
      let analysis = Map.find_exn analysis.blocks label in
      let instrs, _ =
        List.fold
          (List.rev analysis.instructions)
          ~init:([], analysis.block_begin)
          ~f:(fun (output, live) (instr, next_live) ->
            eprint_s
              [%message
                (instr : Bril.Instr.t) (live : Live_vars.t) (next_live : Live_vars.t)];
            let opt_instr =
              if Live_vars.is_root instr
              then Some instr
              else (
                match Bril.Instr.dest instr with
                | None -> None
                | Some (dest, _) -> if Set.mem live dest then Some instr else None)
            in
            let output =
              match opt_instr with
              | Some opt_instr -> opt_instr :: output
              | None -> output
            in
            output, next_live)
      in
      instrs)
  in
  eprint_s [%message "" (analysis : State.t)];
  Bril.Func.set_instrs func optimized_instrs
;;
