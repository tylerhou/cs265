open! Core

let is_root (instr : Bril.Instr.body) =
  match instr with
  | Const _ -> false
  | Binary _ -> false
  | Unary _ -> false
  | Call _ -> true
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

let run func =
  let blocks = Liveness.analyze func in
  let optimized_instrs =
    List.concat_map func.order ~f:(fun label ->
      let block = Map.find_exn blocks label in
      let module Block = Liveness.Analysis.Block in
      let body =
        Block.instrs_with_lattice block
        |> List.filter_map
             ~f:
               (fun
                 ({ instr; before = { default; values = live }; _ } :
                   Block.instr_with_lattice)
               ->
               if is_root instr
               then Some instr
               else (
                 match Bril.Instr.dest instr with
                 | None -> None
                 | Some (dest, _) ->
                   let value : Liveness.Liveness.value =
                     match Map.find live dest with
                     | None -> default
                     | Some live -> live
                   in
                   (match value with
                    | Live -> Some instr
                    | Dead -> None)))
      in
      Bril.Func.Block.
        { header = Block.header block; body; terminator = Block.terminator block }
      |> Bril.Func.Block.instrs)
  in
  Bril.Func.set_instrs func optimized_instrs
;;
