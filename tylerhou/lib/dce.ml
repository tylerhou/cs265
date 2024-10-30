open! Core

module Live_vars = struct
  type t = Var.Set.t [@@deriving compare, equal, sexp_of]

  let bottom : t = Var.Set.empty
  let init : t = Var.Set.empty
  let join (t1 : t) (t2 : t) : t = Set.union t1 t2
end

module Transfer = struct
  module Lattice = Live_vars

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

  let transfer (live : Live_vars.t) ~point:_ ~(instr : Bril.Instr.t)
    : Live_vars.t
    =
    let dest = Bril.Instr.dest instr in
    let will_args_be_used =
      let will_dest_be_used =
        match dest with
        | None -> false
        | Some (dest, _) -> Set.mem live dest
      in
      will_dest_be_used || is_root instr
    in
    let after_kill =
      match dest with
      | None -> live
      | Some (dest, _) -> Set.remove live dest
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

  let direction = `Backwards
end

module Analyze_live_vars = Dataflow.Make (Transfer)

let run func =
  let blocks = Analyze_live_vars.run func in
  let optimized_instrs =
    List.concat_map func.order ~f:(fun label ->
      let block = Map.find_exn blocks label in
      let module Block = Analyze_live_vars.Block in
      let instrs =
        block
        |> Block.to_list
        |> List.filter_map
             ~f:(fun ({ instr; before = live; _ } : Block.instr_with_lattice) ->
               eprint_s [%message (instr : Bril.Instr.t) (live : Live_vars.t)];
               if Transfer.is_root instr
               then Some instr
               else (
                 match Bril.Instr.dest instr with
                 | None -> None
                 | Some (dest, _) -> if Set.mem live dest then Some instr else None))
      in
      instrs)
  in
  eprint_s [%message "" (blocks : Analyze_live_vars.Block.t String.Map.t)];
  Bril.Func.set_instrs func optimized_instrs
;;
