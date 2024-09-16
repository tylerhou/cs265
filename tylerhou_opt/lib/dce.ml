open! Core

module Var = struct
  module T = struct
    type t = string [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module Live_vars = struct
  type t = Var.Set.t

  let join (t1 : t) (t2 : t) : t = Set.union t1 t2

  let transfer (t : t) (instr : Bril.Instr.t) =
    let dest = Bril.Instr.dest instr in
    let will_args_be_used =
      let will_dest_be_used =
        match dest with
        | None -> false
        | Some (dest, _) -> Set.mem t dest
      in
      let is_root =
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
      in
      will_dest_be_used || is_root
    in
    let killed =
      match dest with
      | None -> t
      | Some (dest, _) -> Set.remove t dest
    in
    if will_args_be_used
    then List.fold ~init:killed ~f:Set.add (Bril.Instr.args instr)
    else killed
  ;;
end

module State = struct
  type t =
    { worklist : string list
    ; live_vars_by_block : Live_vars.t String.Map.t
    }

  let empty = { worklist = []; live_vars_by_block = String.Map.empty }
end

let run func = failwith "todo"
