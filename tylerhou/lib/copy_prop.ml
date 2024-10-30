open! Core

module Copies = struct
  type t =
    | Bottom
    | Vars of Var.t Var.Map.t
  [@@deriving compare, equal, sexp_of]

  let bottom : t = Bottom
  let init : t = Vars Var.Map.empty

  let join (t1 : t) (t2 : t) : t =
    match t1, t2 with
    | Bottom, t | t, Bottom -> t
    | Vars l, Vars r ->
      Vars
        (Map.merge l r ~f:(fun ~key ->
             function
             | `Left v | `Right v -> Some v
             | `Both (l, r) ->
               if not (String.equal l r)
               then
                 Error.raise_s
                   [%message "copies do not match" (key : Var.t) (l : Var.t) (r : Var.t)]
               else Some l))
  ;;
end

module Transfer = struct
  module Lattice = Copies

  let transfer (copies : Copies.t) ~point:_ ~(instr : Bril.Instr.t) : Copies.t =
    match instr, copies with
    | Unary ((dest, _), Id, arg), Vars copies -> Vars (Map.set copies ~key:dest ~data:arg)
    | _ -> copies
  ;;

  let direction = `Forwards
end

module Analyze = Dataflow.Make (Transfer)

let run func =
  let blocks = Analyze.run func in
  eprint_s [%message "copy prop" (blocks : Analyze.Block.t String.Map.t)];
  let optimized_instrs =
    List.concat_map func.order ~f:(fun label ->
      let block = Map.find_exn blocks label in
      let module Block = Analyze.Block in
      let instrs =
        block
        |> Block.to_list
        |> List.map ~f:(fun { before; instr; _ } ->
          match instr, before with
          | (Nop | Speculate | Commit | Label _ | Const (_, _) | Jmp _ | Phi _), _ -> instr
          | instr, Vars before ->
            let args =
              Bril.Instr.args instr
              |> List.map ~f:(fun arg ->
                match Map.find before arg with
                | Some existing -> existing
                | None -> arg)
            in
            Bril.Instr.set_args args instr
          | _, Bottom -> Error.raise_s [%message "block still bottom" (block : Block.t)])
      in
      instrs)
  in
  Bril.Func.set_instrs func optimized_instrs
;;
