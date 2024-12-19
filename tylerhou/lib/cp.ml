open! Core

module Constant = struct
  type value =
    | Multiple_values
    | Constant of Bril.Const.t
    | Not_initialized
  [@@deriving compare, equal, sexp_of]

  let bottom = Not_initialized
  let top = Multiple_values

  let join_value (v1 : value) (v2 : value) =
    eprint_s [%message "join_value" (v1 : value) (v2 : value)];
    match v1, v2 with
    | Not_initialized, other | other, Not_initialized -> other
    | Constant l, Constant r ->
      if Bril.Const.equal l r then Constant l else Multiple_values
    | _ -> Multiple_values
  ;;

  let boundary = Not_initialized

  type t =
    { default : value
    ; values : value Var.Map.t
    }
  [@@deriving compare, equal, sexp_of]
end

module Transfer = struct
  module Lattice = Constant

  let transfer
    ({ default; values } : Constant.t)
    ~point:_
    ~(instr : [ `Body of Bril.Instr.body | `Terminator of Bril.Instr.terminator ])
    : Constant.t
    =
    let dest_and_value : (Var.t * Lattice.value) option =
      match instr with
      | `Body (Const ((dest, _), const)) -> Some (dest, Constant const)
      | `Body (Unary ((dest, _), Id, var)) ->
        Some
          ( dest
          , match Map.find values var with
            | None -> default
            | Some existing -> existing )
      | _ -> None
    in
    { default
    ; values =
        (match dest_and_value with
         | None -> values
         | Some (dest, value) -> Map.set values ~key:dest ~data:value)
    }
  ;;

  let direction = `Forwards
end

module Analysis = Dataflow.Make (Transfer)

let run func =
  let blocks = Analysis.run func in
  eprint_s [%message "constants" (blocks : Analysis.Block.t String.Map.t)];
  let optimized_instrs =
    List.concat_map func.order ~f:(fun label ->
      let block = Map.find_exn blocks label in
      let module Block = Analysis.Block in
      let body =
        Block.instrs_with_lattice block
        |> List.map
             ~f:
               (fun
                 ({ instr; after = { default; values = constants }; _ } :
                   Block.instr_with_lattice)
               ->
               match Bril.Instr.dest instr with
               | None -> instr
               | Some ((var, _) as dest) ->
                 let value : Constant.value =
                   match Map.find constants var with
                   | None -> default
                   | Some live -> live
                 in
                 (match value with
                  | Multiple_values | Not_initialized -> instr
                  | Constant const -> Const (dest, const)))
      in
      Bril.Func.Block.
        { header = Block.header block; body; terminator = Block.terminator block }
      |> Bril.Func.Block.instrs)
  in
  Bril.Func.set_instrs func optimized_instrs
;;
