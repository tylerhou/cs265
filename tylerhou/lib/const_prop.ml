open! Core

module Lattice = struct
  module Value = struct
    type t =
      | Uninitialized (* Bottom *)
      | Constant of Bril.Const.t
      | Multiple_values (* Top *)
    [@@deriving compare, equal, sexp_of]

    let join (left : t) (right : t) : t =
      match left, right with
      | Uninitialized, right -> right
      | left, Uninitialized -> left
      | Constant l, Constant r -> if Bril.Const.equal l r then left else Multiple_values
      | Multiple_values, _ -> Multiple_values
      | _, Multiple_values -> Multiple_values
    ;;
  end

  type t = Value.t Var.Map.t [@@deriving compare, equal, sexp_of]

  let join (t1 : t) (t2 : t) =
    Map.merge t1 t2 ~f:(fun ~key:_ elt ->
      Some
        (match elt with
         | `Both (v1, v2) -> Value.join v1 v2
         | `Left v | `Right v -> v))
  ;;

  let bottom = Var.Map.empty
  let init = Var.Map.empty
end

module Transfer = struct
  module Lattice = Lattice

  let transfer (before : Lattice.t) ~point:_ ~(instr : Bril.Instr.t) : Lattice.t =
    let dest_val : Lattice.Value.t =
      match instr with
      | Const (_, arg) -> Constant arg
      | Unary (_, Id, arg) ->
        Map.find before arg |> Option.value ~default:Uninitialized
      | Unary (_, Not, arg) ->
        (match Map.find before arg with
         | Some (Constant (Bool b)) -> Constant (Bool (not b))
         | Some (Constant (Int _)) ->
           Multiple_values (* undefined behavior, be conservative *)
         | Some other -> other
         | None -> Uninitialized)
      | Binary (_, op, left, right) ->
        (match op with
         | Add | Mul | Sub | Div | Eq | Lt | Gt | Le | Ge ->
           (match
              ( Option.value (Map.find before left) ~default:Uninitialized
              , Option.value (Map.find before right) ~default:Uninitialized )
            with
            | Uninitialized, Uninitialized -> Uninitialized
            | Multiple_values, _ | _, Multiple_values -> Multiple_values
            | Uninitialized, Constant _ | Constant _, Uninitialized ->
              Multiple_values (* Be conservative *)
            | Constant (Int left), Constant (Int right) ->
              Constant
                (match op with
                 | Add -> Int (left + right)
                 | Mul -> Int (left * right)
                 | Sub -> Int (left - right)
                 | Div -> Int (left / right)
                 | Eq -> Bool (left = right)
                 | Lt -> Bool (left < right)
                 | Gt -> Bool (left > right)
                 | Le -> Bool (left <= right)
                 | Ge -> Bool (left >= right)
                 | _ -> failwith "not possible")
            | _ -> Multiple_values (* Be conservative *))
         | And | Or ->
           (match
              ( Option.value (Map.find before left) ~default:Uninitialized
              , Option.value (Map.find before right) ~default:Uninitialized )
            with
            | Uninitialized, Uninitialized -> Uninitialized
            | Multiple_values, _ | _, Multiple_values -> Multiple_values
            | Uninitialized, Constant _ | Constant _, Uninitialized ->
              Multiple_values (* Be conservative *)
            | Constant (Bool left), Constant (Bool right) ->
              Constant
                (match op with
                 | And -> Bool (left && right)
                 | Or -> Bool (left || right)
                 | _ -> failwith "not possible")
            | _ -> Multiple_values (* Be conservative *)))
      | _ -> Multiple_values
    in
    let after =
      match Bril.Instr.dest instr with
      | Some (dest, _) -> Map.set before ~key:dest ~data:dest_val
      | None -> before
    in
    after
  ;;

  let direction = `Forwards
end

module Analyze_const_prop = Dataflow.Make (Transfer)

let run func =
  let blocks = Analyze_const_prop.run func in
  let optimized_instrs =
    List.concat_map func.order ~f:(fun label ->
      let block = Map.find_exn blocks label in
      let module Block = Analyze_const_prop.Block in
      let instrs =
        block
        |> Block.to_list
        |> List.map ~f:(fun ({ instr; after; _ } : Block.instr_with_lattice) ->
          match Bril.Instr.dest instr with
          | Some ((dest_var, _) as dest) ->
            (match Map.find after dest_var with
             | Some (Constant c) -> Bril.Instr.Const (dest, c)
             | _ -> instr)
          | None -> instr)
      in
      instrs)
  in
  Bril.Func.set_instrs func optimized_instrs
;;
