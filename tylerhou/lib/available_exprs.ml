open! Core

module Available_exprs = struct
  type t =
    | Bottom
    | Some of Var.t Pure_expr.Map.t
  [@@deriving compare, equal, sexp_of]

  let bottom : t = Bottom
  let init : t = Some Pure_expr.Map.empty

  let join (t1 : t) (t2 : t) : t =
    match t1, t2 with
    | Bottom, t | t, Bottom -> t
    | Some t1, Some t2 ->
      Some
        (Map.merge t1 t2 ~f:(fun ~key:_ ->
             function
             | `Left _ | `Right _ -> None
             | `Both (l, r) -> if Var.equal l r then Some l else None))
  ;;
end

module Transfer = struct
  module Lattice = Available_exprs

  let transfer (available : Available_exprs.t) ~point:_ ~(instr : Bril.Instr.t)
    : Available_exprs.t
    =
    match available with
    | Bottom -> Bottom
    | Some available ->
      let killed = available in
      let gen =
        match Pure_expr.of_instr instr with
        | Some (dest, _, expr) ->
          (* Prefer the earlier instruction so prevent chains of copies. *)
          (match Map.add killed ~key:expr ~data:dest with
           | `Ok gen -> gen
           | `Duplicate -> killed)
        | None -> killed
      in
      Some gen
  ;;

  let direction = `Forwards
end

module Analyze = Dataflow.Make (Transfer)

let run func =
  let blocks = Analyze.run func in
  eprint_s [%message "available expressions" (blocks : Analyze.Block.t String.Map.t)];
  let with_available_expressions =
    List.concat_map func.order ~f:(fun label ->
      let module Block = Analyze.Block in
      let block = Map.find_exn blocks label in
      block
      |> Block.to_list
      |> List.map ~f:(fun { before; instr; _ } : Bril.Instr.t ->
        match Pure_expr.of_instr instr, before with
        | Some (dest, dest_type, expr), Some before ->
          (match Map.find before expr with
           | Some available -> Unary ((dest, dest_type), Id, available)
           | None -> instr)
        | _, Bottom ->
          Error.raise_s
            [%message "still bottom" (instr : Bril.Instr.t) (before : Available_exprs.t)]
        | _ -> instr))
  in
  Bril.Func.set_instrs func with_available_expressions
;;
