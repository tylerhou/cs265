open! Core

module Very_busy_exprs = struct
  type t =
    | Bottom
    | Some of Pure_expr.Set.t
  [@@deriving compare, equal, sexp_of]

  let bottom : t = Bottom
  let init : t = Some Pure_expr.Set.empty

  let join (t1 : t) (t2 : t) : t =
    match t1, t2 with
    | Bottom, t | t, Bottom -> t
    | Some t1, Some t2 -> Some (Set.inter t1 t2)
  ;;
end

module Transfer = struct
  module Lattice = Very_busy_exprs

  let transfer (busy : Very_busy_exprs.t) ~label:_ ~(instr : Bril.Instr.t)
    : Very_busy_exprs.t
    =
    match busy with
    | Some busy ->
      let killed =
        match Bril.Instr.dest instr with
        | Some (dest, _) ->
          Set.filter busy ~f:(fun expr ->
            match expr with
            | Const _ -> true
            | Unary (_, _, arg) -> not (Var.equal dest arg)
            | Binary (_, left, _, right) ->
              (not (Var.equal dest left)) && not (Var.equal dest right))
        | None -> busy
      in
      let gen =
        match Pure_expr.of_instr instr with
        | Some (_, _, expr) -> Set.add killed expr
        | None -> killed
      in
      Some gen
    | Bottom -> Bottom
  ;;

  let direction = `Backwards
end

module Analyze = Dataflow.Make (Transfer)

let run (func : Bril.Func.t) =
  let fresh_instr_prefix = Int32.to_string (Random.int32 Int32.max_value) in
  let blocks = Analyze.run func in
  eprint_s [%message "very busy expressions" (blocks : Analyze.Block.t String.Map.t)];
  let with_early_busy_instrs =
    (* Approximation of fresh variable *)
    let fresh : unit -> Var.t =
      let counter = ref 0 in
      fun () ->
        let id = !counter in
        incr counter;
        Format.sprintf "%s.%d" fresh_instr_prefix id
    in
    List.concat_map func.order ~f:(fun label ->
      let module Block = Analyze.Block in
      let block = Map.find_exn blocks label in
      let instrs = Block.instrs block in
      let before = Block.before block in
      eprint_s [%message (before : Very_busy_exprs.t) (label : string)];
      let very_busy_instrs =
        match before with
        | Bottom -> Error.raise_s [%message "block is still bottom" (block : Block.t)]
        | Some before ->
          before
          |> Set.to_list
          |> List.filter ~f:(function
            | Unary (_, Id, _) -> false (* Copy propagate *)
            | _ -> true)
          |> List.map ~f:(fun expr -> Pure_expr.to_instr (fresh ()) expr)
      in
      let very_busy_instrs =
        List.filter very_busy_instrs ~f:(fun instr ->
          not (List.mem ~equal:Bril.Instr.equal instrs instr))
      in
      let instrs, terminator =
        match List.rev instrs with
        | ((Jmp _ | Br _ | Ret _) as terminator) :: rev_instrs ->
          List.rev rev_instrs, [ terminator ]
        | _ -> instrs, []
      in
      instrs @ very_busy_instrs @ terminator)
  in
  eprint_s [%message (with_early_busy_instrs : Bril.Instr.t list)];
  Bril.Func.set_instrs func with_early_busy_instrs
;;
