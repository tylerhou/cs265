open! Core

module Number : sig
  type t [@@deriving compare, equal, sexp]

  include Comparable.S_plain with type t := t

  val zero : t
  val succ : t -> t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)

  let zero = 0
  let succ t = t + 1
end

(*module Lattice = struct*)
(*  type t =*)
(*    | Not_yet_initialized (* Bottom *)*)
(*    | Numbered of Number.t*)
(*    | Multiple_values (* Top *)*)
(*  [@@deriving sexp]*)
(**)
(*  let join (left : t) (right : t) : t =*)
(*    match left, right with*)
(*    | Not_yet_initialized, right -> right*)
(*    | left, Not_yet_initialized -> left*)
(*    | Numbered l, Numbered r -> if Number.equal l r then left else Multiple_values*)
(*    | Multiple_values, _ -> Multiple_values*)
(*    | _, Multiple_values -> Multiple_values*)
(*  ;;*)
(*end*)

module Var = struct
  module T = struct
    type t = string [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module Expr = struct
  module T = struct
    type t =
      | Const of Bril.Const.t
      | Unary of Bril.Op.Unary.t * Number.t
      | Binary of Bril.Op.Binary.t * Number.t * Number.t
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module Numbering : sig
  (* Invariants:

     1. There is a variable for every number, and possibly an expression.
     2. There is a number for every initialized variable.
  *)
  type t =
    { number_by_var : Number.t Var.Map.t
    ; var_by_number : Var.t Number.Map.t (* TODO: var_expr_by_number *)
    ; number_by_expr : Number.t Expr.Map.t
    ; rdeps_by_number : Number.t list Number.Map.t
    ; simplified : Expr.t Number.Map.t
    ; next_number : Number.t
    }

  val empty : t
  val to_var : t -> Number.t -> Var.t
  val of_var : t -> Var.t -> Number.t option
  val expr_of_ins : t -> Bril.Instr.t -> t * (Bril.Dest.t * Expr.t) option

  val number_expr
    :  t
    -> Var.t
    -> Expr.t
    -> t * [ `Available_expression of Number.t | `New_expression of Number.t ]

  val clobber : t -> Number.t -> t
  val simplify : t -> Number.t -> Expr.t -> t * Expr.t
  val simplified : t -> Number.t -> Expr.t option
  val to_instr : t -> Bril.Dest.t -> Expr.t -> Bril.Instr.t
end = struct
  type t =
    { number_by_var : Number.t Var.Map.t
    ; var_by_number : Var.t Number.Map.t
    ; number_by_expr : Number.t Expr.Map.t
    ; rdeps_by_number : Number.t list Number.Map.t
    ; simplified : Expr.t Number.Map.t
    ; next_number : Number.t
    }

  let empty =
    { number_by_var = Var.Map.empty
    ; var_by_number = Number.Map.empty
    ; number_by_expr = Expr.Map.empty
    ; rdeps_by_number = Number.Map.empty
    ; simplified = Number.Map.empty
    ; next_number = Number.zero
    }
  ;;

  let to_var (t : t) (num : Number.t) = Map.find_exn t.var_by_number num
  let of_var (t : t) (var : Var.t) = Map.find t.number_by_var var
  let fresh_num t = { t with next_number = Number.succ t.next_number }, t.next_number

  let number_var (t : t) (var : Var.t) =
    match Map.find t.number_by_var var with
    | Some number -> t, number
    | None ->
      let t, number = fresh_num t in
      let number_by_var = Map.set t.number_by_var ~key:var ~data:number in
      let var_by_number = Map.set t.var_by_number ~key:number ~data:var in
      { t with number_by_var; var_by_number }, number
  ;;

  let expr_of_ins (t : t) (ins : Bril.Instr.t) : t * (Bril.Dest.t * Expr.t) option =
    let of_var t v =
      match of_var t v with
      | Some num -> t, num
      | None -> number_var t v
    in
    match ins with
    | Const (dest, const) -> t, Some (dest, Const const)
    | Unary (dest, op, var) ->
      let t, var = of_var t var in
      t, Some (dest, Unary (op, var))
    | Binary (dest, op, left, right) ->
      let t, left = of_var t left in
      let t, right = of_var t right in
      t, Some (dest, Binary (op, left, right))
    | _ -> t, None
  ;;

  let number_expr (t : t) (var : Var.t) (expr : Expr.t) =
    match Map.find t.number_by_expr expr with
    | Some number ->
      let number_by_var = Map.set t.number_by_var ~key:var ~data:number in
      { t with number_by_var }, `Available_expression number
    | None ->
      let t, number = fresh_num t in
      let number_by_var = Map.set t.number_by_var ~key:var ~data:number in
      let var_by_number = Map.set t.var_by_number ~key:number ~data:var in
      let number_by_expr = Map.set t.number_by_expr ~key:expr ~data:number in
      let rdeps_by_number =
        match expr with
        | Const _ -> t.rdeps_by_number
        | Unary (_, var) -> t.rdeps_by_number |> Map.add_multi ~key:var ~data:number
        | Binary (_, left, right) ->
          t.rdeps_by_number
          |> Map.add_multi ~key:left ~data:number
          |> Map.add_multi ~key:right ~data:number
      in
      ( { t with number_by_var; var_by_number; number_by_expr; rdeps_by_number }
      , `New_expression number )
  ;;

  let clobber (t : t) (dest : Number.t) =
    let rec find_deps (num : Number.t) : Number.t list =
      num :: (Map.find_multi t.rdeps_by_number num |> List.map ~f:find_deps |> List.concat)
    in
    let deps = find_deps dest |> Number.Set.of_list in
    eprint_s [%message "Clobbering" (deps |> Set.to_list : Number.t list)];
    let keep number = not (Set.mem deps number) in
    let number_by_var = Map.filter t.number_by_var ~f:keep in
    let number_by_expr = Map.filter t.number_by_expr ~f:keep in
    let rdeps_by_number = Map.filter_keys t.rdeps_by_number ~f:keep in
    { t with number_by_var; number_by_expr; rdeps_by_number }
  ;;

  let simplified (t : t) (num : Number.t) = Map.find t.simplified num

  let simplify (t : t) (num : Number.t) (expr : Expr.t) =
    eprint_s [%message "Attempting to simplify" (expr : Expr.t)];
    let maybe_simplified : Expr.t option =
      match expr with
      | Const _ -> None
      (* Copy propagation *)
      | Unary (Id, num) ->
        (match simplified t num with
         | Some simpl -> Some simpl
         | None -> None)
      | Unary (Not, num) ->
        (match simplified t num with
         | Some (Const (Bool b)) -> Some (Const (Bool (not b)))
         | _ -> None)
      | Binary (op, left, right) ->
        let result : Bril.Const.t option =
          match simplified t left, simplified t right with
          | Some (Const (Int l)), Some (Const (Int r)) ->
            (match op with
             | Add -> Some (Int (l + r))
             | Mul -> Some (Int (l * r))
             | Sub -> Some (Int (l - r))
             | Div when r <> 0 -> Some (Int (l / r))
             | Div -> None
             | Eq -> Some (Bool (l = r))
             | Lt -> Some (Bool (l < r))
             | Gt -> Some (Bool (l > r))
             | Le -> Some (Bool (l <= r))
             | Ge -> Some (Bool (l >= r))
             | _ -> None)
          | Some (Const (Bool l)), Some (Const (Bool r)) ->
            (match op with
             | And -> Some (Bool (l && r))
             | Or -> Some (Bool (l || r))
             | _ -> None)
          | Some (Const (Bool true)), _ | _, Some (Const (Bool true)) ->
            (match op with
             | Or -> Some (Bool true)
             | _ -> None)
          | Some (Const (Bool false)), _ | _, Some (Const (Bool false)) ->
            (match op with
             | And -> Some (Bool false)
             | _ -> None)
          | _ ->
            if Number.equal left right
            then (
              match op with
              | Sub -> Some (Int 0)
              | Div -> Some (Int 1)
              | Lt | Gt -> Some (Bool false)
              | Eq | Le | Ge -> Some (Bool true)
              | _ -> None)
            else None
        in
        Option.map result ~f:(fun r -> Expr.Const r)
    in
    let expr =
      match maybe_simplified with
      | None -> expr
      | Some expr ->
        eprint_s [%message "Simplified to" (expr : Expr.t)];
        expr
    in
    let simplified = Map.add_exn t.simplified ~key:num ~data:expr in
    { t with simplified }, expr
  ;;

  let to_instr (t : t) (dest : Bril.Dest.t) (expr : Expr.t) : Bril.Instr.t =
    let to_var arg = to_var t arg in
    match expr with
    | Const const -> Const (dest, const)
    | Unary (op, arg) -> Unary (dest, op, to_var arg)
    | Binary (op, left, right) -> Binary (dest, op, to_var left, to_var right)
  ;;
end

let run (fn : Bril.Func.t) =
  eprintf "\n";
  let run_block (block : Bril.Instr.t list) =
    let _, rev_instrs =
      List.fold block ~init:(Numbering.empty, []) ~f:(fun (numbering, instrs) instr ->
        eprint_s [%message "Processing instruction" (instr : Bril.Instr.t)];
        let to_clobber =
          match Bril.Instr.dest instr with
          | Some (dest_var, _) -> Numbering.of_var numbering dest_var
          | None -> None
        in
        let numbering, instrs =
          match Numbering.expr_of_ins numbering instr with
          | numbering, None -> numbering, instr :: instrs
          | numbering, Some (((dest_var, _) as dest), expr) ->
            let numbering, result = Numbering.number_expr numbering dest_var expr in
            let numbering, instr =
              match result with
              | `New_expression number ->
                let numbering, expr = Numbering.simplify numbering number expr in
                numbering, Numbering.to_instr numbering dest expr
              | `Available_expression number ->
                ( numbering
                , (match Numbering.simplified numbering number with
                   | None -> Unary (dest, Id, Numbering.to_var numbering number)
                   | Some expr -> Numbering.to_instr numbering dest expr) )
            in
            numbering, instr :: instrs
        in
        let numbering =
          match to_clobber with
          | Some old_dest_number -> Numbering.clobber numbering old_dest_number
          | None -> numbering
        in
        eprintf "\n";
        numbering, instrs)
    in
    rev_instrs |> List.rev
  in
  let instrs =
    List.map fn.order ~f:(fun label -> run_block (Map.find_exn fn.blocks label))
    |> List.concat
  in
  Bril.Func.set_instrs fn instrs
;;
