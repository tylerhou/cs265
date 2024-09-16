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
      | Unary of Bril.Op.Unary.t * Var.t
      | Binary of Bril.Op.Binary.t * Var.t * Var.t
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let of_ins (ins : Bril.Instr.t) : (Bril.Dest.t * t) option =
    match ins with
    | Unary (dest, op, var) -> Some (dest, Unary (op, var))
    | Binary (dest, op, left, right) -> Some (dest, Binary (op, left, right))
    | _ -> None
  ;;
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
    ; next_number : Number.t
    }

  val empty : t

  (*val to_var : t -> Number.t -> Var.t*)
  val of_var : t -> Var.t -> Number.t option
  val number_var : t -> Var.t -> t

  val number_expr
    :  t
    -> Var.t
    -> Expr.t
    -> t * [ `Expression_available of Var.t | `New_expression ]

  val clobber : t -> Number.t -> t
end = struct
  type t =
    { number_by_var : Number.t Var.Map.t
    ; var_by_number : Var.t Number.Map.t
    ; number_by_expr : Number.t Expr.Map.t
    ; rdeps_by_number : Number.t list Number.Map.t
    ; next_number : Number.t
    }

  let empty =
    { number_by_var = Var.Map.empty
    ; var_by_number = Number.Map.empty
    ; number_by_expr = Expr.Map.empty
    ; rdeps_by_number = Number.Map.empty
    ; next_number = Number.zero
    }
  ;;

  let to_var (t : t) (num : Number.t) = Map.find_exn t.var_by_number num
  let of_var (t : t) (var : Var.t) = Map.find t.number_by_var var
  let fresh_num t = { t with next_number = Number.succ t.next_number }, t.next_number

  let number_var (t : t) (var : Var.t) =
    match Map.find t.number_by_var var with
    | Some _ -> t
    | None ->
      let t, number = fresh_num t in
      let number_by_var = Map.set t.number_by_var ~key:var ~data:number in
      let var_by_number = Map.set t.var_by_number ~key:number ~data:var in
      { t with number_by_var; var_by_number }
  ;;

  let number_expr (t : t) (var : Var.t) (expr : Expr.t) =
    match Map.find t.number_by_expr expr with
    | Some number ->
      let number_by_var = Map.set t.number_by_var ~key:var ~data:number in
      { t with number_by_var }, `Expression_available (to_var t number)
    | None ->
      let t, number = fresh_num t in
      let number_by_var = Map.set t.number_by_var ~key:var ~data:number in
      let var_by_number = Map.set t.var_by_number ~key:number ~data:var in
      let number_by_expr = Map.set t.number_by_expr ~key:expr ~data:number in
      let rdeps_by_number =
        match expr with
        | Unary (_, var) ->
          t.rdeps_by_number
          |> Map.add_multi ~key:(Map.find_exn t.number_by_var var) ~data:number
        | Binary (_, left, right) ->
          t.rdeps_by_number
          |> Map.add_multi ~key:(Map.find_exn t.number_by_var left) ~data:number
          |> Map.add_multi ~key:(Map.find_exn t.number_by_var right) ~data:number
      in
      ( { t with number_by_var; var_by_number; number_by_expr; rdeps_by_number }
      , `New_expression )
  ;;

  let clobber (t : t) (dest : Number.t) =
    let rec find_deps (num : Number.t) : Number.t list =
      num :: (Map.find_multi t.rdeps_by_number num |> List.map ~f:find_deps |> List.concat)
    in
    let deps = find_deps dest |> Number.Set.of_list in
    eprint_s [%message "clobbering" (deps |> Set.to_list : Number.t list)];
    let keep number = not (Set.mem deps number) in
    let number_by_var = Map.filter t.number_by_var ~f:keep in
    let number_by_expr = Map.filter t.number_by_expr ~f:keep in
    let rdeps_by_number = Map.filter_keys t.rdeps_by_number ~f:keep in
    { t with number_by_var; number_by_expr; rdeps_by_number }
  ;;
end

let run (fn : Bril.Func.t) =
  eprintf "\n";
  let run_block (block : Bril.Instr.t list) =
    let _, rev_instrs =
      List.fold block ~init:(Numbering.empty, []) ~f:(fun (numbering, instrs) instr ->
        eprint_s [%message (instr : Bril.Instr.t)];
        let to_clobber =
          match Bril.Instr.dest instr with
          | Some (dest_var, _) -> Numbering.of_var numbering dest_var
          | None -> None
        in
        let numbering, instrs =
          match Expr.of_ins instr with
          | Some (((dest_var, _) as dest), expr) ->
            let numbering, result = Numbering.number_expr numbering dest_var expr in
            let instr : Bril.Instr.t =
              match result with
              | `Expression_available var -> Unary (dest, Id, var)
              | `New_expression -> instr
            in
            numbering, instr :: instrs
          | None ->
            (match Bril.Instr.dest instr with
             | Some (dest_var, _) ->
               let numbering = Numbering.number_var numbering dest_var in
               eprint_s
                 [%message
                   "" (dest_var : string) (numbering.number_by_var : Number.t Var.Map.t)];
               numbering, instr :: instrs
             | None -> numbering, instr :: instrs)
        in
        eprint_s [%message "" (numbering.number_by_var : Number.t Var.Map.t)];
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
