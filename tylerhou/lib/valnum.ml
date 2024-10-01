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

module Var = struct
  module T = struct
    type t = string [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module With_state = struct
  module type S = sig
    include Monad.S

    type state

    val run : 'a t -> state:state -> state * 'a
    val get : state t
    val put : state -> unit t
  end

  module Make (State : sig
      type t
    end) : sig
    module With_state : sig
      include S with type state := State.t
    end
  end = struct
    module With_state = struct
      module T = struct
        type 'a t = State.t -> State.t * 'a

        let run (t : 'a t) ~(state : State.t) : State.t * 'a = t state
        let get : State.t t = fun state -> state, state
        let put (state : State.t) : unit t = fun _ -> state, ()

        let bind (t : 'a t) ~(f : 'a -> 'b t) : 'b t =
          fun state ->
          let state, a = run t ~state in
          run (f a) ~state
        ;;

        let return (type a) (a : a) : a t = fun state -> state, a
        let map = `Define_using_bind
      end

      include T
      include Monad.Make (T)
    end
  end
end

module Numbering : sig
  (* Invariants:

     1. There is a variable for every number, and possibly an expression.
     2. There is a number for every initialized variable.
  *)
  type t

  module With_state : With_state.S with type state := t

  val empty : t
  val of_var : t -> Var.t -> Number.t option
  val to_instr : t -> Bril.Dest.t -> Number.t -> Bril.Instr.t

  module Stateful : sig
    val number_instr
      :  Bril.Instr.t
      -> [ `Cannot_number
         | `New_expression of Bril.Dest.t * Number.t
         | `Available_expression of Bril.Dest.t * Number.t
         ]
           With_state.t

    val clobber : Number.t -> unit With_state.t
  end
end = struct
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

  module T = struct
    type t =
      { number_by_var : Number.t Var.Map.t
      ; var_by_number : Var.t Number.Map.t
      ; number_by_expr : Number.t Expr.Map.t
      ; rdeps_by_number : Number.t list Number.Map.t
      ; simplified_by_number : Expr.t Number.Map.t
      ; next_number : Number.t
      }
  end

  include T
  include With_state.Make (T)

  let empty =
    { number_by_var = Var.Map.empty
    ; var_by_number = Number.Map.empty
    ; number_by_expr =
        Expr.Map.empty
        (* If an expr is a key in this map, we've computed the expression before. *)
    ; rdeps_by_number =
        Number.Map.empty
        (* For every number, stores a list of numbers that depend on it. Used
           to remove numbers from the above maps on clobber *)
    ; simplified_by_number = Number.Map.empty
    ; next_number = Number.zero
    }
  ;;

  let to_var (t : t) (num : Number.t) = Map.find_exn t.var_by_number num
  let of_var (t : t) (var : Var.t) = Map.find t.number_by_var var

  let to_instr (t : t) (dest : Bril.Dest.t) (num : Number.t) : Bril.Instr.t =
    let to_var n = to_var t n in
    match Map.find t.simplified_by_number num with
    | None -> Unary (dest, Id, to_var num)
    | Some (Const const) -> Const (dest, const)
    | Some (Unary (op, arg)) -> Unary (dest, op, to_var arg)
    | Some (Binary (op, left, right)) -> Binary (dest, op, to_var left, to_var right)
  ;;

  let simplify_expr (t : t) (expr : Expr.t) =
    eprint_s [%message "Attempting to simplify" (expr : Expr.t)];
    let simplified num = Map.find t.simplified_by_number num in
    let maybe_simplified : Expr.t option =
      match expr with
      | Const _ -> None
      (* Copy propagation *)
      | Unary (Id, num) ->
        (match simplified num with
         | Some simpl -> Some simpl
         | None -> None)
      | Unary (Not, num) ->
        (match simplified num with
         | Some (Const (Bool b)) -> Some (Const (Bool (not b)))
         | _ -> None)
      | Binary (op, left, right) ->
        let result : Bril.Const.t option =
          match simplified left, simplified right with
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
    expr
  ;;

  module Stateful = struct
    open With_state.Let_syntax

    let fresh_num : Number.t With_state.t =
      let%bind t = With_state.get in
      let%bind () = With_state.put { t with next_number = Number.succ t.next_number } in
      return t.next_number
    ;;

    let number_var (var : Var.t) : Number.t With_state.t =
      match%bind
        let%map t = With_state.get in
        Map.find t.number_by_var var
      with
      | Some number -> return number
      | None ->
        let%bind number = fresh_num in
        let%bind () =
          let%bind t = With_state.get in
          let number_by_var = Map.set t.number_by_var ~key:var ~data:number in
          let var_by_number = Map.set t.var_by_number ~key:number ~data:var in
          With_state.put { t with number_by_var; var_by_number }
        in
        return number
    ;;

    let number_expr (var : Var.t) (expr : Expr.t) =
      match%bind
        let%map t = With_state.get in
        Map.find t.number_by_expr expr
      with
      | Some number ->
        let%bind () =
          let%bind t = With_state.get in
          let number_by_var = Map.set t.number_by_var ~key:var ~data:number in
          With_state.put { t with number_by_var }
        in
        return (`Available_expression number)
      | None ->
        let%bind number = fresh_num in
        let%bind () =
          let%bind t = With_state.get in
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
          With_state.put
            { t with number_by_var; var_by_number; number_by_expr; rdeps_by_number }
        in
        return (`New_expression number)
    ;;

    let clobber (dest : Number.t) : unit With_state.t =
      let%bind deps =
        let%map t = With_state.get in
        let rec find_deps (num : Number.t) : Number.t list =
          num
          :: (Map.find_multi t.rdeps_by_number num |> List.map ~f:find_deps |> List.concat)
        in
        find_deps dest |> Number.Set.of_list
      in
      eprint_s [%message "Clobbering" (deps |> Set.to_list : Number.t list)];
      let keep number = not (Set.mem deps number) in
      let%bind () =
        let%bind t = With_state.get in
        let number_by_var = Map.filter t.number_by_var ~f:keep in
        let var_by_number = Map.filter_keys t.var_by_number ~f:keep in
        let number_by_expr = Map.filter t.number_by_expr ~f:keep in
        let rdeps_by_number = Map.filter_keys t.rdeps_by_number ~f:keep in
        With_state.put
          { t with number_by_var; var_by_number; number_by_expr; rdeps_by_number }
      in
      return ()
    ;;

    let simplify_and_add_expr (num : Number.t) (expr : Expr.t) : unit With_state.t =
      let%bind expr =
        let%map t = With_state.get in
         simplify_expr t expr
      in
      let%bind () =
        let%bind t = With_state.get in
        let simplified_by_number =
          Map.add_exn t.simplified_by_number ~key:num ~data:expr
        in
        With_state.put { t with simplified_by_number }
      in
      return ()
    ;;

    let expr_of_ins (ins : Bril.Instr.t) : (Bril.Dest.t * Expr.t) option With_state.t =
      let of_var v =
        let%bind t = With_state.get in
        match of_var t v with
        | Some num -> return num
        | None -> number_var v
      in
      match ins with
      | Const (dest, const) -> return (Some (dest, Expr.Const const))
      | Unary (dest, op, var) ->
        let%bind var = of_var var in
        return (Some (dest, Expr.Unary (op, var)))
      | Binary (dest, op, left, right) ->
        let%bind left = of_var left in
        let%bind right = of_var right in
        return (Some (dest, Expr.Binary (op, left, right)))
      | _ -> return None
    ;;

    let number_instr (instr : Bril.Instr.t)
      : [ `Cannot_number
        | `New_expression of Bril.Dest.t * Number.t
        | `Available_expression of Bril.Dest.t * Number.t
        ]
          With_state.t
      =
      match%bind expr_of_ins instr with
      | None -> return `Cannot_number
      | Some (((dest_var, _) as dest), expr) ->
        let%bind result = number_expr dest_var expr in
        (match result with
         | `New_expression number ->
           let%bind () = simplify_and_add_expr number expr in
           return (`New_expression (dest, number))
         | `Available_expression number -> return (`Available_expression (dest, number)))
    ;;
  end
end

let run (fn : Bril.Func.t) =
  let open Numbering.With_state.Let_syntax in
  eprintf "\n";
  let run_block (block : Bril.Instr.t list) =
    let _, rev_instrs =
      List.fold block ~init:(Numbering.With_state.return []) ~f:(fun instrs instr ->
        eprint_s [%message "Processing instruction" (instr : Bril.Instr.t)];
        let%bind instrs = instrs in
        let%bind to_clobber =
          match Bril.Instr.dest instr with
          | Some (dest_var, _) ->
            let%bind numbering = Numbering.With_state.get in
            return (Numbering.of_var numbering dest_var)
          | None -> return None
        in
        let%bind number = Numbering.Stateful.number_instr instr in
        let%bind instr =
          match number with
          | `Cannot_number -> return instr
          | `New_expression (dest, number) | `Available_expression (dest, number) ->
            let%bind numbering = Numbering.With_state.get in
            return (Numbering.to_instr numbering dest number)
        in
        let instrs = instr :: instrs in
        let%bind () =
          match to_clobber with
          | Some old_dest_number -> Numbering.Stateful.clobber old_dest_number
          | None -> return ()
        in
        eprintf "\n";
        return instrs)
      |> Numbering.With_state.run ~state:Numbering.empty
    in
    rev_instrs |> List.rev
  in
  let instrs =
    List.map fn.order ~f:(fun label -> run_block (Map.find_exn fn.blocks label))
    |> List.concat
  in
  Bril.Func.set_instrs fn instrs
;;
