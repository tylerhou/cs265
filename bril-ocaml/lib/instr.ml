open! Core
open! Common

type label = string [@@deriving compare, equal, sexp_of]
type arg = string [@@deriving compare, equal, sexp_of]
type header = Label of label * Dest.t list [@@deriving compare, equal, sexp_of]

type body =
  | Const of Dest.t * Const.t
  | Binary of Dest.t * Op.Binary.t * arg * arg
  | Unary of Dest.t * Op.Unary.t * arg
  | Call of Dest.t option * string * arg list
  | Print of arg list
  | Nop
  | Phi of Dest.t * (label * arg) list
  | Speculate
  | Commit
  | Guard of arg * (label * arg list)
  | Alloc of (Dest.t * arg)
  | Free of arg
  | Store of (arg * arg)
  | Load of (Dest.t * arg)
  | PtrAdd of (Dest.t * arg * arg)
[@@deriving compare, equal, sexp_of]

type terminator =
  | Jmp of label * arg list
  | Br of arg * (label * arg list) * (label * arg list)
  | Ret of arg option
[@@deriving compare, equal, sexp_of]

type t =
  | Header of header
  | Body of body
  | Terminator of terminator
[@@deriving compare, equal, sexp_of]

let to_string =
  let dest_to_string (name, bril_type) = sprintf "%s: %s =" name (Bril_type.to_string bril_type) in
  let block_args_to_string = function
    | [] -> ""
    | nonempty -> sprintf "(%s)" (String.concat ~sep:" " nonempty)
  in
  function
  | Header (Label (label, args)) ->
    (match args with
    | [] -> sprintf ".%s" label
    | _ ->
      sprintf
        ".%s(%s)"
        label
        (args
        |> List.map ~f:(fun (arg, bril_type) ->
          sprintf "%s: %s" arg (Bril_type.to_string bril_type))
        |> String.concat ~sep:", "))
  | Body instr ->
    (match instr with
    | Const (dest, const) -> sprintf "%s const %s" (dest_to_string dest) (Const.to_string const)
    | Binary (dest, op, arg1, arg2) ->
      sprintf "%s %s %s %s" (dest_to_string dest) (Op.Binary.to_string op) arg1 arg2
    | Unary (dest, op, arg) -> sprintf "%s %s %s" (dest_to_string dest) (Op.Unary.to_string op) arg
    | Call (dest, func_name, args) ->
      List.filter
        ([ Option.value_map dest ~default:"" ~f:dest_to_string; func_name ] @ args)
        ~f:(Fn.non String.is_empty)
      |> String.concat ~sep:" "
    | Print args -> String.concat ~sep:" " ("print" :: args)
    | Nop -> "nop"
    | Phi (dest, alist) ->
      sprintf
        "%s phi %s"
        (dest_to_string dest)
        (List.map alist ~f:(fun (label, arg) -> sprintf ".%s %s" label arg)
        |> String.concat ~sep:" ")
    | Speculate -> "speculate"
    | Commit -> "commit"
    | Guard (arg, (name, args)) -> sprintf "guard %s .%s%s" arg name (block_args_to_string args)
    | Alloc (dst, arg) -> sprintf "%s alloc %s" (dest_to_string dst) arg
    | Store (arg1, arg2) -> sprintf "store %s %s" arg1 arg2
    | Load (dst, arg) -> sprintf "%s load %s" (dest_to_string dst) arg
    | PtrAdd (dst, arg1, arg2) -> sprintf "%s ptradd %s %s" (dest_to_string dst) arg1 arg2
    | Free arg -> sprintf "free %s" arg)
  | Terminator instr ->
    (match instr with
    | Jmp (label, args) -> sprintf "jmp .%s%s" label (block_args_to_string args)
    | Br (arg, (l1, a1), (l2, a2)) ->
      sprintf "br %s .%s%s .%s%s" arg l1 (block_args_to_string a1) l2 (block_args_to_string a2)
    | Ret arg ->
      (match arg with
      | Some arg -> sprintf "ret %s" arg
      | None -> "ret"))

let dest = function
  | Header (Label _) -> None
  | Body instr ->
    (match instr with
    | Const (dest, _)
    | Binary (dest, _, _, _)
    | Unary (dest, _, _)
    | Phi (dest, _)
    | Alloc (dest, _)
    | PtrAdd (dest, _, _)
    | Load (dest, _) ->
      Some dest
    | Call (dest, _, _) -> dest
    | Nop
    | Speculate
    | Commit
    | Print _
    | Guard (_, _)
    | Free _
    | Store _ ->
      None)
  | Terminator instr ->
    (match instr with
    | Jmp _
    | Br (_, _, _)
    | Ret _ ->
      None)

let set_dest dest t =
  match (t, dest) with
  | (Const (_, const), Some dest) -> Const (dest, const)
  | (Binary (_, op, arg1, arg2), Some dest) -> Binary (dest, op, arg1, arg2)
  | (Unary (_, op, arg), Some dest) -> Unary (dest, op, arg)
  | (Call (_, f, args), dest) -> Call (dest, f, args)
  | (Phi (_, params), Some dest) -> Phi (dest, params)
  | (Alloc (_, arg), Some dest) -> Alloc (dest, arg)
  | (Load (_, arg), Some dest) -> Load (dest, arg)
  | (PtrAdd (_, a1, a2), Some dest) -> PtrAdd (dest, a1, a2)
  | (instr, None) -> instr
  | _ ->
    let dest = [%sexp_of: Dest.t Option.t] dest in
    failwithf !"Cannot [set_dest] on instruction %{to_string} with dest %{Sexp}" (Body t) dest ()

let args = function
  | Body (Binary (_, _, arg1, arg2)) -> [ arg1; arg2 ]
  | Body (Unary (_, _, arg))
  | Terminator (Br (arg, _, _))
  | Body (Guard (arg, _)) ->
    [ arg ]
  | Body (Call (_, _, args))
  | Body (Print args) ->
    args
  | Body (Alloc ((_ : Dest.t), arg)) -> [ arg ]
  | Body (Free arg) -> [ arg ]
  | Body (Store (arg1, arg2)) -> [ arg1; arg2 ]
  | Body (Load ((_ : Dest.t), arg)) -> [ arg ]
  | Body (PtrAdd ((_ : Dest.t), arg1, arg2)) -> [ arg1; arg2 ]
  | Terminator (Ret arg) -> Option.value_map arg ~default:[] ~f:List.return
  | Body (Phi ((_ : Dest.t), label_and_args)) -> List.map label_and_args ~f:snd
  | ( Body Nop
    | Body Speculate
    | Body Commit
    | Header (Label _)
    | Body (Const (_, _))
    | Terminator (Jmp _) ) as instr ->
    failwithf "Cannot call [args] on %s" (to_string instr) ()

let set_args args t =
  match t with
  | Body instr ->
    Body
      (match (instr, args) with
      | (Binary (dest, op, _, _), [ arg1; arg2 ]) -> Binary (dest, op, arg1, arg2)
      | (Unary (dest, op, _), [ arg ]) -> Unary (dest, op, arg)
      | (Call (dest, f, _), args) -> Call (dest, f, args)
      | (Print _, args) -> Print args
      | (Guard (_, l), [ arg ]) -> Guard (arg, l)
      | (Alloc (dst, _), [ arg ]) -> Alloc (dst, arg)
      | (Free _, [ arg ]) -> Free arg
      | (Store (_, _), [ a1; a2 ]) -> Store (a1, a2)
      | (Load (dst, _), [ a ]) -> Load (dst, a)
      | (PtrAdd (dst, _, _), [ a1; a2 ]) -> PtrAdd (dst, a1, a2)
      | (instr, []) -> instr
      | _ -> failwith "invalid set_args")
  | Terminator instr ->
    Terminator
      (match (instr, args) with
      | (Br (_, l1, l2), [ arg ]) -> Br (arg, l1, l2)
      | (Ret _, []) -> Ret None
      | (Ret _, [ arg ]) -> Ret (Some arg)
      | (instr, []) -> instr
      | _ -> failwith "invalid set_args")
  | _ ->
    (match args with
    | [] -> t
    | _ -> failwith "invalid set_args")

let of_json json : t =
  let open Yojson.Basic.Util in
  match json |> member "label" with
  | `String label ->
    let args =
      match member "args" json with
      | `Null -> []
      | `List args ->
        List.map args ~f:(fun arg ->
          (arg |> member "name" |> to_string, arg |> member "type" |> Bril_type.of_json))
      | _ -> failwith "invalid args"
    in
    Header (Label (label, args))
  | `Null ->
    let dest () =
      (json |> member "dest" |> to_string, json |> member "type" |> Bril_type.of_json)
    in
    let args () = json |> member "args" |> to_list_nonnull |> List.map ~f:to_string in
    let labels () =
      json
      |> member "labels"
      |> to_list_nonnull
      |> List.map ~f:(fun label ->
        ( member "name" label |> to_string,
          match member "args" label with
          | `Null -> []
          | `List lst -> List.map ~f:to_string lst
          | _ -> failwith "wrong type for label args" ))
    in
    let arg = List.nth_exn (args ()) in
    let label = List.nth_exn (labels ()) in
    (match json |> member "op" |> to_string with
    | "const" ->
      let const =
        match json |> member "type" |> Bril_type.of_json with
        | IntType -> Const.Int (json |> member "value" |> to_int)
        | BoolType -> Const.Bool (json |> member "value" |> to_bool)
        | PtrType _ -> failwith "pointer is not supported in constants"
      in
      Body (Const (dest (), const))
    | op when Op.Binary.is_op op -> Body (Binary (dest (), Op.Binary.of_string op, arg 0, arg 1))
    | op when Op.Unary.is_op op -> Body (Unary (dest (), Op.Unary.of_string op, arg 0))
    | "jmp" ->
      let (name, args) = label 0 in
      Terminator (Jmp (name, args))
    | "br" -> Terminator (Br (arg 0, label 0, label 1))
    | "call" ->
      Body
        (Call
           ( (if has_key json "dest" then Some (dest ()) else None),
             json |> member "funcs" |> to_list_nonnull |> List.hd_exn |> to_string,
             args () ))
    | "ret" -> Terminator (Ret (if List.is_empty (args ()) then None else Some (arg 0)))
    | "print" -> Body (Print (args ()))
    | "nop" -> Body Nop
    | "phi" ->
      Body
        (Phi
           ( dest (),
             List.zip_exn
               (labels ()
               |> List.map ~f:(fun (name, arg) ->
                 if not (List.is_empty arg) then failwith "phi labels must have no arguments"
                 else name))
               (args ()) ))
    | "speculate" -> Body Speculate
    | "commit" -> Body Commit
    | "guard" -> Body (Guard (arg 0, label 0))
    | "alloc" -> Body (Alloc (dest (), arg 0))
    | "free" -> Body (Free (arg 0))
    | "store" -> Body (Store (arg 0, arg 1))
    | "load" -> Body (Load (dest (), arg 0))
    | "ptradd" -> Body (PtrAdd (dest (), arg 0, arg 1))
    | op -> failwithf "invalid op: %s" op ())
  | json -> failwithf "invalid label: %s" (json |> to_string) ()

let to_json =
  let dest_to_json (name, bril_type) =
    [ ("dest", `String name); ("type", Bril_type.to_json bril_type) ]
  in
  let build_op ?dest ?args ~op () =
    `Assoc
      ([ ("op", `String op) ]
      @ (match args with
        | None -> []
        | Some args -> [ ("args", `List (List.map args ~f:(fun a -> `String a))) ])
      @
      match dest with
      | None -> []
      | Some dest -> dest_to_json dest)
  in
  let block_args_to_json name args =
    `Assoc [ ("name", `String name); ("args", `List (List.map args ~f:(fun a -> `String a))) ]
  in
  function
  | Header (Label (label, args)) ->
    `Assoc
      [
        ("label", `String label);
        ( "args",
          `List
            (List.map args ~f:(fun (arg, bril_type) ->
               `Assoc [ ("name", `String arg); ("type", Bril_type.to_json bril_type) ])) );
      ]
  | Body (Const (dest, const)) ->
    `Assoc
      ([
         ("op", `String "const");
         ( "value",
           match const with
           | Int i -> `Int i
           | Bool b -> `Bool b );
       ]
      @ dest_to_json dest)
  | Body (Binary (dest, op, arg1, arg2)) ->
    `Assoc
      ([ ("op", `String (Op.Binary.to_string op)); ("args", `List [ `String arg1; `String arg2 ]) ]
      @ dest_to_json dest)
  | Body (Unary (dest, op, arg)) ->
    `Assoc
      ([ ("op", `String (Op.Unary.to_string op)); ("args", `List [ `String arg ]) ]
      @ dest_to_json dest)
  | Body (Call (dest, func_name, args)) ->
    `Assoc
      ([
         ("op", `String "call");
         ("funcs", `List [ `String func_name ]);
         ("args", `List (List.map args ~f:(fun arg -> `String arg)));
       ]
      @ Option.value_map dest ~default:[] ~f:dest_to_json)
  | Body (Print args) ->
    `Assoc [ ("op", `String "print"); ("args", `List (List.map args ~f:(fun arg -> `String arg))) ]
  | Body Nop -> `Assoc [ ("op", `String "nop") ]
  | Body (Phi (dest, alist)) ->
    `Assoc
      ([
         ("op", `String "phi");
         ("labels", `List (List.map alist ~f:(fun (label, _) -> `String label)));
         ("args", `List (List.map alist ~f:(fun (_, arg) -> `String arg)));
       ]
      @ dest_to_json dest)
  | Body Speculate -> `Assoc [ ("op", `String "speculate") ]
  | Body Commit -> `Assoc [ ("op", `String "commit") ]
  | Body (Guard (arg, (name, args))) ->
    `Assoc
      [
        ("op", `String "guard");
        ("args", `List [ `String arg ]);
        ("labels", `List [ block_args_to_json name args ]);
      ]
  | Body (Alloc (dest, arg)) -> build_op ~op:"alloc" ~args:[ arg ] ~dest ()
  | Body (Free arg) -> build_op ~op:"free" ~args:[ arg ] ()
  | Body (Load (dest, arg)) -> build_op ~op:"load" ~args:[ arg ] ~dest ()
  | Body (Store (arg1, arg2)) -> build_op ~op:"store" ~args:[ arg1; arg2 ] ()
  | Body (PtrAdd (dest, arg1, arg2)) -> build_op ~op:"ptradd" ~args:[ arg1; arg2 ] ~dest ()
  | Terminator (Jmp (name, args)) ->
    `Assoc [ ("op", `String "jmp"); ("labels", `List [ block_args_to_json name args ]) ]
  | Terminator (Br (arg, (n1, args1), (n2, args2))) ->
    `Assoc
      [
        ("op", `String "br");
        ("args", `List [ `String arg ]);
        ("labels", `List [ block_args_to_json n1 args1; block_args_to_json n2 args2 ]);
      ]
  | Terminator (Ret arg) ->
    `Assoc
      [
        ("op", `String "ret");
        ("args", Option.value_map arg ~default:(`List []) ~f:(fun arg -> `List [ `String arg ]));
      ]
