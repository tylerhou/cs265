open! Core
open! Common

module Block = struct
  type t = {
    header : Instr.header;
    body : Instr.body list;
    terminator : Instr.terminator;
  }
  [@@deriving compare, equal, sexp_of]

  let instrs ({ header; body; terminator } : t) : Instr.t list =
    [ Instr.Header header ]
    @ List.map body ~f:(fun b -> Instr.Body b)
    @ [ Instr.Terminator terminator ]
end

type t = {
  name : string;
  args : Dest.t list;
  ret_type : Bril_type.t option;
  blocks : Block.t String.Map.t;
  order : string list;
  preds : string list String.Map.t;
  succs : string list String.Map.t;
}
[@@deriving compare, equal, sexp_of]

let instrs { blocks; order; _ } =
  List.concat_map order ~f:(fun label -> Block.instrs (Map.find_exn blocks label))

let process_instrs instrs =
  let add_block ~header ~rev_body ~terminator ~blocks : Block.t list =
    let block_name i = sprintf "__b%d" i in
    let next_block : Block.t option =
      match header with
      | None ->
        if List.is_empty rev_body then None
        else
          let header : Instr.header = Label (block_name (List.length blocks + 1), []) in
          Some { header; body = List.rev rev_body; terminator }
      | Some header -> Some { header; body = List.rev rev_body; terminator }
    in
    match next_block with
    | Some block -> block :: blocks
    | None -> blocks
  in
  let (header, rev_body, blocks) =
    List.fold
      instrs
      ~init:((None, [], []) : Instr.header option * Instr.body list * Block.t list)
      ~f:(fun (header, rev_body, blocks) (instr : Instr.t) ->
        match instr with
        | Header (Label (label, _) as next_header) ->
          let terminator : Instr.terminator = Jmp (label, []) in
          (Some next_header, [], add_block ~header ~rev_body ~terminator ~blocks)
        | Terminator terminator -> (None, [], add_block ~header ~rev_body ~terminator ~blocks)
        | Body body_instr -> (header, body_instr :: rev_body, blocks))
  in
  let blocks =
    (if List.is_empty rev_body then blocks
     else add_block ~header ~rev_body ~terminator:(Ret None) ~blocks)
    |> List.rev
  in
  let order = List.map blocks ~f:(fun { header = Label (name, _); _ } -> name) in
  let succs =
    List.map blocks ~f:(fun { header = Label (name, _); terminator; _ } ->
      let next =
        match terminator with
        | Jmp (label, _) -> [ label ]
        | Br (_, (l1, _), (l2, _)) -> [ l1; l2 ]
        | Ret _ -> []
      in
      (name, next))
    |> String.Map.of_alist_exn
  in
  let preds =
    Map.fold
      succs
      ~init:(List.map order ~f:(fun name -> (name, [])) |> String.Map.of_alist_exn)
      ~f:(fun ~key:name ~data:succs preds ->
        List.fold succs ~init:preds ~f:(fun preds succ -> Map.add_multi preds ~key:succ ~data:name))
  in
  let blocks_with_label_name : (string * Block.t) list =
    List.map blocks ~f:(fun ({ header = Label (name, _); _ } as block) -> (name, block))
  in
  (String.Map.of_alist_exn blocks_with_label_name, order, preds, succs)

let set_instrs t instrs =
  let (blocks, order, preds, succs) = process_instrs instrs in
  { t with blocks; order; preds; succs }

let of_json json =
  let open Yojson.Basic.Util in
  let arg_of_json json =
    (json |> member "name" |> to_string, json |> member "type" |> Bril_type.of_json)
  in
  let name = json |> member "name" |> to_string in
  let args = json |> member "args" |> to_list_nonnull |> List.map ~f:arg_of_json in
  let ret_type = json |> member "type" |> Bril_type.of_json_opt in
  let instrs = json |> member "instrs" |> to_list_nonnull |> List.map ~f:Instr.of_json in
  let (blocks, order, preds, succs) = process_instrs instrs in
  { name; args; ret_type; blocks; order; preds; succs }

let to_json t =
  `Assoc
    ([
       ("name", `String t.name);
       ( "args",
         `List
           (List.map t.args ~f:(fun (name, bril_type) ->
              `Assoc [ ("name", `String name); ("type", Bril_type.to_json bril_type) ])) );
       ("instrs", `List (instrs t |> List.map ~f:Instr.to_json));
     ]
    @ Option.value_map t.ret_type ~default:[] ~f:(fun t -> [ ("type", Bril_type.to_json t) ]))

let to_string { name; args; ret_type; blocks; order; _ } =
  let header =
    sprintf
      "@%s%s%s {"
      name
      (match args with
      | [] -> ""
      | args ->
        sprintf
          "(%s)"
          (List.map args ~f:(fun (name, bril_type) ->
             sprintf "%s: %s" name (Bril_type.to_string bril_type))
          |> String.concat ~sep:", "))
      (Option.value_map ret_type ~default:"" ~f:Bril_type.to_string)
  in
  let body =
    order
    |> List.concat_map ~f:(fun label -> label |> Map.find_exn blocks |> Block.instrs)
    |> List.map ~f:(fun (instr : Instr.t) ->
      sprintf
        (match instr with
        | Header (Label _) -> "%s:"
        | _ -> "  %s;")
        (Instr.to_string instr))
    |> String.concat ~sep:"\n"
  in
  sprintf "%s\n%s\n}" header body

module Dominance = struct
  module type S = sig
    type out

    val dominators : ?strict:bool -> t -> out String.Map.t
    val dominated : ?strict:bool -> t -> out String.Map.t
    val tree : t -> out String.Map.t * out String.Map.t
    val frontier : t -> out String.Map.t
    val back_edges : t -> out String.Map.t
  end

  module Sets : S with type out := String.Set.t = struct
    let preds_to_succs preds =
      Map.fold
        preds
        ~init:(Map.map preds ~f:(const String.Set.empty))
        ~f:(fun ~key:vertex ~data:vertices succs ->
          Set.fold vertices ~init:succs ~f:(fun succs pred ->
            Map.update succs pred ~f:(function
              | None -> String.Set.singleton vertex
              | Some vertices -> Set.add vertices vertex)))

    let dominators ?(strict = false) { order; preds; succs; _ } =
      let rec postorder (visited, vertices) vertex =
        if Set.mem visited vertex then (visited, vertices)
        else
          let (visited, vertices) =
            Map.find_exn succs vertex
            |> List.fold ~init:(Set.add visited vertex, vertices) ~f:postorder
          in
          (visited, vertices @ [ vertex ])
      in
      let (_, vertices) = postorder (String.Set.empty, []) (List.hd_exn order) in
      let rec compute dom =
        let new_dom =
          List.fold vertices ~init:dom ~f:(fun dom vertex ->
            if String.equal vertex (List.hd_exn order) then dom
            else
              let inter =
                Map.find_exn preds vertex
                |> List.map ~f:(Map.find_exn dom)
                |> List.reduce ~f:Set.inter
                |> Option.value ~default:String.Set.empty
              in
              Map.set dom ~key:vertex ~data:(Set.add inter vertex))
        in
        if String.Map.equal String.Set.equal dom new_dom then dom else compute new_dom
      in
      let init =
        List.mapi order ~f:(fun i vertex ->
          (vertex, if i = 0 then String.Set.singleton vertex else String.Set.of_list vertices))
        |> String.Map.of_alist_exn
      in
      compute init |> if strict then Map.mapi ~f:(fun ~key ~data -> Set.remove data key) else Fn.id

    let dominated ?strict = Fn.compose preds_to_succs (dominators ?strict)

    let tree func =
      let dominators = dominators ~strict:true func in
      let preds =
        Map.map dominators ~f:(fun doms ->
          Set.fold doms ~init:doms ~f:(fun doms dom -> Set.diff doms (Map.find_exn dominators dom)))
      in
      (preds, preds_to_succs preds)

    let frontier func =
      let dominated = dominated func in
      Map.map dominated ~f:(fun dominated ->
        Set.fold dominated ~init:String.Set.empty ~f:(fun frontier vertex ->
          Map.find_exn func.succs vertex
          |> List.filter ~f:(Fn.non (Set.mem dominated))
          |> String.Set.of_list
          |> Set.union frontier))

    let back_edges func =
      let dominators = dominators func in
      Map.mapi func.succs ~f:(fun ~key ~data ->
        List.filter data ~f:(fun succ -> Set.mem (Map.find_exn dominators key) succ)
        |> String.Set.of_list)
  end

  module Lists : S with type out := string list = struct
    let f = Map.map ~f:Set.to_list
    let dominators ?strict = Fn.compose f (Sets.dominators ?strict)
    let dominated ?strict = Fn.compose f (Sets.dominated ?strict)

    let tree func =
      let (preds, succs) = Sets.tree func in
      (f preds, f succs)

    let frontier = Fn.compose f Sets.frontier
    let back_edges = Fn.compose f Sets.back_edges
  end
end
