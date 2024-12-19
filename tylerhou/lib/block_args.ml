open! Core

let run (func : Bril.Func.t) : Bril.Func.t =
  let assignment_counts_by_var : (int Var.Map.t, exn) Result.t =
    Result.try_with (fun () ->
      List.fold func.order ~init:Var.Map.empty ~f:(fun counts label ->
        let block = Map.find_exn func.blocks label in
        let (Label (_, params)) = block.header in
        if not (List.is_empty params)
        then
          Error.raise_s
            [%message
              "unhandled: program already uses block arguments"
                (block.header : Bril.Instr.header)]
        else
          List.fold block.body ~init:counts ~f:(fun counts (instr : Bril.Instr.body) ->
            match Bril.Instr.dest instr with
            | None -> counts
            | Some (var, _type) ->
              Map.update counts var ~f:(function
                | None -> 1
                | Some n -> n + 1))))
  in
  match assignment_counts_by_var with
  | Error _ -> func
  | Ok assignment_counts_by_var ->
    let single_assignment, multiple_assignments =
      Map.partition_tf assignment_counts_by_var ~f:(fun count -> count = 1)
    in
    let vars = Map.key_set assignment_counts_by_var
    and single_assignment : Var.Set.t = Map.key_set single_assignment
    and multiple_assignments : Var.Set.t = Map.key_set multiple_assignments in
    let params_by_block : Var.Set.t String.Map.t =
      func.order
      |> List.map ~f:(fun label ->
        let block = Map.find_exn func.blocks label in
        let params =
          block
          |> Bril.Func.Block.instrs
          |> List.fold ~init:Var.Set.empty ~f:(fun params instr ->
            match Bril.Instr.args instr with
            | None -> params
            | Some args ->
              args
              |> List.filter ~f:(Set.mem multiple_assignments)
              |> List.fold ~init:params ~f:Set.add)
        in
        label, params)
      |> String.Map.of_alist_exn
    in
    let pick_renamed vars original_by_renamed original =
      (* This is quadratic but who cares. *)
      let rec pick_var_impl vars original i =
        let candidate = Format.sprintf "%s%d" original i in
        if not (Set.mem vars candidate)
        then candidate
        else pick_var_impl vars original (i + 1)
      in
      let renamed = pick_var_impl vars original 0 in
      let vars = Set.add vars renamed in
      let original_by_renamed =
        Map.add_exn original_by_renamed ~key:renamed ~data:original
      in
      vars, original_by_renamed, renamed
    in
    (* Rename the destinations of all non-SSA variables. *)
    let _, original_by_renamed, params_by_block, rev_blocks_with_renamed_dests =
      List.fold
        func.order
        ~init:(vars, Var.Map.empty, params_by_block, [])
        ~f:(fun (vars, original_by_renamed, params_by_block, blocks) label ->
          let block = Map.find_exn func.blocks label in
          let vars, original_by_renamed, params =
            Map.find_exn params_by_block label
            |> Set.fold
                 ~init:(vars, original_by_renamed, Var.Set.empty)
                 ~f:(fun (vars, original_by_renamed, params) original ->
                   let vars, original_by_renamed, renamed =
                     pick_renamed vars original_by_renamed original
                   in
                   let params = Set.add params renamed in
                   vars, original_by_renamed, params)
          in
          let params_by_block = Map.set params_by_block ~key:label ~data:params in
          let (vars, original_by_renamed), renamed_dests =
            List.fold_map
              (Bril.Func.Block.instrs block)
              ~init:(vars, original_by_renamed)
              ~f:(fun (vars, original_by_renamed) instr ->
                match instr with
                | Header _ | Terminator _ -> (vars, original_by_renamed), instr
                | Body body ->
                  (match Bril.Instr.dest body with
                   | None -> (vars, original_by_renamed), instr
                   | Some (original, dest_type) ->
                     if not (Set.mem multiple_assignments original)
                     then (vars, original_by_renamed), instr
                     else (
                       let vars, original_by_renamed, renamed =
                         pick_renamed vars original_by_renamed original
                       in
                       ( (vars, original_by_renamed)
                       , Body (Bril.Instr.set_dest (Some (renamed, dest_type)) body) ))))
          in
          vars, original_by_renamed, params_by_block, (label, renamed_dests) :: blocks)
    in
    let blocks_with_renamed_dests = List.rev rev_blocks_with_renamed_dests in
    (* Pick a concrete ordering for each block's params. *)
    let params_by_block = Map.map params_by_block ~f:Set.to_list in
    (* For every block, compute reaching definitions: find which new renamed
       variable reaches the old unrenamed variable. Replace arguments with the
       renamed variables.

       Add block arguments for terminators. Keep track of the types of block
       arguments, so we can add block parameters.
    *)
    let param_types_by_block, blocks_with_renamed_dests_and_args =
      List.fold_map
        blocks_with_renamed_dests
        ~init:String.Map.empty
        ~f:(fun param_types_by_block (label, instrs) ->
          let reaching_by_original : Var.t Var.Map.t =
            Map.find_exn params_by_block label
            |> List.map ~f:(fun renamed ->
              let original = Map.find_exn original_by_renamed renamed in
              original, renamed)
            |> Var.Map.of_alist_exn
          in
          let (_, _, param_types_by_block), instrs_with_renamed_dests_and_args =
            List.fold_map
              instrs
              ~init:(reaching_by_original, Var.Map.empty, param_types_by_block)
              ~f:
                (fun
                  (reaching_by_original, type_by_renamed, param_types_by_block) instr ->
                let instr_with_renamed_args =
                  match Bril.Instr.args instr with
                  | None -> instr
                  | Some args ->
                    Bril.Instr.set_args
                      (List.map args ~f:(fun original ->
                         if Set.mem single_assignment original
                         then original
                         else Map.find_exn reaching_by_original original))
                      instr
                in
                let param_types_by_block, instr_with_renamed_args =
                  let make_block_args dest =
                    Map.find_exn params_by_block dest
                    |> List.map ~f:(fun renamed ->
                      let reaching =
                        renamed
                        |> Map.find_exn original_by_renamed
                        |> Map.find_exn reaching_by_original
                      in
                      reaching, reaching |> Map.find_exn type_by_renamed)
                    |> List.unzip
                  in
                  match instr_with_renamed_args with
                  | Terminator term ->
                    (match term with
                     | Jmp (label, _) ->
                       let args, param_types = make_block_args label in
                       ( param_types_by_block |> Map.add_multi ~key:label ~data:param_types
                       , Bril.Instr.Terminator (Jmp (label, args)) )
                     | Br (cond, (l1, _), (l2, _)) ->
                       let args1, param_types1 = make_block_args l1
                       and args2, param_types2 = make_block_args l2 in
                       ( param_types_by_block
                         |> Map.add_multi ~key:label ~data:param_types1
                         |> Map.add_multi ~key:label ~data:param_types2
                       , Terminator (Br (cond, (l1, args1), (l2, args2))) )
                     | Ret _ -> param_types_by_block, instr_with_renamed_args)
                  | _ -> param_types_by_block, instr_with_renamed_args
                in
                let reaching_by_original, type_by_renamed =
                  match instr with
                  | Header _ | Terminator _ -> reaching_by_original, type_by_renamed
                  | Body body ->
                    (match Bril.Instr.dest body with
                     | None -> reaching_by_original, type_by_renamed
                     | Some (maybe_renamed, dest_type) ->
                       if Set.mem single_assignment maybe_renamed
                       then reaching_by_original, type_by_renamed
                       else (
                         let renamed = maybe_renamed in
                         let original = Map.find_exn original_by_renamed renamed in
                         let reaching_by_original =
                           Map.set reaching_by_original ~key:original ~data:renamed
                         in
                         let type_by_renamed =
                           Map.set type_by_renamed ~key:renamed ~data:dest_type
                         in
                         reaching_by_original, type_by_renamed))
                in
                ( (reaching_by_original, type_by_renamed, param_types_by_block)
                , instr_with_renamed_args ))
          in
          param_types_by_block, (label, instrs_with_renamed_dests_and_args))
    in
    let param_types_by_block =
      Map.map param_types_by_block ~f:(fun (types : Bril.Bril_type.t list list) ->
        (* Each list is non-empty since we used add_multi. *)
        types
        |> List.all_equal ~equal:(List.equal Bril.Bril_type.equal)
        |> Option.value_exn)
    in
    let instructions =
      blocks_with_renamed_dests_and_args
      |> List.concat_map ~f:(fun (_, instrs) ->
        List.map instrs ~f:(fun instr : Bril.Instr.t ->
          match instr with
          | Header (Label (label, _)) ->
            (match Map.find param_types_by_block label with
             | None -> instr
             | Some param_types ->
               let param_names = Map.find_exn params_by_block label in
               let block_params = List.zip_exn param_names param_types in
               Header (Label (label, block_params)))
          | _ -> instr))
    in
    Bril.Func.set_instrs func instructions
;;
