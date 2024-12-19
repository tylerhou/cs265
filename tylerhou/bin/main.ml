open! Core
open! Opt

let command =
  Command.basic
    ~summary:"Run optimizations on Bril JSON"
    (let%map_open.Command optimizations =
       anon (non_empty_sequence_as_list ("pass" %: string))
     in
     fun () ->
       Random.self_init ();
       let optimization_fns =
         List.map optimizations ~f:(fun opt ->
           (* TODO: Rename these options to their module names *)
           match opt with
           | "dce" -> Dce.run
           | "cp" -> Cp.run
           | other ->
             eprint_s [%message "no such optimization" (other : string)];
             failwith "no such optimization")
       in
       let rec fixpoint_opt limit before =
         let after =
           List.fold optimization_fns ~init:before ~f:(fun bril opt -> opt bril)
         in
         if Bril.Func.equal before after || limit = 0 then after else fixpoint_opt (limit - 1) after
       in
       In_channel.input_all In_channel.stdin
       |> Yojson.Basic.from_string
       |> Bril.from_json
       |> List.map ~f:(fixpoint_opt 10)
       |> Bril.to_json
       |> Yojson.Basic.to_string
       |> Out_channel.output_string Out_channel.stdout)
;;

let () = Command_unix.run ~version:"0.1" command
