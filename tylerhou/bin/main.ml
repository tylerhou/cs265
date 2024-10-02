open! Core
open! Opt

let command =
  Command.basic
    ~summary:"Run optimizations on Bril JSON"
    (let%map_open.Command optimizations =
       anon (non_empty_sequence_as_list ("pass" %: string))
     in
     fun () ->
       let optimizations =
         List.map optimizations ~f:(fun opt ->
           match opt with
           | "valnum" -> Valnum.run
           | "dce" -> Dce.run
           | "constprop" -> Const_prop.run
           | other ->
             eprint_s [%message "no such optimization" (other : string)];
             failwith "no such optimization")
       in
       let rec fixpoint_opt before =
         let after = List.fold optimizations ~init:before ~f:(fun bril opt -> opt bril) in
         if Bril.Func.equal before after then after else fixpoint_opt after
       in
       In_channel.input_all In_channel.stdin
       |> Yojson.Basic.from_string
       |> Bril.from_json
       |> List.map ~f:fixpoint_opt
       |> Bril.to_json
       |> Yojson.Basic.to_string
       |> Out_channel.output_string Out_channel.stdout)
;;

let () = Command_unix.run ~version:"0.1" command
