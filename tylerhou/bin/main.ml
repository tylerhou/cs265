open! Core
open! Opt

let () =
  In_channel.input_all In_channel.stdin
  |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.map ~f:Valnum.run
  |> Bril.to_json
  |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout
;;
