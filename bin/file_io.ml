open Core
module Harness = Test_reedsolomon.Harness

module Encode_file = struct
  let encoder ~simulate (params : Reedsolomon.Iter_codec.params) =
    if simulate
    then
      let module Codec =
        Test_hardcaml_reedsolomon.Harness.Hardware_codec (struct
          let n = 1
          let waves = false
        end)
      in
      let encoder = Codec.init params in
      Codec.encode encoder
    else
      let module Codec = Harness.Iter_codec in
      let encoder = Codec.init params in
      Codec.encode encoder
  ;;

  let run (params : Reedsolomon.Iter_codec.params) ~simulate in_file out_file =
    let in_file = In_channel.create in_file in
    let out_file = Out_channel.create out_file in
    let total_length_in_bytes =
      Core_unix.((fstat (descr_of_in_channel in_file)).st_size |> Int64.to_int_exn)
    in
    let get_byte () =
      In_channel.input_byte in_file |> Option.value_exn |> Char.of_int_exn
    in
    let put_byte c = Out_channel.output_byte out_file (Char.to_int c) in
    let harness =
      Harness.Encode.create
        ~params
        (Harness.Io_buffers.create ~get_byte ~put_byte)
        ~total_length_in_bytes
        ~encoder:(encoder ~simulate params)
    in
    Harness.Encode.encode harness;
    In_channel.close in_file;
    Out_channel.close out_file
  ;;

  let command =
    Command.basic
      ~summary:"RS encode a file"
      [%map_open.Command
        let params = Codec_args.args
        and simulate =
          flag
            "-simulate"
            no_arg
            ~doc:"Run in simulation mode.  By default use the iterative codec."
        and in_file = anon ("IN_FILE" %: string)
        and out_file = anon ("OUT_FILE" %: string) in
        fun () -> run params ~simulate in_file out_file]
  ;;
end

module Decode_file = struct
  let decoder ~simulate ~parallelism params =
    if simulate
    then
      let module Codec =
        Test_hardcaml_reedsolomon.Harness.Hardware_codec (struct
          let n = parallelism
          let waves = false
        end)
      in
      let decoder = Codec.init params in
      fun x ->
        (* XXX figure out the data order among the various codecs.
           This is getting annoying. *)
        Codec.decode decoder (Array.rev x) |> Array.rev
    else
      let module Codec = Harness.Iter_codec in
      let decoder = Codec.init params in
      Codec.decode decoder
  ;;

  let run ~simulate ~parallelism in_file out_file =
    let in_file = In_channel.create in_file in
    let out_file = Out_channel.create out_file in
    let get_byte () =
      In_channel.input_byte in_file |> Option.value_exn |> Char.of_int_exn
    in
    let put_byte c = Out_channel.output_byte out_file (Char.to_int c) in
    let harness =
      Harness.Decode.create
        (Harness.Io_buffers.create ~get_byte ~put_byte)
        ~decoder:(decoder ~simulate ~parallelism)
    in
    Harness.Decode.decode harness;
    In_channel.close in_file;
    Out_channel.close out_file
  ;;

  let command =
    Command.basic
      ~summary:"RS decode a file"
      [%map_open.Command
        let parallelism =
          flag
            "-parallelism"
            (optional_with_default 1 int)
            ~doc:"PARALLISM code word parallelism"
        and simulate =
          flag
            "-simulate"
            no_arg
            ~doc:"Run in simulation mode.  By default use the iterative codec."
        and in_file = anon ("IN_FILE" %: string)
        and out_file = anon ("OUT_FILE" %: string) in
        fun () -> run ~simulate ~parallelism in_file out_file]
  ;;
end

let command =
  Command.group
    ~summary:"Encode and decode from files using hardware simulation"
    [ "encode", Encode_file.command; "decode", Decode_file.command ]
;;
