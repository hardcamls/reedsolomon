open Core
module Harness = Test_reedsolomon.Harness

module Encode_file = struct
  let encoder (params : Reedsolomon.Iter_codec.params) =
    let encoder = Harness.Iter_codec.init params in
    Harness.Iter_codec.encode encoder
  ;;

  (* 
 {v
    let module Gp = struct
      let pp = prim_poly
      let pe = prim_elt
    end
    in
    let module Rp = struct
      let k = k
      let t = t
      let b = b
    end
    in
    let module Standard = Reedsolomon.Standards.Make (Gp) (Rp) in
 v}  
*)
  let run (params : Reedsolomon.Iter_codec.params) in_file out_file =
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
        ~encoder:(encoder params)
    in
    Harness.Encode.encode harness;
    In_channel.close in_file;
    Out_channel.close out_file
  ;;

  let command =
    Command.basic
      ~summary:"RS encode a file"
      [%map_open.Command
        let args = Codec_args.args
        and in_file = anon ("IN_FILE" %: string)
        and out_file = anon ("OUT_FILE" %: string) in
        fun () -> run args in_file out_file]
  ;;
end

module Decode_file = struct
  let decoder params =
    let decoder = Harness.Iter_codec.init params in
    Harness.Iter_codec.decode decoder
  ;;

  let run _parallelism in_file out_file =
    let in_file = In_channel.create in_file in
    let out_file = Out_channel.create out_file in
    let get_byte () =
      In_channel.input_byte in_file |> Option.value_exn |> Char.of_int_exn
    in
    let put_byte c = Out_channel.output_byte out_file (Char.to_int c) in
    let harness =
      Harness.Decode.create (Harness.Io_buffers.create ~get_byte ~put_byte) ~decoder
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
        and in_file = anon ("IN_FILE" %: string)
        and out_file = anon ("OUT_FILE" %: string) in
        fun () -> run parallelism in_file out_file]
  ;;
end

let command =
  Command.group
    ~summary:"Encode and decode from files using hardware simulation"
    [ "encode", Encode_file.command; "decode", Decode_file.command ]
;;
