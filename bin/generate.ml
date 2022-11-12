open Core
open Hardcaml

let command_encoder =
  Command.basic
    ~summary:"Generate encoder"
    [%map_open.Command
      let codec_args = Codec_args.args in
      fun () ->
        let module Gp = struct
          let pp = codec_args.prim_poly
          let pe = codec_args.prim_elt
        end
        in
        let module Rp = struct
          let k = codec_args.k
          let t = codec_args.t
          let b = codec_args.b
        end
        in
        let module Hw = Hardcaml_reedsolomon.Codec.Make (Gp) (Rp) in
        let module Circuit = Hardcaml.Circuit.With_interface (Hw.Encoder.I) (Hw.Encoder.O)
        in
        Rtl.print Verilog (Circuit.create_exn ~name:"encoder" Hw.Encoder.create)]
;;

let command_decoder =
  Command.basic
    ~summary:"Generate decoder"
    [%map_open.Command
      let codec_args = Codec_args.args
      and parallelism =
        flag
          "-parallelism"
          (optional_with_default 1 int)
          ~doc:"PARALLELISM Code word parallelism - symbols processed per cycle"
      in
      fun () ->
        let module Gp = struct
          let pp = codec_args.prim_poly
          let pe = codec_args.prim_elt
        end
        in
        let module Rp = struct
          let k = codec_args.k
          let t = codec_args.t
          let b = codec_args.b
        end
        in
        let module N = struct
          let n = parallelism
        end
        in
        let module Hw = Hardcaml_reedsolomon.Codec.Make (Gp) (Rp) in
        let module Decoder = Hw.Decoder (N) in
        let module Circuit = Hardcaml.Circuit.With_interface (Decoder.I) (Decoder.O) in
        let scope = Scope.create ~flatten_design:false () in
        let circuit = Circuit.create_exn ~name:"rsdecoder" (Decoder.create scope) in
        Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit]
;;

let command =
  Command.group
    ~summary:"Verilog generation"
    [ "encoder", command_encoder; "decoder", command_decoder ]
;;
