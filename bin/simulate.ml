open Core

let command_encoder =
  Command.basic
    ~summary:"Test encoder"
    [%map_open.Command
      let params = Codec_args.args
      and seed = flag "-seed" (optional_with_default 1 int) ~doc:"SEED random seed"
      and waves = flag "-waves" no_arg ~doc:"Show waveform"
      and verbose = flag "-verbose" no_arg ~doc:"Print messages"
      and num_tests =
        flag
          "-num-tests"
          (optional_with_default 1 int)
          ~doc:"COUNT Number of tests to run"
      in
      fun () ->
        Random.init seed;
        let module Harness = Test_hardcaml_reedsolomon.Harness in
        let module Util =
          Test_hardcaml_reedsolomon.Util.Make
            ((val Reedsolomon.Iter_codec.to_standard params))
        in
        let module Codec =
          Harness.Hardware_codec (struct
            let n = 1
            let waves = waves
          end)
        in
        let codec = Codec.init params in
        for _ = 1 to num_tests do
          let message = Util.message () in
          let codeword_tb = Codec.encode codec message in
          let codeword_sw = Util.codeword (Array.rev message) |> Array.rev in
          let msg () =
            [%message
              (message : int array) (codeword_tb : int array) (codeword_sw : int array)]
          in
          if verbose
          then print_s (msg ())
          else if not ([%compare.equal: int array] codeword_tb codeword_sw)
          then raise_s (msg ())
        done;
        Option.iter (Codec.encoder_waves codec) ~f:Hardcaml_waveterm_interactive.run]
;;

let command_syndromes =
  Command.basic
    ~summary:"Test syndrome calculation"
    [%map_open.Command
      let seed = flag "-seed" (optional_with_default 1 int) ~doc:"SEED random seed"
      and waves = flag "-waves" no_arg ~doc:"Show waveform"
      and parallelism =
        flag
          "-parallelism"
          (optional_with_default 1 int)
          ~doc:"PARALLELISM Code word parallelism - symbols processed per cycle"
      in
      fun () ->
        Random.init seed;
        let waves = Test_hardcaml_reedsolomon.Test_syndromes.test ~waves parallelism in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_chien =
  Command.basic
    ~summary:"Test chien search calculation"
    [%map_open.Command
      let seed = flag "-seed" (optional_with_default 1 int) ~doc:"SEED random seed"
      and waves = flag "-waves" no_arg ~doc:"Show waveform"
      and parallelism =
        flag
          "-parallelism"
          (optional_with_default 1 int)
          ~doc:"PARALLELISM Code word parallelism - symbols processed per cycle"
      in
      fun () ->
        Random.init seed;
        let waves = Test_hardcaml_reedsolomon.Test_chien.test ~waves parallelism in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_forney =
  Command.basic
    ~summary:"Test forney calculation"
    [%map_open.Command
      let seed = flag "-seed" (optional_with_default 1 int) ~doc:"SEED random seed"
      and waves = flag "-waves" no_arg ~doc:"Show waveform"
      and parallelism =
        flag
          "-parallelism"
          (optional_with_default 1 int)
          ~doc:"PARALLELISM Code word parallelism - symbols processed per cycle"
      in
      fun () ->
        Random.init seed;
        let waves = Test_hardcaml_reedsolomon.Test_forney.test ~waves parallelism in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_berlekamp =
  Command.basic
    ~summary:"Test berlekamp calculation"
    [%map_open.Command
      let seed = flag "-seed" (optional_with_default 1 int) ~doc:"SEED random seed"
      and waves = flag "-waves" no_arg ~doc:"Show waveform"
      and num_errors =
        flag "-num-errors" (optional_with_default 1 int) ~doc:"ERRORS number of errors"
      in
      fun () ->
        Random.init seed;
        let waves = Test_hardcaml_reedsolomon.Test_berlekamp.test ~waves num_errors in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_decoder =
  Command.basic
    ~summary:"Test reed-solomon decoder"
    [%map_open.Command
      let params = Codec_args.args
      and seed = flag "-seed" (optional int) ~doc:"SEED random seed"
      and waves = flag "-waves" no_arg ~doc:"Show waveform"
      and verbose = flag "-verbose" no_arg ~doc:"Print codewords"
      and errors =
        flag "-errors" (optional int) ~doc:"Number of errors to insert (default t)"
      and parallelism =
        flag
          "-parallelism"
          (optional_with_default 1 int)
          ~doc:"PARALLELISM Code word parallelism - symbols processed per cycle"
      and num_tests =
        flag
          "-num-tests"
          (optional_with_default 1 int)
          ~doc:"COUNT Number of tests to run"
      in
      fun () ->
        Option.iter seed ~f:Random.init;
        let module Harness = Test_hardcaml_reedsolomon.Harness in
        let module Util =
          Test_hardcaml_reedsolomon.Util.Make
            ((val Reedsolomon.Iter_codec.to_standard params))
        in
        let module Codec =
          Harness.Hardware_codec (struct
            let n = parallelism
            let waves = waves
          end)
        in
        let codec = Codec.init params in
        for _ = 1 to num_tests do
          let codeword = Array.rev (Util.codeword (Array.rev (Util.message ()))) in
          let error = Util.error (Option.value ~default:params.t errors) in
          let received = Util.( ^. ) codeword error in
          let decoded = Codec.decode codec received in
          let msg () =
            [%message
              (codeword : int array)
                (error : int array)
                (received : int array)
                (decoded : int array)]
          in
          if verbose
          then print_s (msg ())
          else if not ([%compare.equal: int array] codeword decoded)
          then raise_s (msg ())
        done;
        Option.iter (Codec.decoder_waves codec) ~f:Hardcaml_waveterm_interactive.run]
;;

let command =
  Command.group
    ~summary:"RS codec simulation testbenches"
    [ "encoder", command_encoder
    ; "decoder", command_decoder
    ; "syndromes", command_syndromes
    ; "chien-search", command_chien
    ; "berlekamp", command_berlekamp
    ; "forney", command_forney
    ]
;;
