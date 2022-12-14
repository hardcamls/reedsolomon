open Core

let command_encoder =
  Command.basic
    ~summary:"Test encoder"
    [%map_open.Command
      let seed = flag "-seed" (optional_with_default 1 int) ~doc:"SEED random seed"
      and waves = flag "-waves" no_arg ~doc:"Show waveform"
      and num_tests =
        flag
          "-num-tests"
          (optional_with_default 1 int)
          ~doc:"COUNT Number of tests to run"
      in
      fun () ->
        Random.init seed;
        let waves = Test_hardcaml_reedsolomon.Test_encoder.test ~waves num_tests in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
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
        let waves = Test_hardcaml_reedsolomon.Test_decoder.test ~waves parallelism in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command =
  Command.group
    ~summary:"RS codec simulation tests"
    [ "encoder", command_encoder
    ; "decoder", command_decoder
    ; "syndromes", command_syndromes
    ; "chien-search", command_chien
    ; "berlekamp", command_berlekamp
    ; "forney", command_forney
    ]
;;
