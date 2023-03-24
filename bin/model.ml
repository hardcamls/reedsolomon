(* Reed-solomon test application. *)
open Core

type params = Reedsolomon.Iter_codec.params [@@deriving sexp_of]

include struct
  open Test_reedsolomon.Harness

  module type Codec = Codec

  module Iter_codec = Iter_codec
  module Poly_codec = Poly_codec

  let codec_selection = codec_selection
end

let find_codec name =
  match
    List.find codec_selection ~f:(fun (module Codec) -> String.equal name Codec.name)
  with
  | Some codec -> codec
  | None ->
    let codecs = List.map codec_selection ~f:(fun (module Codec) -> Codec.name) in
    raise_s [%message "Invalid codec" (name : string) (codecs : string list)]
;;

let codec_selection_arg = Command.Arg_type.create find_codec

let random_codes (params : params) n =
  Array.init n ~f:(fun _ -> Random.int (1 lsl params.m))
;;

let random_message (params : params) = random_codes params params.k

let random_errors (params : params) ~num_errors =
  let errors =
    Array.init params.n ~f:(fun i ->
        if i < num_errors then 1 + Random.int ((1 lsl params.m) - 1) else 0)
  in
  Array.permute errors;
  errors
;;

let command_message =
  Command.basic
    ~summary:"Generate a random message"
    [%map_open.Command
      let params = Codec_args.args
      and verbose = flag "-v" no_arg ~doc:"Display codec parameters" in
      fun () ->
        if verbose then print_s [%message (params : params)];
        Array.iter (random_message params) ~f:(printf "%i\n")]
;;

let command_encode =
  Command.basic
    ~summary:"Encode data (default configuration n=255, t=8)"
    [%map_open.Command
      let params = Codec_args.args
      and codec =
        flag
          "-codec"
          (optional_with_default (module Iter_codec : Codec) codec_selection_arg)
          ~doc:"CODEC select codec implementation (default iterative)"
      and verbose = flag "-v" no_arg ~doc:"Display codec parameters" in
      fun () ->
        let module Codec = (val codec : Codec) in
        if verbose then print_s [%message (params : params)];
        let codec = Codec.init params in
        let message =
          In_channel.input_lines In_channel.stdin
          |> List.map ~f:Int.of_string
          |> Array.of_list
        in
        if Array.length message <> params.k
        then
          raise_s
            [%message
              "Message length incorrect"
                ~expected:(params.k : int)
                ~got:(Array.length message : int)];
        let encoded = Codec.encode codec message in
        Array.iter encoded ~f:(printf "%i\n")]
;;

let command_errors =
  Command.basic
    ~summary:"Add errors"
    [%map_open.Command
      let params = Codec_args.args
      and num_errors =
        flag
          "-errors"
          (optional int)
          ~doc:"ERRORS add given number of errors to each code word"
      and verbose = flag "-v" no_arg ~doc:"Display codec parameters" in
      fun () ->
        if verbose then print_s [%message (params : params)];
        let num_errors = Option.value ~default:params.t num_errors in
        let rec read_block errors n d =
          if n = params.n
          then Some (List.rev d)
          else (
            match In_channel.input_line In_channel.stdin with
            | Some line ->
              let x = Int.of_string line lxor errors.(n) in
              read_block errors (n + 1) (x :: d)
            | None -> None)
        in
        let rec read_blocks () =
          match read_block (random_errors params ~num_errors) 0 [] with
          | None -> ()
          | Some x ->
            List.iter x ~f:(printf "%i\n");
            read_blocks ()
        in
        read_blocks ()]
;;

let command_decode =
  Command.basic
    ~summary:"Decode data (default configuration n=255, t=8)"
    [%map_open.Command
      let params = Codec_args.args
      and codec =
        flag
          "-codec"
          (optional_with_default (module Iter_codec : Codec) codec_selection_arg)
          ~doc:"CODEC select codec implementation (default iterative)"
      and verbose = flag "-v" no_arg ~doc:"Display codec parameters" in
      fun () ->
        let module Codec = (val codec : Codec) in
        if verbose then print_s [%message (params : params)];
        let codec = Codec.init params in
        let codeword =
          In_channel.input_lines In_channel.stdin
          |> List.map ~f:Int.of_string
          |> Array.of_list
        in
        if Array.length codeword <> params.n
        then
          raise_s
            [%message
              "Code word length incorrect"
                ~expected:(params.n : int)
                ~got:(Array.length codeword : int)];
        let decoded = Codec.decode codec codeword in
        Array.iter decoded ~f:(printf "%i\n")]
;;

type correctness_input =
  { message : int array
  ; errors : int array
  ; num_errors : int
  }

let create_correctness_input (params : params) =
  let num_errors = Random.int (params.t + 1) in
  { message = random_message params
  ; errors = random_errors params ~num_errors
  ; num_errors
  }
;;

let test_correctness ~params ~verbose ~codec { message; errors; num_errors } =
  let module Codec = (val codec : Codec) in
  if verbose then print_s [%message (params : params)];
  let codec = Codec.init params in
  let codeword = Codec.encode codec message in
  let corrupted_codeword = Array.map2_exn codeword errors ~f:( lxor ) in
  let decoded_message = Codec.decode codec corrupted_codeword in
  let diff = Array.map2_exn codeword decoded_message ~f:( lxor ) in
  let msg () =
    [%message
      "failed to decode message"
        (Codec.name : string)
        (num_errors : int)
        (codeword : int array)
        (decoded_message : int array)
        (errors : int array)
        (diff : int array)]
  in
  if verbose
  then print_s (msg ())
  else if not ([%compare.equal: int array] decoded_message codeword)
  then raise_s (msg ())
;;

let command_correctness =
  Command.basic
    ~summary:"Test correctness of a particular codec configuration"
    [%map_open.Command
      let params = Codec_args.args
      and verbose = flag "-v" no_arg ~doc:"Display codec parameters"
      and num_tests =
        flag "-num-tests" (optional_with_default 1 int) ~doc:"NUM number of test to run"
      and codec =
        flag
          "-codec"
          (optional_with_default (module Iter_codec : Codec) codec_selection_arg)
          ~doc:"CODEC select codec implementation (default iterative)"
      and seed = flag "-seed" (optional int) ~doc:"Random number generator seed" in
      fun () ->
        Option.iter seed ~f:Random.init;
        for _ = 1 to num_tests do
          test_correctness ~params ~verbose ~codec (create_correctness_input params)
        done]
;;

let params =
  let params n t b =
    let m = Int.ceil_log2 (n + 1) in
    { Reedsolomon.Iter_codec.m
    ; k = n - (2 * t)
    ; t
    ; n
    ; b
    ; prim_poly = Codec_args.default_poly m
    ; prim_elt = 2
    }
  in
  [ params 7 1 0
  ; params 7 2 1
  ; params 15 1 0
  ; params 15 2 3
  ; params 63 3 0
  ; params 63 4 9
  ; params 255 8 0
  ; params 255 16 33
  ]
;;

let command_regression =
  Command.basic
    ~summary:"Iterate over various CODEC parameters and check for correctness"
    [%map_open.Command
      let verbose = flag "-v" no_arg ~doc:"Display codec parameters"
      and num_tests =
        flag "-num-tests" (optional_with_default 1 int) ~doc:"NUM number of test to run"
      and codec =
        flag
          "-codec"
          (optional codec_selection_arg)
          ~doc:"CODEC select codec implementation to test (default all)"
      in
      fun () ->
        (* choose a codec, or test them all *)
        let codec_selection =
          Option.map codec ~f:(fun codec -> [ codec ])
          |> Option.value ~default:codec_selection
        in
        if verbose
        then (
          let codecs = List.map codec_selection ~f:(fun (module Codec) -> Codec.name) in
          print_s [%message (codecs : string list)]);
        List.iter params ~f:(fun params ->
            if verbose then print_s [%message (params : params)];
            let inputs = create_correctness_input params in
            List.iter codec_selection ~f:(fun codec ->
                for _ = 1 to num_tests do
                  test_correctness ~params ~verbose:false ~codec inputs
                done))]
;;

let command =
  Command.group
    ~summary:"Reedsolomon codec test application"
    [ "encode", command_encode
    ; "decode", command_decode
    ; "message", command_message
    ; "errors", command_errors
    ; "correctness", command_correctness
    ; "regression", command_regression
    ]
;;
