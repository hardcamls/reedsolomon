(* Reed-solomon test application. *)
open Core

module Codec_args = struct
  let default_poly idx =
    if idx < 2
    then raise_s [%message "number of bits must be > 1"]
    else
      Array.foldi
        Reedsolomon.Galois.GF2N.gf2_prim_polys.(idx)
        ~init:0
        ~f:(fun idx acc b -> acc + if b = 0 then 0 else b lsl idx)
  ;;

  let args =
    [%map_open.Command
      let n = flag "-n" (optional int) ~doc:"NUM_SYMBOLS number of symbols per codeword"
      and t = flag "-t" (optional int) ~doc:"PARITY_BYTES numbers of parity bytes"
      and b =
        flag
          "-b"
          (optional_with_default 0 int)
          ~doc:"INITAIL_ROOT starting root of generator"
      and prim_poly = flag "-poly" (optional int) ~doc:"POLY primitive polynomial"
      and prim_elt =
        flag "-elt" (optional_with_default 2 int) ~doc:"ELT primitive element"
      in
      let n = Option.value ~default:255 n in
      if not (Int.is_pow2 (n + 1))
      then
        raise_s
          [%message
            "n must be a equal to a power of two minus 1 (ie 3, 7, 15...255 etc)"
              (n : int)];
      let t = Option.value ~default:8 t in
      if 2 * t >= n
      then raise_s [%message "t is too large for given b" (t : int) (n : int)];
      let m = Int.ceil_log2 (n + 1) in
      let prim_poly =
        match prim_poly with
        | None -> default_poly m
        | Some prim_poly -> prim_poly
      in
      { Reedsolomon.Iter.m; k = n - (2 * t); t; n; b; prim_poly; prim_elt }]
  ;;
end

let random_codes (params : Reedsolomon.Iter.rsparams) n =
  Array.init n ~f:(fun _ -> Random.int (1 lsl params.m))
;;

let random_message (params : Reedsolomon.Iter.rsparams) = random_codes params params.k

let random_errors (params : Reedsolomon.Iter.rsparams) ~num_errors =
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
        if verbose then print_s [%message (params : Reedsolomon.Iter.rsparams)];
        Array.iter (random_message params) ~f:(printf "%i\n")]
;;

let command_encode =
  Command.basic
    ~summary:"Encode data (default configuration n=255, t=8)"
    [%map_open.Command
      let params = Codec_args.args
      and verbose = flag "-v" no_arg ~doc:"Display codec parameters" in
      fun () ->
        if verbose then print_s [%message (params : Reedsolomon.Iter.rsparams)];
        let codec = Reedsolomon.Iter.init params in
        let message =
          In_channel.input_lines In_channel.stdin
          |> List.map ~f:Int.of_string
          |> Array.of_list
        in
        if Array.length message <> params.k
        then raise_s [%message "Message length too large"];
        let parity = Array.create ~len:(2 * params.t) 0 in
        codec.encode message parity;
        Array.iter message ~f:(printf "%i\n");
        Array.iter parity ~f:(printf "%i\n")]
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
        if verbose then print_s [%message (params : Reedsolomon.Iter.rsparams)];
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
      and verbose = flag "-v" no_arg ~doc:"Display codec parameters" in
      fun () ->
        if verbose then print_s [%message (params : Reedsolomon.Iter.rsparams)];
        let codec = Reedsolomon.Iter.init params in
        let codeword =
          In_channel.input_lines In_channel.stdin
          |> List.map ~f:Int.of_string
          |> Array.of_list
        in
        if Array.length codeword <> params.n
        then raise_s [%message "Code word length too large"];
        let decoded = Array.create ~len:params.n 0 in
        let num_errors = codec.decode codeword decoded in
        if verbose then printf "errors = %i\n" num_errors;
        Array.iter decoded ~f:(printf "%i\n")]
;;

let command_correctness =
  Command.basic
    ~summary:"Test correctness of a particular codec configuration"
    [%map_open.Command
      let params = Codec_args.args
      and verbose = flag "-v" no_arg ~doc:"Display codec parameters"
      and num_tests =
        flag "-num-tests" (optional_with_default 1 int) ~doc:"NUM number of test to run"
      in
      fun () ->
        if verbose then print_s [%message (params : Reedsolomon.Iter.rsparams)];
        let codec = Reedsolomon.Iter.init params in
        for i = 0 to num_tests - 1 do
          let message = random_message params in
          let parity = Array.create ~len:(2 * params.t) 0 in
          codec.encode message parity;
          let codeword = Array.concat [ message; parity ] in
          let num_errors = Random.int (params.t + 1) in
          let errors = random_errors params ~num_errors in
          let corrupted_codeword = Array.map2_exn codeword errors ~f:( lxor ) in
          let decoded_message = Array.create ~len:params.n 0 in
          let got_errors = codec.decode corrupted_codeword decoded_message in
          if (not ([%compare.equal: int array] decoded_message codeword))
             || got_errors <> num_errors
          then (
            let diff = Array.map2_exn codeword decoded_message ~f:( lxor ) in
            raise_s
              [%message
                "failed to decode message"
                  (i : int)
                  (got_errors : int)
                  (num_errors : int)
                  (codeword : int array)
                  (decoded_message : int array)
                  (errors : int array)
                  (diff : int array)])
        done]
;;

let command =
  Command.group
    ~summary:"Reedsolomon codec test application"
    [ "encode", command_encode
    ; "decode", command_decode
    ; "message", command_message
    ; "errors", command_errors
    ; "correctness", command_correctness
    ]
;;
