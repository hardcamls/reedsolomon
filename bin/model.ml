(* Reed-solomon test application. *)
open Core

type params = Reedsolomon.Iter_codec.params [@@deriving sexp_of]

module type Codec = sig
  type t

  val init : params -> t
  val decode : t -> int array -> int array
  val encode : t -> int array -> int array
end

module Iter_codec : Codec = struct
  include Reedsolomon.Iter_codec

  let encode t message =
    let parity = Array.create ~len:((params t).t * 2) 0 in
    encode t message parity;
    Array.concat [ message; parity ]
  ;;

  let decode t errd =
    let corrected = Array.create ~len:(params t).n 0 in
    let _num_errors = decode t errd corrected in
    corrected
  ;;
end

module Poly_codec (Decoder : sig
  val decoder : [ `euclid | `peterson | `berlekamp ]
end) : Codec = struct
  type t =
    { rp : (module Reedsolomon.Poly_codec.Params)
    ; g : (module Reedsolomon.Galois.Table_ops with type t = int)
    }

  let init (params : params) =
    let module Rp = struct
      let k = params.k
      let t = params.t
      let b = params.b
    end
    in
    let module Gp = struct
      let pp = params.prim_poly
      let pe = params.prim_elt
    end
    in
    let module G = Reedsolomon.Galois.Int_table_of_params (Gp) in
    { rp = (module Rp); g = (module G) }
  ;;

  let encode t (message : int array) =
    let module Codec = Reedsolomon.Poly_codec.Make ((val t.g)) ((val t.rp)) in
    let encoded = Codec.encode (Array.rev message) in
    Array.rev encoded
  ;;

  let decode t (message : int array) =
    let module Codec = Reedsolomon.Poly_codec.Make ((val t.g)) ((val t.rp)) in
    let decoded =
      (match Decoder.decoder with
       | `euclid -> Codec.decode_euclid
       | `peterson -> Codec.decode_peterson
       | `berlekamp -> Codec.decode_berlekamp_massey)
        (Array.rev message)
    in
    Array.rev decoded
  ;;
end

let codec_selection_arg =
  Command.Arg_type.create (function
    | "peterson" ->
      let module Codec =
        Poly_codec (struct
          let decoder = `peterson
        end)
      in
      (module Codec : Codec)
    | "euclid" ->
      let module Codec =
        Poly_codec (struct
          let decoder = `euclid
        end)
      in
      (module Codec : Codec)
    | "berlekamp" | "berlekamp-massey" ->
      let module Codec =
        Poly_codec (struct
          let decoder = `berlekamp
        end)
      in
      (module Codec : Codec)
    | "iter" | "iterative" -> (module Iter_codec : Codec)
    | _ -> raise_s [%message "invalid codec specification"])
;;

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
      in
      fun () ->
        let module Codec = (val codec : Codec) in
        if verbose then print_s [%message (params : params)];
        let codec = Codec.init params in
        for i = 0 to num_tests - 1 do
          let message = random_message params in
          let parity = Codec.encode codec message in
          let codeword = Array.concat [ message; parity ] in
          let num_errors = Random.int (params.t + 1) in
          let errors = random_errors params ~num_errors in
          let corrupted_codeword = Array.map2_exn codeword errors ~f:( lxor ) in
          let decoded_message = Codec.decode codec corrupted_codeword in
          if not ([%compare.equal: int array] decoded_message codeword)
          then (
            let diff = Array.map2_exn codeword decoded_message ~f:( lxor ) in
            raise_s
              [%message
                "failed to decode message"
                  (i : int)
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
