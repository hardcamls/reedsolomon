open Core

module Hardware_codec (Config : sig
  include Hardcaml_reedsolomon.Parallelism.S

  val waves : bool
end) =
struct
  type t =
    { encode : int array -> int array
    ; decode : int array -> int array
    ; encoder_waves : Hardcaml_waveterm.Waveform.t option Lazy.t
    ; decoder_waves : Hardcaml_waveterm.Waveform.t option Lazy.t
    }

  let name = "hardware"

  let init params =
    let module Standard = (val Reedsolomon.Iter_codec.to_standard params) in
    let module Encoder = Test_encoder.Test (Standard) in
    let module Decoder = Test_decoder.Test (Standard) (Config) in
    let encoder = lazy (Encoder.create_and_reset ~waves:Config.waves ()) in
    let decoder = lazy (Decoder.create_and_reset ~waves:Config.waves ()) in
    (* XXX Huh.  The various revs here dont match the iterative codec, which doesn't
       match the poly codecs.  This is a bit of a mess.  But also, I dunno what the
       right ordering really is - or if there is one. *)
    let encode encoder message =
      let parity = Encoder.simulate_message (Lazy.force encoder) (Array.rev message) in
      Array.concat [ message; Array.rev parity ]
    in
    { encode = encode encoder
    ; decode = (fun x -> Decoder.simulate_codeword (Lazy.force decoder) x |> Array.rev)
    ; encoder_waves = lazy (Encoder.waves (Lazy.force encoder))
    ; decoder_waves = lazy (Decoder.waves (Lazy.force decoder))
    }
  ;;

  let encode t = t.encode
  let decode t = t.decode
  let encoder_waves t = Lazy.force t.encoder_waves
  let decoder_waves t = Lazy.force t.decoder_waves
end

let received =
  [| 35
   ; 32
   ; 82
   ; 101
   ; 101
   ; 100
   ; 115
   ; 111
   ; 108
   ; 111
   ; 109
   ; 111
   ; 110
   ; 10
   ; 10
   ; 79
   ; 67
   ; 97
   ; 109
   ; 108
   ; 32
   ; 108
   ; 105
   ; 98
   ; 114
   ; 97
   ; 114
   ; 121
   ; 32
   ; 105
   ; 109
   ; 112
   ; 108
   ; 101
   ; 109
   ; 101
   ; 110
   ; 116
   ; 105
   ; 110
   ; 103
   ; 32
   ; 97
   ; 32
   ; 82
   ; 101
   ; 101
   ; 100
   ; 45
   ; 83
   ; 111
   ; 108
   ; 111
   ; 109
   ; 111
   ; 110
   ; 32
   ; 101
   ; 114
   ; 114
   ; 111
   ; 114
   ; 32
   ; 99
   ; 111
   ; 114
   ; 114
   ; 101
   ; 99
   ; 116
   ; 105
   ; 111
   ; 110
   ; 32
   ; 67
   ; 79
   ; 68
   ; 69
   ; 67
   ; 32
   ; 97
   ; 110
   ; 100
   ; 32
   ; 99
   ; 111
   ; 114
   ; 114
   ; 101
   ; 115
   ; 112
   ; 111
   ; 110
   ; 100
   ; 105
   ; 110
   ; 103
   ; 32
   ; 10
   ; 105
   ; 109
   ; 112
   ; 108
   ; 101
   ; 109
   ; 101
   ; 110
   ; 116
   ; 97
   ; 116
   ; 105
   ; 111
   ; 110
   ; 32
   ; 105
   ; 110
   ; 32
   ; 72
   ; 97
   ; 114
   ; 100
   ; 99
   ; 97
   ; 109
   ; 108
   ; 46
   ; 10
   ; 10
   ; 33
   ; 91
   ; 98
   ; 117
   ; 105
   ; 108
   ; 100
   ; 32
   ; 115
   ; 116
   ; 97
   ; 116
   ; 117
   ; 115
   ; 93
   ; 40
   ; 104
   ; 116
   ; 116
   ; 112
   ; 115
   ; 58
   ; 47
   ; 47
   ; 103
   ; 105
   ; 116
   ; 104
   ; 117
   ; 98
   ; 46
   ; 99
   ; 111
   ; 109
   ; 47
   ; 104
   ; 97
   ; 114
   ; 100
   ; 99
   ; 97
   ; 109
   ; 108
   ; 115
   ; 47
   ; 114
   ; 101
   ; 101
   ; 100
   ; 115
   ; 111
   ; 108
   ; 111
   ; 109
   ; 111
   ; 110
   ; 47
   ; 97
   ; 99
   ; 116
   ; 105
   ; 111
   ; 110
   ; 115
   ; 47
   ; 119
   ; 111
   ; 114
   ; 107
   ; 102
   ; 108
   ; 111
   ; 119
   ; 115
   ; 47
   ; 109
   ; 97
   ; 105
   ; 110
   ; 46
   ; 121
   ; 109
   ; 108
   ; 47
   ; 98
   ; 97
   ; 100
   ; 103
   ; 101
   ; 46
   ; 115
   ; 118
   ; 103
   ; 41
   ; 10
   ; 10
   ; 35
   ; 32
   ; 65
   ; 98
   ; 111
   ; 117
   ; 116
   ; 32
   ; 82
   ; 83
   ; 32
   ; 99
   ; 111
   ; 100
   ; 105
   ; 151
   ; 209
   ; 227
   ; 240
   ; 51
   ; 134
   ; 141
   ; 17
   ; 143
   ; 37
   ; 204
   ; 239
   ; 70
   ; 47
   ; 37
   ; 161
  |]
;;

let params =
  { Reedsolomon.Iter_codec.n = 255
  ; t = 8
  ; k = 255 - 16
  ; b = 0
  ; m = 8
  ; prim_elt = 2
  ; prim_poly = 285
  }
;;

let test_bad_decode ?(waves = false) () =
  let module Codec =
    Hardware_codec (struct
      let n = 1
      let waves = waves
    end)
  in
  let codec = Codec.init params in
  let decoded = Codec.decode codec received in
  let diff = Array.map2_exn received decoded ~f:(fun a b -> if a = b then "-" else "X") in
  print_s [%message (received : int array) (decoded : int array) (diff : string array)];
  Codec.decoder_waves codec
;;

let%expect_test "example bad decode." =
  ignore (test_bad_decode () : _);
  [%expect
    {|
    ((received
      (35 32 82 101 101 100 115 111 108 111 109 111 110 10 10 79 67 97 109 108 32
       108 105 98 114 97 114 121 32 105 109 112 108 101 109 101 110 116 105 110
       103 32 97 32 82 101 101 100 45 83 111 108 111 109 111 110 32 101 114 114
       111 114 32 99 111 114 114 101 99 116 105 111 110 32 67 79 68 69 67 32 97
       110 100 32 99 111 114 114 101 115 112 111 110 100 105 110 103 32 10 105
       109 112 108 101 109 101 110 116 97 116 105 111 110 32 105 110 32 72 97 114
       100 99 97 109 108 46 10 10 33 91 98 117 105 108 100 32 115 116 97 116 117
       115 93 40 104 116 116 112 115 58 47 47 103 105 116 104 117 98 46 99 111
       109 47 104 97 114 100 99 97 109 108 115 47 114 101 101 100 115 111 108 111
       109 111 110 47 97 99 116 105 111 110 115 47 119 111 114 107 102 108 111
       119 115 47 109 97 105 110 46 121 109 108 47 98 97 100 103 101 46 115 118
       103 41 10 10 35 32 65 98 111 117 116 32 82 83 32 99 111 100 105 151 209
       227 240 51 134 141 17 143 37 204 239 70 47 37 161))
     (decoded
      (35 32 82 101 101 100 115 111 108 111 109 111 110 10 10 79 67 97 109 108 32
       108 105 98 114 97 114 121 32 105 109 112 108 101 109 101 110 116 105 110
       103 32 97 32 82 101 101 100 45 83 111 108 111 109 111 110 32 101 114 114
       111 114 32 99 111 114 114 101 99 116 105 111 110 32 67 79 68 69 67 32 97
       110 100 32 99 111 114 114 101 115 112 111 110 100 105 110 103 32 10 105
       109 112 108 101 109 101 110 116 97 114 105 111 110 32 105 110 32 72 97 114
       100 99 97 109 108 46 10 10 33 91 98 117 105 108 100 32 115 116 97 116 117
       115 93 40 104 116 116 112 115 58 47 47 103 105 116 104 117 98 46 99 111
       109 47 104 97 114 100 99 97 109 108 115 47 114 101 101 100 115 111 108 111
       109 111 110 47 97 99 116 105 111 110 115 47 119 111 114 107 102 108 111
       119 115 47 109 97 105 110 46 121 109 108 47 98 97 100 103 101 46 115 118
       103 41 10 10 35 32 65 98 111 117 116 32 82 83 32 99 111 100 105 151 209
       227 240 51 134 141 17 143 37 204 239 70 47 37 161))
     (diff
      (- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - X -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -))) |}]
;;

let%expect_test "Ok with sw codec." =
  let module Codec =
    Test_reedsolomon.Harness.Poly_codec (struct
      let decoder = `berlekamp
    end)
  in
  let codec = Codec.init params in
  let decoded = Codec.decode codec received in
  let diff = Array.map2_exn received decoded ~f:(fun a b -> if a = b then "-" else "X") in
  print_s [%message (received : int array) (decoded : int array) (diff : string array)];
  [%expect
    {|
    ((received
      (35 32 82 101 101 100 115 111 108 111 109 111 110 10 10 79 67 97 109 108 32
       108 105 98 114 97 114 121 32 105 109 112 108 101 109 101 110 116 105 110
       103 32 97 32 82 101 101 100 45 83 111 108 111 109 111 110 32 101 114 114
       111 114 32 99 111 114 114 101 99 116 105 111 110 32 67 79 68 69 67 32 97
       110 100 32 99 111 114 114 101 115 112 111 110 100 105 110 103 32 10 105
       109 112 108 101 109 101 110 116 97 116 105 111 110 32 105 110 32 72 97 114
       100 99 97 109 108 46 10 10 33 91 98 117 105 108 100 32 115 116 97 116 117
       115 93 40 104 116 116 112 115 58 47 47 103 105 116 104 117 98 46 99 111
       109 47 104 97 114 100 99 97 109 108 115 47 114 101 101 100 115 111 108 111
       109 111 110 47 97 99 116 105 111 110 115 47 119 111 114 107 102 108 111
       119 115 47 109 97 105 110 46 121 109 108 47 98 97 100 103 101 46 115 118
       103 41 10 10 35 32 65 98 111 117 116 32 82 83 32 99 111 100 105 151 209
       227 240 51 134 141 17 143 37 204 239 70 47 37 161))
     (decoded
      (35 32 82 101 101 100 115 111 108 111 109 111 110 10 10 79 67 97 109 108 32
       108 105 98 114 97 114 121 32 105 109 112 108 101 109 101 110 116 105 110
       103 32 97 32 82 101 101 100 45 83 111 108 111 109 111 110 32 101 114 114
       111 114 32 99 111 114 114 101 99 116 105 111 110 32 67 79 68 69 67 32 97
       110 100 32 99 111 114 114 101 115 112 111 110 100 105 110 103 32 10 105
       109 112 108 101 109 101 110 116 97 116 105 111 110 32 105 110 32 72 97 114
       100 99 97 109 108 46 10 10 33 91 98 117 105 108 100 32 115 116 97 116 117
       115 93 40 104 116 116 112 115 58 47 47 103 105 116 104 117 98 46 99 111
       109 47 104 97 114 100 99 97 109 108 115 47 114 101 101 100 115 111 108 111
       109 111 110 47 97 99 116 105 111 110 115 47 119 111 114 107 102 108 111
       119 115 47 109 97 105 110 46 121 109 108 47 98 97 100 103 101 46 115 118
       103 41 10 10 35 32 65 98 111 117 116 32 82 83 32 99 111 100 105 151 209
       227 240 51 134 141 17 143 37 204 239 70 47 37 161))
     (diff
      (- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -))) |}]
;;

let test_bad_syndromes ?waves () =
  let module Standard = (val Reedsolomon.Iter_codec.to_standard params) in
  let module Codec = Reedsolomon.Poly_codec.Make (Standard.G) (Standard.Rp) in
  let received = Array.rev received in
  let syndromes = Codec.syndromes received in
  print_s [%message (syndromes : int array)];
  let module Syndromes =
    Test_syndromes.Test
      (Standard)
      (struct
        let n = 1
      end)
  in
  Syndromes.test_codeword ?waves ~verbose:true received
;;

let%expect_test "the syndromes" =
  ignore (test_bad_syndromes : _);
  [%expect
    {| |}]
;;
