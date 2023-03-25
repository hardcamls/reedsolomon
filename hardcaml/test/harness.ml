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
