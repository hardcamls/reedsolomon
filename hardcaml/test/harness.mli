module Hardware_codec (Config : sig
  include Hardcaml_reedsolomon.Parallelism_intf.S

  val waves : bool
end) : sig
  include Test_reedsolomon.Harness.Codec

  val encoder_waves : t -> Hardcaml_waveterm.Waveform.t option
  val decoder_waves : t -> Hardcaml_waveterm.Waveform.t option
end
