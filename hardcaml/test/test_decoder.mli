module Test
    (Standard : Reedsolomon.Standards.Standard)
    (N : Hardcaml_reedsolomon.Parallelism.S) : sig
  val display_rules : Hardcaml_waveterm.Display_rules.t

  type t

  val waves : t -> Hardcaml_waveterm.Waveform.t option
  val create_and_reset : ?waves:bool -> unit -> t
  val simulate_codeword : t -> int array -> int array
  val test_one_codeword : ?waves:bool -> unit -> Hardcaml_waveterm.Waveform.t option
end

val test_one_codeword : ?waves:bool -> int -> Hardcaml_waveterm.Waveform.t option
