module Test (Standard : Reedsolomon.Standards.Standard) : sig
  type t

  val waves : t -> Hardcaml_waveterm.Waveform.t option
  val create_and_reset : ?waves:bool -> unit -> t
  val simulate_message : t -> int array -> int array
end
