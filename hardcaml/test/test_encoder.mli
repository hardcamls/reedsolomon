module Test (Standard : Reedsolomon.Standards.Standard) : sig
  type t

  val create_and_reset : ?waves:bool -> unit -> t
  val simulate_message : t -> int array -> int array
  val test : ?waves:bool -> int -> Hardcaml_waveterm.Waveform.t option
end

val display_rules : Hardcaml_waveterm.Display_rules.t
val test : ?waves:bool -> int -> Hardcaml_waveterm.Waveform.t option
