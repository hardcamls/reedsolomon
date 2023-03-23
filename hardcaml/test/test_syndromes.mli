module Test (N : Hardcaml_reedsolomon.Parallelism.S) : sig
  val display_rules : Hardcaml_waveterm.Display_rules.t
  val test : ?waves:bool -> unit -> Hardcaml_waveterm.Waveform.t option
end

val test : ?waves:bool -> int -> Hardcaml_waveterm.Waveform.t option
