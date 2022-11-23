module Test (Standard : Reedsolomon.Standards.Standard) (N : Util.Parallelism) : sig
  val display_rules : Hardcaml_waveterm.Display_rules.t
  val test : ?waves:bool -> unit -> Hardcaml_waveterm.Waveform.t option
end

val test : ?waves:bool -> int -> Hardcaml_waveterm.Waveform.t option
