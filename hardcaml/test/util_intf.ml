open! Core
open! Hardcaml

module type S = sig
  module Sw : Reedsolomon.Standards.Standard
  module Hw : module type of Hardcaml_reedsolomon.Codec.Make (Sw.Gp) (Sw.Rp)
  module Gfb : module type of Hardcaml_reedsolomon.Galois.Make (Bits) (Sw.Gp)

  val k : int
  val t : int
  val b : int
  val n : int
  val m : int
  val n_elems : int
  val message : unit -> Sw.R.poly
  val rev : Sw.R.poly -> Sw.R.poly
  val ( ^. ) : Sw.R.poly -> Sw.R.poly -> Sw.R.poly
  val codeword : Sw.R.poly -> Sw.R.poly
  val parity : Sw.R.poly -> Sw.R.poly
  val error : int -> Sw.R.poly
  val syndromes : Sw.R.poly -> Sw.R.poly
  val berlekamp : Sw.R.poly -> Sw.R.poly * Sw.R.poly
  val chien : Sw.R.poly -> int list
  val err_loc : int -> int
  val dump : Sw.R.poly -> unit

  val waveform_opt
    :  ?waves:bool
    -> ('i, 'o) Hardcaml.Cyclesim.t
    -> Hardcaml_waveterm.Waveform.t option * ('i, 'o) Hardcaml.Cyclesim.t
end

module type Util = sig
  module type S = S

  module Make (Standard : Reedsolomon.Standards.Standard) : S
  module Basic : S
end
