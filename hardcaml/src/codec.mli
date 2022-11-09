(** hardware reed-solomon codec *)
open! Base

open Hardcaml

module Make (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Poly_codec.Params) : sig
  module Gfh : module type of Galois.Make (Signal) (Gp)
  module Encoder : Encoder_intf.M(Gp)(Rp).S
  module Decoder (N : Parallelism.S) : Decoder_intf.M(Gp)(Rp)(N).S
end
