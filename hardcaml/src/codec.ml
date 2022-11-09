(* hardware implementation of reed-solomon codec *)
open! Base
open Hardcaml

module Make (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Poly_codec.Params) =
struct
  module Gfh = Galois.Make (Signal) (Gp)
  module Encoder = Encoder.Make (Gp) (Rp)
  module Decoder (N : Parallelism.S) = Decoder.Make (Gp) (Rp) (N)
end
