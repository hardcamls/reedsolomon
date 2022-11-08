(** hardware reed-solomon codec *)
open Hardcaml

module Make (Gp : Reedsolomon.Galois.Table.Params) (Rp : Reedsolomon.Codec.RsParams) : sig
  module Gfh : module type of Galois.Make (Signal) (Gp)
  module Encoder : Encoder_intf.M(Gp)(Rp).S
  module Decoder (N : Parallelism.S) : Decoder_intf.M(Gp)(Rp)(N).S
end
