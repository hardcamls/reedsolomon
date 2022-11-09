module Make
  (Gp : Reedsolomon.Galois.Table_params)
  (Rp : Reedsolomon.Poly_codec.Params)
  (N : Parallelism.S) : Decoder_intf.M(Gp)(Rp)(N).S
