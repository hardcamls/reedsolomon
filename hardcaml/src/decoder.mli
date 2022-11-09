module Make
  (Gp : Reedsolomon.Galois.Table_params)
  (Rp : Reedsolomon.Codec.RsParams)
  (N : Parallelism.S) : Decoder_intf.M(Gp)(Rp)(N).S
