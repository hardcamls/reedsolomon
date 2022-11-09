module Make
  (Gp : Reedsolomon.Galois.Table_params)
  (Rp : Reedsolomon.Codec.RsParams)
  (N : Parallelism.S) : Syndromes_intf.M(Gp)(Rp)(N).S
