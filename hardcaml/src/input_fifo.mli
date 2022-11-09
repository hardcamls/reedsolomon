module Make
  (Gp : Reedsolomon.Galois.Table_params)
  (Rp : Reedsolomon.Poly_codec.Params)
  (N : Parallelism.S) : Input_fifo_intf.M(Gp)(Rp)(N).S
