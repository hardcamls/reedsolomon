module Make
  (Gp : Reedsolomon.Galois.Table.Params)
  (Rp : Reedsolomon.Codec.RsParams)
  (N : Parallelism.S) : Input_fifo_intf.M(Gp)(Rp)(N).S