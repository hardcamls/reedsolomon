module Make (Gp : Reedsolomon.Galois.Table.Params) (Rp : Reedsolomon.Codec.RsParams) :
  Encoder_intf.M(Gp)(Rp).S
