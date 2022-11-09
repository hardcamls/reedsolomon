module Make (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Codec.RsParams) :
  Encoder_intf.M(Gp)(Rp).S
