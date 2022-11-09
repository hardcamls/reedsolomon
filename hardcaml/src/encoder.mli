module Make (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Poly_codec.Params) :
  Encoder_intf.M(Gp)(Rp).S
