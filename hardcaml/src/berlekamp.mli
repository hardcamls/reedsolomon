module Make (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Codec.RsParams) :
  Berlekamp_intf.M(Gp)(Rp).S
