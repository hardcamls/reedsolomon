(** Configuration of a Reed-Solomon code *)
module type Params = Poly_codec_intf.Params

(** RS encoding and decoding *)
module type S = Poly_codec_intf.S

(** Create a Reed-Solomon code based on the given Galois field and code parameters *)
module Make (G : Galois.Table_ops) (P : Params) : S with type elt = G.t
