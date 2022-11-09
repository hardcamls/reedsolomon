(** Configuration of a Reed-Solomon code *)
module type RsParams = Codec_intf.RsParams

(** RS encoding and decoding *)
module type RsPoly = Codec_intf.RsPoly

(** Create a Reed-Solomon code based on the given Galois field and code parameters *)
module MakePoly (G : Galois.Table_ops) (P : RsParams) : RsPoly with type elt = G.t
