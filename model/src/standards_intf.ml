open Base

module type Standard = sig
  module Gp : Galois.Table_params
  module G : Galois.Table_ops with type t = int
  module Rp : Poly_codec.Params
  module R : Poly_codec.S with type elt = int
end
