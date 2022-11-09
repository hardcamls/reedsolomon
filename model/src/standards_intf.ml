open Base

module type Standard = sig
  module Gp : Galois.Table_params
  module G : Galois.Table_ops with type t = int
  module Rp : Codec.RsParams
  module R : Codec.RsPoly with type elt = int
end
