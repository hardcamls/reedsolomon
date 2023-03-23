open Base

module type Standard = sig
  module Gp : Galois.Table_params
  module G : Galois.Table_ops with type t = int
  module Rp : Poly_codec.Params
  module R : Poly_codec.S with type elt = int
end

module type Standards = sig
  module type Standard = Standard

  module Make (Gp : Galois.Table_params) (Rp : Poly_codec.Params) : Standard

  (** Test code used in BBC white paper *)
  module BBCTest : Standard

  (** Consultative Committee for Space Data Systems *)
  module CCSDS : sig
    val dual_of_poly : int array
    val poly_of_dual : int array

    (** t=16 *)
    module Rs16 : Standard

    (** t=8 *)
    module Rs8 : Standard
  end

  (** Digital Video Broadcasting *)
  module DVB : Standard

  (** Advanced Television Systems Committee *)
  module ATSC : Standard

  (** Interfaces for the Optical Transport Network *)
  module G709 : Standard
end
