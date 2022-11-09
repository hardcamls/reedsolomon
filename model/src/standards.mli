open Base

module type Standard = Standards_intf.Standard

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
