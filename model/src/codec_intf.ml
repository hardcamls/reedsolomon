module type RsParams = sig
  val k : int
  val t : int
  val b : int
end

module type RsPoly = sig
  type elt

  module M : Matrix.S with type elt = elt
  module R : Poly.S with type t = elt array and type elt = elt

  type poly = R.t [@@deriving sexp_of]
  type loc = int [@@deriving sexp_of]

  val root : int -> elt
  val generator : poly
  val xn : int -> poly
  val x2t : poly
  val parity : poly -> poly
  val encode : poly -> poly
  val horner : poly -> elt -> elt
  val syndromes : poly -> poly
  val key_equations : poly -> int -> M.t * M.t
  val solve_key_equations : M.t * M.t -> M.t
  val peterson : poly -> poly
  val euclid_inner : poly * poly -> poly * poly -> poly * poly
  val euclid : ?norm:bool -> ?lim:int -> poly -> poly * poly
  val berlekamp_massey_iter : poly -> int -> poly * poly * int -> poly * poly * int
  val berlekamp_massey : poly -> poly

  module Sarwate : sig
    val iBM : poly -> poly
    val riBM : poly -> poly * poly
    val rriBM : poly -> poly * poly
    val forney : poly -> poly -> loc -> elt
  end

  val chien : poly -> loc list
  val error_location : loc -> int
  val error_magnitude : int -> poly -> poly -> poly
  val deriv : poly -> poly
  val forney : poly -> poly -> loc -> elt
  val error : elt list -> loc list -> poly
  val correct : poly -> poly -> poly
  val decode_euclid : poly -> poly
  val decode_berlekamp_massey : poly -> poly
  val decode_peterson : poly -> poly
  val decode : poly -> poly
  val erasure_locator : int list -> poly
  val zero_erasures : poly -> int list -> poly
  val error_and_erasure : elt list -> loc list -> elt list -> loc list -> poly
  val decode_erasures_euclid : poly -> int list -> poly
  val decode_erasures : poly -> int list -> poly
  val decode_errors_and_erasures_euclid : poly -> int list -> poly

  (*val decode_errors_and_erasures_berlekamp_massey : poly -> int list -> poly*)
  val decode_errors_and_erasures : poly -> int list -> poly
end
