open Base

module type S = sig
  type elt [@@deriving sexp_of]
  type t = elt array array [@@deriving sexp_of]

  (* size of matrix *)
  val rows : t -> int
  val cols : t -> int

  (* construction of various matrices *)
  val init : int -> int -> (int -> int -> elt) -> t
  val create : int -> int -> t
  val copy : t -> t
  val identity : int -> t
  val transpose : t -> t
  val map : (elt -> elt) -> t -> t
  val map2 : (elt -> elt -> elt) -> t -> t -> t
  val row_vector : elt array -> t
  val col_vector : elt array -> t

  (* concatenate matrices *)
  val ( >> ) : t -> t -> t
  val ( ^^ ) : t -> t -> t

  (* select a sub matrix *)
  val sub : int -> int -> int -> int -> t -> t

  (* arithmetic *)
  val ( +: ) : t -> t -> t
  val ( -: ) : t -> t -> t
  val ( *: ) : t -> t -> t
  val ( *:. ) : t -> elt -> t

  (* functions related to inverses *)
  val minor : int -> int -> t -> t
  val det : t -> elt
  val adjoint_inverse : t -> elt * t

  (* these functions require element division *)
  val gauss_jordan : t -> t
  val gauss_jordan_inverse : t -> t

  (* elementary row operations *)
  module Row : sig
    val swap : int -> int -> int -> t
    val mult : int -> int -> elt -> t
    val madd : int -> int -> int -> elt -> t
  end
end

module type Matrix = sig
  module type S = S

  module Make (Ops : Ops.S) : S with type elt = Ops.t
end
