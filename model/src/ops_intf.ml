module type S = sig
  type t [@@deriving sexp_of, compare]

  val zero : t
  val one : t
  val ( +: ) : t -> t -> t
  val ( -: ) : t -> t -> t
  val ( *: ) : t -> t -> t
  val ( /: ) : t -> t -> t
  val to_string : t -> string
end

module type Extended = sig
  include S

  val ( %: ) : t -> t -> t
  val abs : t -> t
  val ( <<: ) : t -> int -> t
  val ( >>+ ) : t -> int -> t
  val ( >>: ) : t -> int -> t
  val ( &: ) : t -> t -> t
  val ( |: ) : t -> t -> t
  val ( ^: ) : t -> t -> t
  val ( ~: ) : t -> t
  val of_int : int -> t
  val to_int : t -> int
  val of_int32 : int32 -> t
  val to_int32 : t -> int32
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_float : float -> t
  val to_float : t -> float
  val of_string : string -> t
end
