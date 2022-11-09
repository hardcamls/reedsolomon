module type Primitive_field_prime = sig
  val n : int
end

module type Extension_field_generator = sig
  (* polynomial with primitive field coefficients *)
  module Poly : Poly.S

  (* primitive polynomial *)
  val pp : Poly.t
end

module type GF2_extension_field_generator = sig
  val pp : int array
end

module type Table_generator = sig
  module Ops : Ops.S

  (* primitive element *)
  val alpha : Ops.t
end

module type Table_ops = sig
  include Ops.S

  (** primitive element *)
  val alpha : t

  (** number of elements in field *)
  val n_elems : int

  (** log x = b when alpha^b = x *)
  val log : t -> int

  (** inverse log *)
  val antilog : int -> t

  (** multiplication *)
  val ( *: ) : t -> t -> t

  (** division *)
  val ( /: ) : t -> t -> t

  (** power *)
  val ( **: ) : t -> int -> t

  (** inverse *)
  val inv : t -> t
end

module type Table_params = sig
  val pp : int
  val pe : int
end
