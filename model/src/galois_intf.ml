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

module type Galois = sig
  module type Primitive_field_prime = Primitive_field_prime

  (* primitive fields of type GF(n) where n is prime *)
  module Primitive_field (Prime : Primitive_field_prime) : Ops.S with type t = int

  (* Primitive field of order 2 GF(2) *)
  module GF2 : Ops.S with type t = int

  module type Extension_field_generator = Extension_field_generator

  (* GF(n^m) extension fields built from primitive fields and polynomials *)
  module Extension_field (G : Extension_field_generator) : Ops.S with type t = G.Poly.t

  module type GF2_extension_field_generator = GF2_extension_field_generator

  (** convenience module for building GF(2^n) fields *)
  module GF2N (G : GF2_extension_field_generator) : Ops.S with type t = int array

  (** list of primitive polys for GF(2); 3..24 *)
  val gf2_prim_polys : int array array

  val int_of_gf2_prim_poly : int -> int

  module type Table_generator = Table_generator
  module type Table_ops = Table_ops
  module type Table_params = Table_params

  (** builds log/antilog table representation over any field representation *)
  module Table (G : Table_generator) : Table_ops with type t = G.Ops.t

  (** specialised representation using integers *)
  module To_int_table_field (Ops : Table_ops with type t = int array) :
    Table_ops with type t = int

  module Generator_of_table_params (P : Table_params) :
    Table_generator with type Ops.t = int array

  module Int_table_of_params (P : Table_params) : Table_ops with type t = int
end
