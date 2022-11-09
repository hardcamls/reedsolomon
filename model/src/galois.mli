module type Primitive_field_prime = Galois_intf.Primitive_field_prime

(* primitive fields of type GF(n) where n is prime *)
module Primitive_field (Prime : Primitive_field_prime) : Ops.S with type t = int

(* Primitive field of order 2 GF(2) *)
module GF2 : Ops.S with type t = int

module type Extension_field_generator = Galois_intf.Extension_field_generator

(* GF(n^m) extension fields built from primitive fields and polynomials *)
module Extension_field (G : Extension_field_generator) : Ops.S with type t = G.Poly.t

module type GF2_extension_field_generator = Galois_intf.GF2_extension_field_generator

(** convenience module for building GF(2^n) fields *)
module GF2N (G : GF2_extension_field_generator) : Ops.S with type t = int array

(** list of primitive polys for GF(2); 3..24 *)
val gf2_prim_polys : int array array

module type Table_generator = Galois_intf.Table_generator
module type Table_ops = Galois_intf.Table_ops
module type Table_params = Galois_intf.Table_params

(** builds log/antilog table representation over any field representation *)
module Table (G : Table_generator) : Table_ops with type t = G.Ops.t

(** specialised representation using integers *)
module To_int_table_field (Ops : Table_ops with type t = int array) :
  Table_ops with type t = int

module Generator_of_table_params (P : Table_params) :
  Table_generator with type Ops.t = int array

module Int_table_of_params (P : Table_params) : Table_ops with type t = int
