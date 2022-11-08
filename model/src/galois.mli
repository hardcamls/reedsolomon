(* primitive fields of type GF(n) where n is prime *)
module Primitive : sig
  module type GF_n = sig
    val n : int
  end

  (* GF(n) primitive finite field *)
  module GFN (GF_n : GF_n) : Ops.OpsBase with type t = int

  (* GF(2) *)
  module GF2 : Ops.OpsBase with type t = int
end

(* GF(n^m) extension fields built from primitive fields and polynomials *)
module Extension : sig
  module type Generator = sig
    (** polynomial with primitive field coefficients *)
    module Poly : Poly.S

    (** primitive polynomial *)
    val pp : Poly.t
  end

  (** make extension field *)
  module Make (G : Generator) : Ops.OpsBase with type t = G.Poly.t
end

(** convenience module for building GF(2^n) fields *)
module GF2N : sig
  module Make (P : sig
    val pp : int array
  end) : Ops.OpsBase with type t = int array

  (** list of primitive polys for GF(2); 3..24 **)
  val gf2_prim_polys : int array array
end

module Table : sig
  module type Generator = sig
    module Ops : Ops.OpsBase

    val alpha : Ops.t
    (* primitive element *)
  end

  module type Ops = sig
    include Ops.OpsBase

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

  (** builds log/antilog table representation over any field representation *)
  module Make (G : Generator) : Ops with type t = G.Ops.t

  (** specialised representation using integers *)
  module Int (Ops : Ops with type t = int array) : Ops with type t = int

  (** simplified field specification using integers *)
  module type Params = sig
    (** primitive polynomial (including leading power) *)
    val pp : int

    (** primitive element *)
    val pe : int
  end

  module Params (P : Params) : Generator with type Ops.t = int array
  module MakeInt (P : Params) : Ops with type t = int
end
