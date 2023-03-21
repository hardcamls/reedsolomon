open Base
open Hardcaml

module type S = sig
  module G : Reedsolomon.Galois.Table_ops with type t = int

  type t
  type bits

  val n_elems : int
  val bits : int
  val alpha : t
  val zero : t
  val one : t
  val log : t -> t
  val antilog : t -> t
  val inv : t -> t
  val ( +: ) : t -> t -> t
  val ( -: ) : t -> t -> t
  val ( *: ) : t -> t -> t
  val modfs : t -> t
  val ( /: ) : t -> t -> t
  val ( **: ) : t -> bits -> t
  val to_string : t -> string
  val rom : (int -> G.t) -> bits -> t
  val cpow : t -> int -> t
  val cmul : ?rom:bool -> G.t -> t -> t
end

module type Galois = sig
  module type S = S

  module Make (B : Comb.S) (P : Reedsolomon.Galois.Table_params) :
    S with type t = B.t and type bits := B.t

  module Make_hierarchical (Scope : sig
    val scope : Scope.t
  end)
  (P : Reedsolomon.Galois.Table_params) :
    S with type t = Signal.t and type bits := Signal.t

  module type S_scoped = S with type t = Signal.t and type bits := Signal.t

  val of_scope : (module Reedsolomon.Galois.Table_params) -> Scope.t -> (module S_scoped)
end
