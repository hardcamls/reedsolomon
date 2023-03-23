open Base
open Hardcaml

module M
    (Gp : Reedsolomon.Galois.Table_params)
    (Rp : Reedsolomon.Poly_codec.Params)
    (N : Parallelism.S) =
struct
  module type S = sig
    module I : sig
      type 'a t =
        { clocking : 'a Clocking.t
        ; enable : 'a
        ; first : 'a
        ; last : 'a
        ; x : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { valid : 'a
        ; syndromes : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create : scale:int -> Scope.t -> Interface.Create_fn(I)(O).t
    val hierarchy : scale:int -> Scope.t -> Interface.Create_fn(I)(O).t
  end
end

module type Syndromes = sig
  module M = M

  module Make
      (Gp : Reedsolomon.Galois.Table_params)
      (Rp : Reedsolomon.Poly_codec.Params)
      (N : Parallelism.S) : M(Gp)(Rp)(N).S
end
