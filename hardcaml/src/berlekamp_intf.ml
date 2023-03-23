open Base
open Hardcaml

module M (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Poly_codec.Params) =
struct
  module type S = sig
    module I : sig
      type 'a t =
        { clocking : 'a Clocking.t
        ; enable : 'a
        ; first : 'a
        ; last : 'a
        ; syndromes : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { w : 'a array
        ; l : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
    val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  end
end

module type Berlekamp = sig
  module Make (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Poly_codec.Params) :
    M(Gp)(Rp).S
end
