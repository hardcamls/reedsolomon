open! Base
open Hardcaml

module M (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Poly_codec.Params) =
struct
  module type S = sig
    module I : sig
      type 'a t =
        { clocking : 'a Clocking.t
        ; enable : 'a
        ; ctrl : 'a
        ; d : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
    end

    val create : Interface.Create_fn(I)(O).t
  end
end

module type Encoder = sig
  module M = M

  module Make (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Poly_codec.Params) :
    M(Gp)(Rp).S
end
