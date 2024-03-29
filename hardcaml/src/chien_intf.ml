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
        ; start : 'a
        ; lambda : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { eval : 'a array
        ; eloc : 'a array
        ; evld : 'a array
        ; eerr : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create : Scope.t -> Interface.Create_fn(I)(O).t
    val hierarchy : Scope.t -> Interface.Create_fn(I)(O).t
  end
end

module type Chien = sig
  module M = M

  module Make
      (Gp : Reedsolomon.Galois.Table_params)
      (Rp : Reedsolomon.Poly_codec.Params)
      (N : Parallelism.S) : M(Gp)(Rp)(N).S
end
