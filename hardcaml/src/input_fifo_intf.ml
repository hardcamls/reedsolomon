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
        ; wr : 'a
        ; d : 'a array
        ; rd : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { q : 'a array } [@@deriving sexp_of, hardcaml]
    end

    val create : Signal.t Interface.Create_fn(I)(O).t
  end
end
