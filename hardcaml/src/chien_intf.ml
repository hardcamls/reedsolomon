open Base
open Hardcaml

module M
  (Gp : Reedsolomon.Galois.Table.Params)
  (Rp : Reedsolomon.Codec.RsParams)
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

    val create : Signal.t Interface.Create_fn(I)(O).t
  end
end