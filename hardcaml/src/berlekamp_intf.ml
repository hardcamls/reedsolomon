open Base
open Hardcaml

module M (Gp : Reedsolomon.Galois.Table.Params) (Rp : Reedsolomon.Codec.RsParams) = struct
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

    val create : Signal.t Interface.Create_fn(I)(O).t
  end
end
