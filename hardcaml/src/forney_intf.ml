open Base
open Hardcaml

module M
  (Gp : Reedsolomon.Galois.Table_params)
  (Rp : Reedsolomon.Poly_codec.Params)
  (N : Parallelism.S) =
struct
  module type S = sig
    module Serial : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; start : 'a
          ; store : 'a
          ; ctrl : 'a
          ; tap : 'a
          ; x : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t = { e : 'a } [@@deriving sexp_of, hardcaml]
      end

      val create : Signal.t Interface.Create_fn(I)(O).t
    end

    module Parallel : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; vld : 'a array
          ; err : 'a array
          ; v : 'a array
          ; l : 'a array
          ; x : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t =
          { emag : 'a array
          ; frdy : 'a array
          ; ferr : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      val create : Signal.t Interface.Create_fn(I)(O).t
    end
  end
end
