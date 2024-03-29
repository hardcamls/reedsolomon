open Base
open Hardcaml

module M
    (Gp : Reedsolomon.Galois.Table_params)
    (Rp : Reedsolomon.Poly_codec.Params)
    (N : Parallelism.S) =
struct
  module type S = sig
    module Syndromes : Syndromes_intf.M(Gp)(Rp)(N).S
    module RiBM : Berlekamp_intf.M(Gp)(Rp).S
    module Chien : Chien_intf.M(Gp)(Rp)(N).S
    module Forney : Forney_intf.M(Gp)(Rp)(N).S
    module Input_fifo : Input_fifo_intf.M(Gp)(Rp)(N).S

    module I : sig
      type 'a t =
        { clocking : 'a Clocking.t
        ; enable : 'a
        ; load : 'a
        ; first : 'a
        ; last : 'a
        ; x : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O_debug : sig
      type 'a t =
        { syn : 'a Syndromes.O.t
        ; bm : 'a RiBM.O.t
        ; ch : 'a Chien.O.t
        ; fy : 'a Forney.Parallel.O.t
        ; corrected : 'a array [@length N.n] [@bits Gfh.bits]
        ; ordy : 'a [@bits 1]
        ; error_count : 'a [@bits Gfh.bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { corrected : 'a array
        ; ordy : 'a
        ; error_count : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create_with_debug : Scope.t -> Interface.Create_fn(I)(O_debug).t
    val create : Scope.t -> Interface.Create_fn(I)(O).t
    val hierarchy : Scope.t -> Interface.Create_fn(I)(O).t
  end
end

module type Decoder = sig
  module M = M

  module Make
      (Gp : Reedsolomon.Galois.Table_params)
      (Rp : Reedsolomon.Poly_codec.Params)
      (N : Parallelism.S) : M(Gp)(Rp)(N).S
end
