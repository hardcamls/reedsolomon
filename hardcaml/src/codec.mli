(** hardware reed-solomon codec *)
open Hardcaml

module type S = sig
  open Signal

  module Clocking : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Encoder : sig
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

    val create : t I.t -> t O.t
  end

  module type N = sig
    val n : int
  end

  module Decoder (N : N) : sig
    (** parallel calculation of syndromes.

        Calculates [x{n-1}.r{n-1}^(n-1) + ... + x{0}.r{0}^0] where [x{i}] is an
        input symbol and [r{i}] is a root of the generator polynomial.  The symbols [x{i}]
        should be provided from the highest index to the lowest.

        The [Rp.n] symbols which comprise the received codeword are passed into the circuit
        via the [x] array at a rate of [N.n] symbols per cycle.

        The [first] and [last] cycles of data must be strobed (active high, in extremis they
        can be on the same cycle).

        In most cases the parallel inputs will not evenly divide the codeword size [Rp.n].
        For example with [Rp.n=15] and [N.n=2] there will be 1 extra symbol.  If we
        logically pack 0's at the start of the codeword (corresponding to the highest power)
        then all is OK.

        Alternatively we could add 0's at the end of the codeword (this might be easier
        for integration with a system and is required by the correction logic in the
        provided decoder architecture).  The output syndrome values become scaled by
        [(root i)^(extra-zeros)].  The [scale] parameter can be used to remove this 
        scale factor by setting it to [extra-zeros].

     *)
    module Syndromes : sig
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

      val create : scale:int -> t I.t -> t O.t
    end

    (* berlekamp massey *)
    module RiBM : sig
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

      val create : t I.t -> t O.t
    end

    module Chien : sig
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

      val create : t I.t -> t O.t
    end

    module Forney_serial : sig
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

      val create : t I.t -> t O.t
    end

    module Forney : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; vld : 'a
          ; err : 'a
          ; v : 'a array
          ; l : 'a array
          ; x : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t =
          { emag : 'a
          ; frdy : 'a
          ; ferr : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end

      val create : t I.t -> t O.t
    end

    module PForney : sig
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

      val create : t I.t -> t O.t
    end

    module Decode : sig
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
          ; fy : 'a PForney.O.t
          ; corrected : 'a array
          ; ordy : 'a
          ; error_count : 'a
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

      val create_with_debug : t I.t -> t O_debug.t
      val create : t I.t -> t O.t
    end
  end
end

module Make (Gp : Reedsolomon.Galois.Table.Params) (Rp : Reedsolomon.Codec.RsParams) : sig
  include S
  module Gfh : module type of Galois.Make (Signal) (Gp)
end
