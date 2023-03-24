open Base

(** {2 Unified CODEC API}

   Select between one of the abstract polynomial CODECs or the faster iterative CODEC. *)

type params = Reedsolomon.Iter_codec.params

module type Codec = sig
  type t

  val name : string
  val init : params -> t
  val decode : t -> int array -> int array
  val encode : t -> int array -> int array
end

module Iter_codec : Codec

module Poly_codec (Decoder : sig
  val decoder : [ `euclid | `peterson | `berlekamp ]
end) : Codec

module Io_buffers : sig
  type t

  val create : get_byte:(unit -> Char.t) -> put_byte:(Char.t -> unit) -> t
end

val codec_selection : (module Codec) list

(** {2 Steaming CODEC API. }*)

module Encode : sig
  type t

  val create
    :  params:Reedsolomon.Iter_codec.params
    -> Io_buffers.t
    -> total_length_in_bytes:int
    -> encoder:(int array -> int array)
    -> t

  val encode : t -> unit
end

module Decode : sig
  type t

  val create
    :  Io_buffers.t
    -> decoder:(Reedsolomon.Iter_codec.params -> int array -> int array)
    -> t

  val decode : t -> unit
end
