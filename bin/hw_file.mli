val command : Core.Command.t

module New : sig
  module Encode : sig
    type t

    val create
      :  params:Reedsolomon.Iter_codec.params
      -> get_byte:(unit -> Char.t)
      -> put_byte:(Char.t -> unit)
      -> total_length_in_bits:int
      -> t

    val encode : t -> unit
  end
end
