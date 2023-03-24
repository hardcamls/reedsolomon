open Core

module Hardware_codec (Parallelism : Hardcaml_reedsolomon.Parallelism.S) = struct
  type t =
    { encode : int array -> int array
    ; decode : int array -> int array
    }

  let name = "hardware"

  let init
      ({ n = _; m = _; k; t; b; prim_poly; prim_elt } : Reedsolomon.Iter_codec.params)
    =
    let module Gp = struct
      let pp = prim_poly
      let pe = prim_elt
    end
    in
    let module Rp = struct
      let k = k
      let t = t
      let b = b
    end
    in
    let module Standard = Reedsolomon.Standards.Make (Gp) (Rp) in
    let module Encoder = Test_encoder.Test (Standard) in
    let module Decoder = Test_decoder.Test (Standard) (Parallelism) in
    let encoder = lazy (Encoder.create_and_reset ()) in
    let decoder = lazy (Decoder.create_and_reset ()) in
    (* XXX Huh.  The various revs here dont match the iterative codec, which doesn't
       match the poly codecs.  This is a bit of a mess.  But also, I dunno what the
        right ordering really is - or if there is one. *)
    let encode encoder message =
      let parity = Encoder.simulate_message (Lazy.force encoder) (Array.rev message) in
      Array.concat [ message; Array.rev parity ]
    in
    { encode = encode encoder
    ; decode = (fun x -> Decoder.simulate_codeword (Lazy.force decoder) x |> Array.rev)
    }
  ;;

  let encode t = t.encode
  let decode t = t.decode
end
