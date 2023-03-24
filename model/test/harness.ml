open Core
open Hardcaml

type params = Reedsolomon.Iter_codec.params

module type Codec = sig
  type t

  val name : string
  val init : params -> t
  val decode : t -> int array -> int array
  val encode : t -> int array -> int array
end

module Iter_codec : Codec = struct
  include Reedsolomon.Iter_codec

  let name = "iterative"

  let encode t message =
    let parity = Array.create ~len:((params t).t * 2) 0 in
    encode t message parity;
    Array.concat [ message; parity ]
  ;;

  let decode t errd =
    let corrected = Array.create ~len:(params t).n 0 in
    let _num_errors = decode t errd corrected in
    corrected
  ;;
end

module Poly_codec (Decoder : sig
  val decoder : [ `euclid | `peterson | `berlekamp ]
end) : Codec = struct
  type t =
    { rp : (module Reedsolomon.Poly_codec.Params)
    ; g : (module Reedsolomon.Galois.Table_ops with type t = int)
    ; params : params
    }

  let init (params : params) =
    let module Rp = struct
      let k = params.k
      let t = params.t
      let b = params.b
    end
    in
    let module Gp = struct
      let pp = params.prim_poly
      let pe = params.prim_elt
    end
    in
    let module G = Reedsolomon.Galois.Int_table_of_params (Gp) in
    { rp = (module Rp); g = (module G); params }
  ;;

  let name =
    match Decoder.decoder with
    | `euclid -> "euclid"
    | `peterson -> "peterson"
    | `berlekamp -> "berlekamp"
  ;;

  let encode t (message : int array) =
    let module Codec = Reedsolomon.Poly_codec.Make ((val t.g)) ((val t.rp)) in
    let encoded = Codec.encode (Array.rev message) in
    let encoded = Codec.R.slice encoded (t.params.n - 1) in
    Array.rev encoded
  ;;

  let decode t (message : int array) =
    let module Codec = Reedsolomon.Poly_codec.Make ((val t.g)) ((val t.rp)) in
    let decoded =
      (match Decoder.decoder with
      | `euclid -> Codec.decode_euclid
      | `peterson -> Codec.decode_peterson
      | `berlekamp -> Codec.decode_berlekamp_massey)
        (Array.rev message)
    in
    let decoded = Codec.R.slice decoded (t.params.n - 1) in
    Array.rev decoded
  ;;
end

let codec_selection =
  [ (let module Codec =
       Poly_codec (struct
         let decoder = `peterson
       end)
     in
    (module Codec : Codec))
  ; (let module Codec =
       Poly_codec (struct
         let decoder = `euclid
       end)
     in
    (module Codec : Codec))
  ; (let module Codec =
       Poly_codec (struct
         let decoder = `berlekamp
       end)
     in
    (module Codec : Codec))
  ; (module Iter_codec : Codec)
  ]
;;

let params m t =
  let n = (1 lsl m) - 1 in
  let k = n - (2 * t) in
  { Reedsolomon.Iter_codec.n
  ; k
  ; t
  ; m
  ; b = 0
  ; prim_poly = Reedsolomon.Galois.int_of_gf2_prim_poly m
  ; prim_elt = 2
  }
;;

module Bit_buffer : sig
  type t

  val create : unit -> t
  val add : t -> Bits.t -> unit
  val get : t -> int -> Bits.t
  val available : t -> int
end = struct
  type t = Bits.t ref

  let create () = ref Bits.empty

  let add t bits =
    if Bits.is_empty !t then t := bits else t := Bits.concat_msb [ bits; !t ]
  ;;

  let get t nbits =
    let width = Bits.width !t in
    if width < nbits
    then raise_s [%message "Not enough bits left" (width : int) (nbits : int)]
    else if width = nbits
    then (
      let result = !t in
      t := Bits.empty;
      result)
    else (
      let result = Bits.sel_bottom !t nbits in
      t := Bits.drop_bottom !t nbits;
      result)
  ;;

  let available t = Bits.width !t
end

module Io_buffers = struct
  type t =
    { buffer_in : Bit_buffer.t
    ; buffer_out : Bit_buffer.t
    ; get_byte : unit -> Char.t
    ; put_byte : Char.t -> unit
    ; mutable num_bytes_out : int
    ; mutable max_bytes_out : int option
    }

  let create ~get_byte ~put_byte =
    { buffer_in = Bit_buffer.create ()
    ; buffer_out = Bit_buffer.create ()
    ; get_byte =
        (fun () ->
          try get_byte () with
          | _ -> '\000')
    ; put_byte
    ; num_bytes_out = 0
    ; max_bytes_out = None
    }
  ;;

  let set_max_bytes_out t max = t.max_bytes_out <- Some max

  let fill_buffer_in { buffer_in; get_byte; _ } target_level =
    while Bit_buffer.available buffer_in < target_level do
      Bit_buffer.add buffer_in (Bits.of_char (get_byte ()))
    done
  ;;

  let more_to_output { num_bytes_out; max_bytes_out; _ } =
    match max_bytes_out with
    | Some max_bytes_out -> num_bytes_out < max_bytes_out
    | None -> true
  ;;

  let flush_buffer_out ?(force = false) ({ buffer_out; put_byte; _ } as t) =
    while Bit_buffer.available buffer_out >= 8 && more_to_output t do
      put_byte (Bits.to_char (Bit_buffer.get buffer_out 8));
      t.num_bytes_out <- t.num_bytes_out + 1
    done;
    if force
    then
      if Bit_buffer.available buffer_out > 0
      then (
        let bits = Bit_buffer.get buffer_out (Bit_buffer.available buffer_out) in
        let bits = Bits.uresize bits 8 in
        put_byte (Bits.to_char bits);
        t.num_bytes_out <- t.num_bytes_out + 1)
  ;;

  let get_input t ~count ~bits =
    let num_bits = count * bits in
    fill_buffer_in t num_bits;
    let inp = Bit_buffer.get t.buffer_in num_bits in
    let inp = Array.init count ~f:(fun j -> Bits.(to_int inp.:+[bits * j, Some bits])) in
    inp
  ;;

  let put_output t outp ~bits =
    let outp =
      Array.map outp ~f:(Bits.of_int ~width:bits) |> Array.to_list |> Bits.concat_lsb
    in
    Bit_buffer.add t.buffer_out outp;
    flush_buffer_out t
  ;;
end

module Header = struct
  type 'a t =
    { m : 'a [@bits 8]
    ; t : 'a [@bits 8]
    ; total_length_in_bytes : 'a [@bits 64]
    }
  [@@deriving sexp_of, hardcaml]
end

module Encode = struct
  type t =
    { io_buffers : Io_buffers.t
    ; params : Reedsolomon.Iter_codec.params
    ; total_length_in_bytes : int
    ; encoder : int array -> int array
    }

  let header (header : int Header.t) =
    Header.Of_bits.of_ints header |> Header.Of_bits.pack
  ;;

  let create
      ~(params : Reedsolomon.Iter_codec.params)
      (io_buffers : Io_buffers.t)
      ~total_length_in_bytes
      ~encoder
    =
    Bit_buffer.add
      io_buffers.buffer_out
      (header { Header.m = params.m; t = params.t; total_length_in_bytes });
    { io_buffers; params; total_length_in_bytes; encoder }
  ;;

  let get_message { io_buffers; params = { m; k; _ }; _ } =
    Io_buffers.get_input io_buffers ~count:k ~bits:m
  ;;

  let put_codeword { io_buffers; params = { m; _ }; _ } codeword =
    Io_buffers.put_output io_buffers codeword ~bits:m
  ;;

  let encode t =
    let bits_encoded = ref 0 in
    while !bits_encoded < t.total_length_in_bytes * 8 do
      let message = get_message t in
      let codeword = t.encoder message in
      assert (Array.length codeword = t.params.n);
      put_codeword t codeword;
      bits_encoded := !bits_encoded + (t.params.m * t.params.k)
    done;
    Io_buffers.flush_buffer_out ~force:true t.io_buffers
  ;;
end

module Decode = struct
  type t =
    { io_buffers : Io_buffers.t
    ; mutable params : Reedsolomon.Iter_codec.params
    ; mutable total_length_in_bytes : int
    ; mutable decoder : int array -> int array
    }

  let read_header ({ io_buffers = { buffer_in; _ }; _ } as dec) =
    Io_buffers.fill_buffer_in dec.io_buffers Header.sum_of_port_widths;
    let { Header.m; t; total_length_in_bytes } =
      Bit_buffer.get buffer_in Header.sum_of_port_widths
      |> Header.Of_bits.unpack
      |> Header.map ~f:Bits.to_int
    in
    dec.params <- params m t;
    dec.total_length_in_bytes <- total_length_in_bytes;
    Io_buffers.set_max_bytes_out dec.io_buffers total_length_in_bytes
  ;;

  let create io_buffers ~decoder =
    let t =
      { io_buffers
      ; params =
          { Reedsolomon.Iter_codec.m = 0
          ; k = 0
          ; t = 0
          ; b = 0
          ; n = 0
          ; prim_poly = 0
          ; prim_elt = 0
          }
      ; total_length_in_bytes = 0
      ; decoder = (fun _ -> raise_s [%message "decoder not ready"])
      }
    in
    read_header t;
    t.decoder <- decoder t.params;
    t
  ;;

  let get_codeword { io_buffers; params = { n; m; _ }; _ } =
    Io_buffers.get_input io_buffers ~count:n ~bits:m
  ;;

  let put_message { io_buffers; params = { m; _ }; _ } message =
    Io_buffers.put_output io_buffers message ~bits:m
  ;;

  let decode t =
    let bits_decoded = ref 0 in
    while !bits_decoded < t.total_length_in_bytes * 8 do
      let codeword = get_codeword t in
      let message = t.decoder codeword in
      assert (Array.length message = t.params.n);
      let message = Array.subo message ~len:t.params.k in
      put_message t message;
      bits_decoded := !bits_decoded + (t.params.m * t.params.k)
    done
  ;;
end

module Testing = struct
  let sexp_of_int_char_array a = [%sexp (Array.map a ~f:Char.to_int : int array)]

  let test ~verbose ~m ~t ~total_bytes ~encoder ~decoder =
    let params = params m t in
    let message = Array.init total_bytes ~f:(fun _ -> Random.char ()) in
    let get_byte data =
      let cnt = ref 0 in
      let get_byte () =
        let r = data.(!cnt) in
        Int.incr cnt;
        r
      in
      get_byte
    in
    let put_byte () =
      let data = ref [] in
      let put_byte e = data := e :: !data in
      put_byte, fun () -> List.rev !data |> Array.of_list
    in
    let encoder = encoder params in
    let enc, codeword =
      let get_byte = get_byte message in
      let put_byte, codeword = put_byte () in
      let encoder =
        Encode.create
          ~params
          (Io_buffers.create ~get_byte ~put_byte)
          ~total_length_in_bytes:(Array.length message)
          ~encoder
      in
      encoder, codeword
    in
    Encode.encode enc;
    let codeword = codeword () in
    let dec, decoded_message =
      let get_byte = get_byte codeword in
      let put_byte, decoded_message = put_byte () in
      let decoder = Decode.create (Io_buffers.create ~get_byte ~put_byte) ~decoder in
      decoder, decoded_message
    in
    Decode.decode dec;
    let decoded_message = decoded_message () in
    let msg () =
      [%message
        (m : int)
          (t : int)
          (total_bytes : int)
          (message : int_char_array)
          (codeword : int_char_array)
          (decoded_message : int_char_array)]
    in
    if verbose
    then print_s (msg ())
    else if not ([%compare.equal: char array] message decoded_message)
    then raise_s (msg ())
  ;;

  (* Pass data through a psuedo encode/decode process.   We are mainly looking 
   at reasonable bit packing here.  We only check the inputs and outputs are
   ok, but dont look at the inserted parity bytes at all. *)
  let%expect_test "test harness" =
    let encoder (params : Reedsolomon.Iter_codec.params) x =
      Array.concat [ x; Array.create ~len:(params.t * 2) 0xe ]
    in
    let decoder _ = Fn.id in
    for m = 3 to 8 do
      for t = 1 to min 16 (max 1 (1 lsl m) / 4) do
        for _ = 1 to 10 do
          let total_bytes = Random.int 1000 + 1 in
          test ~verbose:false ~m ~t ~total_bytes ~encoder ~decoder
        done
      done
    done
  ;;

  let%expect_test "with coding" =
    let encoder (params : params) =
      let encoder = Iter_codec.init params in
      Iter_codec.encode encoder
    in
    let decoder params =
      let decoder = Iter_codec.init params in
      Iter_codec.decode decoder
    in
    for m = 3 to 8 do
      for t = 1 to min 16 (max 1 (1 lsl m) / 4) do
        for _ = 1 to 2 do
          let total_bytes = Random.int 1000 + 1 in
          test ~verbose:false ~m ~t ~total_bytes ~encoder ~decoder
        done
      done
    done
  ;;

  let%expect_test "with errors" =
    let random_errors (params : params) ~num_errors =
      let errors =
        Array.init params.n ~f:(fun i ->
            if i < num_errors then 1 + Random.int ((1 lsl params.m) - 1) else 0)
      in
      Array.permute errors;
      errors
    in
    let add_errors (params : params) codeword =
      let errors = random_errors params ~num_errors:params.t in
      Array.map2_exn codeword errors ~f:( lxor )
    in
    let encoder (params : params) =
      let encoder = Iter_codec.init params in
      fun x -> Iter_codec.encode encoder x |> add_errors params
    in
    let decoder params =
      let decoder = Iter_codec.init params in
      Iter_codec.decode decoder
    in
    for m = 3 to 8 do
      for t = 1 to min 16 (max 1 (1 lsl m) / 4) do
        for _ = 1 to 20 do
          let total_bytes = Random.int 1000 + 1 in
          test ~verbose:false ~m ~t ~total_bytes ~encoder ~decoder
        done
      done
    done
  ;;
end
