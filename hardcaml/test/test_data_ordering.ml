(* We should have thought about this properly from the start, however, the 
   various codec implementations comsume and produce data in very differnt 
   orders which is very annoying to deal with thoughout the code base. 
   
   Lets look here at the various orderings and figure out the something 
   appropriate.  We should probably try to unify around what the hardware
   requirements are if possible.
   *)
open Core

let params =
  { Reedsolomon.Iter_codec.n = 15
  ; k = 11
  ; t = 2
  ; m = 4
  ; b = 0
  ; prim_poly = Reedsolomon.Galois.int_of_gf2_prim_poly 4
  ; prim_elt = 2
  }
;;

module Standard = (val Reedsolomon.Iter_codec.to_standard params)
module Poly = Reedsolomon.Poly_codec.Make (Standard.G) (Standard.Rp)
module Iter = Reedsolomon.Iter_codec

(* Our input message is 1,2,...k-1 *)
let message = Array.init params.k ~f:(fun i -> i + 1)

let%expect_test "poly - our message is multiplied by x^(2*t) and the parity bytes go at \
                 the bottom of the polynomial"
  =
  let codeword = Poly.encode message in
  print_s [%message (codeword : Standard.R.R.t)];
  [%expect {| (codeword (1 8 5 12 1 2 3 4 5 6 7 8 9 10 11)) |}];
  let poly = Standard.R.R.to_string codeword in
  print_s [%message (poly : string)];
  [%expect
    {|
    (poly
     "11.x^14 + 10.x^13 + 9.x^12 + 8.x^11 + 7.x^10 + 6.x^9 + 5.x^8 + 4.x^7 + 3.x^6 + 2.x^5 + x^4 + 12.x^3 + 5.x^2 + 8.x + 1") |}]
;;

let%expect_test "iter - generate parity bytes - relative to the poly codec, the message \
                 must be reversed but the parity can be placed after the message which I \
                 think is what we want."
  =
  let codec = Iter.init params in
  let message = Array.rev message in
  let parity = Array.init (params.t * 2) ~f:(Fn.const 0) in
  Iter.encode codec message parity;
  print_s [%message (parity : int array)];
  [%expect {| (parity (12 5 8 1)) |}];
  let codeword = Array.concat [ message; parity ] in
  let received = Array.copy codeword in
  received.(3) <- codeword.(3) lxor 0xf;
  received.(11) <- codeword.(11) lxor 0xf;
  let corrected = Array.init params.n ~f:(Fn.const 0) in
  let errors = Iter.decode codec received corrected in
  print_s
    [%message
      (codeword : int array) (received : int array) (corrected : int array) (errors : int)];
  [%expect
    {|
    ((codeword (11 10 9 8 7 6 5 4 3 2 1 12 5 8 1))
     (received (11 10 9 7 7 6 5 4 3 2 1 3 5 8 1))
     (corrected (11 10 9 8 7 6 5 4 3 2 1 12 5 8 1)) (errors 2)) |}]
;;

let%expect_test "hardware codec - the encoder testbench reverses the input message and \
                 reverses the output parity.   The decoder testbench reverses the \
                 received codeword and outputs in normal order.  Both of these are very \
                 odd."
  =
  let module Encoder = Test_encoder.Test (Standard) in
  let codeword_poly = Poly.encode message in
  let t = Encoder.create_and_reset () in
  let parity = Encoder.simulate_message_in_not_crazy_order t (Array.rev message) in
  print_s [%message (parity : int array) (codeword_poly : int array)];
  [%expect {| ((parity (12 5 8 1)) (codeword_poly (1 8 5 12 1 2 3 4 5 6 7 8 9 10 11))) |}];
  let module Decoder =
    Test_decoder.Test
      (Standard)
      (struct
        let n = 1
      end)
  in
  let t = Decoder.create_and_reset () in
  let received = Array.concat [ Array.rev message; parity ] in
  received.(3) <- received.(3) lxor 0xf;
  received.(11) <- received.(11) lxor 0xf;
  let received = Decoder.simulate_codeword_in_not_crazy_order t received in
  print_s [%message (received : int array)];
  [%expect {| (received (11 10 9 8 7 6 5 4 3 2 1 12 5 8 1)) |}]
;;
