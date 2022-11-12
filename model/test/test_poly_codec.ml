open Core
open Reedsolomon
module Codec = Standards.BBCTest

let n = Codec.Rp.k + (2 * Codec.Rp.t)
let field_size = n + 1
let () = assert (field_size = 16)
let input = Array.init Codec.Rp.k ~f:(fun _ -> Random.int field_size)
let coded = Codec.R.encode input

let%expect_test "encode" =
  print_s [%message (coded : int array)];
  [%expect {| (coded (1 15 10 11 0 13 6 9 7 12 8 12 1 2 1)) |}]
;;

let add_errors e =
  let e =
    Array.init n ~f:(fun i -> if i < e then 1 + Random.int (field_size - 1) else 0)
  in
  Array.permute e;
  Array.map2_exn coded e ~f:( lxor )
;;

let decode f num_errors =
  let errored = add_errors num_errors in
  let decoded = f errored in
  if not ([%compare.equal: int array] decoded coded)
  then raise_s [%message (coded : int array) (errored : int array) (decoded : int array)]
;;

let%expect_test "0 errors peterson" = decode Codec.R.decode_peterson 0
let%expect_test "1 errors peterson" = decode Codec.R.decode_peterson 1
let%expect_test "2 errors peterson" = decode Codec.R.decode_peterson 2

let%expect_test "3 errors peterson" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    decode Codec.R.decode_peterson 3);
  [%expect
    {|
    ((coded (1 15 10 11 0 13 6 9 7 12 8 12 1 2 1))
     (errored (1 15 4 11 0 13 11 9 7 12 8 12 1 2 11))
     (decoded (1 15 4 11 0 13 11 9 7 12 8 12 1 2 11))) |}]
;;

let%expect_test "0 errors euclid" = decode Codec.R.decode_euclid 0
let%expect_test "1 errors euclid" = decode Codec.R.decode_euclid 1
let%expect_test "2 errors euclid" = decode Codec.R.decode_euclid 2

let%expect_test "3 errors euclid" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    decode Codec.R.decode_euclid 3);
  [%expect
    {|
    ((coded (1 15 10 11 0 13 6 9 7 12 8 12 1 2 1))
     (errored (1 15 4 11 0 13 11 9 7 12 8 12 1 2 11))
     (decoded (1 15 4 11 0 13 11 9 7 12 8 12 1 2 11))) |}]
;;

let%expect_test "0 errors berlekamp massey" = decode Codec.R.decode_berlekamp_massey 0
let%expect_test "1 errors berlekamp massey" = decode Codec.R.decode_berlekamp_massey 1
let%expect_test "2 errors berlekamp massey" = decode Codec.R.decode_berlekamp_massey 2

let%expect_test "3 errors berlekamp massey" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    decode Codec.R.decode_berlekamp_massey 3);
  [%expect
    {|
    ((coded (1 15 10 11 0 13 6 9 7 12 8 12 1 2 1))
     (errored (1 15 4 11 0 13 11 9 7 12 8 12 1 2 11))
     (decoded (1 15 4 11 0 13 11 9 7 12 8 12 1 2 11))) |}]
;;
