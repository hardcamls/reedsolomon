open Core
open Reedsolomon
module I = Matrix.Make (Ops.Int)

let zero = I.create 2 2
let identity = I.identity 2
let m3x2 = I.init 2 3 (fun row col -> (row * 4) + col)
let m4x5 = I.init 4 5 (fun row col -> (row * 5) + col)

let%expect_test "construct" =
  print_s [%message (zero : I.matrix) (identity : I.matrix)];
  print_s [%message (m3x2 : I.matrix) (m4x5 : I.matrix)];
  [%expect
    {|
    ((zero ((0 0) (0 0))) (identity ((1 0) (0 1))))
    ((m3x2 ((0 1 2) (4 5 6)))
     (m4x5 ((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19)))) |}];
  print_s
    [%message
      (I.row_vector [| 1; 2; 3 |] : I.matrix) (I.col_vector [| 1; 2; 3 |] : I.matrix)];
  [%expect
    {|
    (("I.row_vector [|1;2;3|]" ((1 2 3)))
     ("I.col_vector [|1;2;3|]" ((1) (2) (3)))) |}]
;;

let%expect_test "transpose" =
  print_s [%message (I.transpose m3x2 : I.matrix) (I.transpose m4x5 : I.matrix)];
  [%expect
    {|
    (("I.transpose m3x2" ((0 4) (1 5) (2 6)))
     ("I.transpose m4x5"
      ((0 5 10 15) (1 6 11 16) (2 7 12 17) (3 8 13 18) (4 9 14 19)))) |}]
;;

let%expect_test "math" =
  print_s [%message (I.( +: ) m3x2 m3x2 : I.matrix)];
  [%expect {| ("I.(+:) m3x2 m3x2" ((0 2 4) (8 10 12))) |}];
  print_s [%message (I.( -: ) m3x2 m3x2 : I.matrix)];
  [%expect {| ("I.(-:) m3x2 m3x2" ((0 0 0) (0 0 0))) |}];
  print_s [%message (I.( *: ) m3x2 (I.transpose m3x2) : I.matrix)];
  [%expect {| ("I.( *: ) m3x2 (I.transpose m3x2)" ((5 17) (17 77))) |}];
  print_s [%message (I.( *:. ) m3x2 10 : I.matrix)];
  [%expect {| ("I.( *:. ) m3x2 10" ((0 10 20) (40 50 60))) |}]
;;

let%expect_test "sub" =
  print_s [%message (I.sub 1 2 2 2 m4x5 : I.matrix)];
  [%expect {| ("I.sub 1 2 2 2 m4x5" ((7 8) (12 13))) |}]
;;

let%expect_test "map" =
  print_s [%message (I.map (( + ) 1) m3x2 : I.matrix)];
  [%expect {| ("I.map ((+) 1) m3x2" ((1 2 3) (5 6 7))) |}];
  print_s [%message (I.map2 ( + ) m3x2 m3x2 : I.matrix)];
  [%expect {| ("I.map2 (+) m3x2 m3x2" ((0 2 4) (8 10 12))) |}]
;;

(* TODO: Test determinant, gauss jordan elimination, row operations. *)
