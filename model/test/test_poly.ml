open Core
open Reedsolomon
module I = Poly.Make (Ops.Int)

let%expect_test "constants" =
  print_s [%message (I.zero : I.t) (I.one : I.t) (I.x : I.t)];
  [%expect {| ((I.zero (0)) (I.one (1)) (I.x (0 1))) |}]
;;

let%expect_test "create" =
  print_s [%message (I.of_poly [| 2 |] : I.t)];
  print_s [%message (I.of_poly [| 0; 3 |] : I.t)];
  print_s [%message (I.of_poly [| 1; 5; 0 |] : I.t)];
  [%expect
    {|
    ("I.of_poly [|2|]" (2))
    ("I.of_poly [|0;3|]" (0 3))
    ("I.of_poly [|1;5;0|]" (1 5 0)) |}]
;;

let%expect_test "to_string" =
  printf "%s" (I.to_string (I.of_poly [| 1; 3; 5 |]));
  [%expect {| 5.x^2 + 3.x + 1 |}];
  printf "%s" (I.to_string ~down:false (I.of_poly [| 1; 3; 5 |]));
  [%expect {| 1 + 3.x + 5.x^2 |}]
;;

let%expect_test "trim" =
  print_s [%message (I.trim (I.to_poly [| 1; 1; 1; 0; 0 |]) : I.t)];
  print_s [%message (I.trim (I.to_poly [| 0; 0 |]) : I.t)];
  print_s [%message (I.trim (I.to_poly [| 0 |]) : I.t)];
  [%expect
    {|
    ("I.trim (I.to_poly [|1;1;1;0;0|])" (1 1 1))
    ("I.trim (I.to_poly [|0;0|])" (0))
    ("I.trim (I.to_poly [|0|])" (0)) |}]
;;

let%expect_test "slice" =
  print_s [%message (I.slice (I.to_poly [| 1; 2; 3; 4 |]) 0 : I.t)];
  print_s [%message (I.slice (I.to_poly [| 1; 2; 3; 4 |]) 1 : I.t)];
  print_s [%message (I.slice (I.to_poly [| 1; 2; 3; 4 |]) 2 : I.t)];
  print_s [%message (I.slice (I.to_poly [| 1; 2; 3; 4 |]) 3 : I.t)];
  [%expect
    {|
    ("I.slice (I.to_poly [|1;2;3;4|]) 0" (1))
    ("I.slice (I.to_poly [|1;2;3;4|]) 1" (1 2))
    ("I.slice (I.to_poly [|1;2;3;4|]) 2" (1 2 3))
    ("I.slice (I.to_poly [|1;2;3;4|]) 3" (1 2 3 4)) |}]
;;

let%expect_test "maths" =
  let a = I.to_poly [| 1; 2; 3 |] in
  let b = I.to_poly [| 3; 0; 3 |] in
  print_s [%message (I.( +: ) a b : I.t)];
  print_s [%message (I.( -: ) a b : I.t)];
  [%expect {|
    ("I.(+:) a b" (4 2 6))
    ("I.(-:) a b" (-2 2)) |}];
  print_s [%message (I.( *: ) a b : I.t)];
  print_s [%message (I.( /: ) b b : I.t * I.t)];
  [%expect {|
    ("I.( *: ) a b" (3 6 12 6 9))
    ("I.(/:) b b" ((1) (0))) |}];
  print_s [%message (I.( ^: ) a 3 : I.t)];
  [%expect {| ("I.(^:) a 3" (0 0 0 1 2 3)) |}];
  print_s [%message (I.( **: ) a 3 : I.t)];
  [%expect {| ("I.( **: ) a 3" (1 6 21 44 63 54 27)) |}]
;;
