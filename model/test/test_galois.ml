open Core
open Reedsolomon

let%expect_test "primitive fields" =
  let n = 7 in
  let module Gf =
    Galois.Primitive.GFN (struct
      let n = n
    end)
  in
  let add = Array.init n ~f:(fun a -> Array.init n ~f:(fun b -> Gf.( +: ) a b)) in
  print_s [%message (add : Gf.t array array)];
  [%expect {|
    (add
     ((0 1 2 3 4 5 6) (1 2 3 4 5 6 0) (2 3 4 5 6 0 1) (3 4 5 6 0 1 2)
      (4 5 6 0 1 2 3) (5 6 0 1 2 3 4) (6 0 1 2 3 4 5))) |}];
  let mul = Array.init n ~f:(fun a -> Array.init n ~f:(fun b -> Gf.( *: ) a b)) in
  print_s [%message (mul : Gf.t array array)];
  [%expect {|
    (mul
     ((0 0 0 0 0 0 0) (0 1 2 3 4 5 6) (0 2 4 6 1 3 5) (0 3 6 2 5 1 4)
      (0 4 1 5 2 6 3) (0 5 3 1 6 4 2) (0 6 5 4 3 2 1))) |}];
  let inv = Array.init n ~f:(fun a -> Gf.(one /: a)) in
  print_s [%message (inv : Gf.t array)];
  [%expect {| (inv (0 1 4 5 2 3 6)) |}];
  let div = Array.init n ~f:(fun a -> Gf.(a /: a)) in
  print_s [%message (div : Gf.t array)];
  [%expect {| (div (0 1 1 1 1 1 1)) |}]
;;

let%expect_test "binary field" =
  let n = 2 in
  let module Gf = Galois.Primitive.GF2 in
  let add = Array.init n ~f:(fun a -> Array.init n ~f:(fun b -> Gf.( +: ) a b)) in
  print_s [%message (add : Gf.t array array)];
  [%expect {|
    (add ((0 1) (1 0))) |}];
  let mul = Array.init n ~f:(fun a -> Array.init n ~f:(fun b -> Gf.( *: ) a b)) in
  print_s [%message (mul : Gf.t array array)];
  [%expect {|
    (mul ((0 0) (0 1))) |}];
  let inv = Array.init n ~f:(fun a -> Gf.(one /: a)) in
  print_s [%message (inv : Gf.t array)];
  [%expect {| (inv (0 1)) |}];
  let div = Array.init n ~f:(fun a -> Gf.(a /: a)) in
  print_s [%message (div : Gf.t array)];
  [%expect {| (div (0 1)) |}]
;;
