open Core
open Hardcaml
open Util.Basic
module Gh = Gfb
module Gs = Hw.Gfh.G

let gh = Bits.of_int ~width:4

let%expect_test "powers of alpha" =
  let a = Array.init 16 ~f:(fun i -> Gs.(alpha **: i)) in
  print_s [%message (a : Gs.t array)];
  [%expect {| (a (1 2 4 8 3 6 12 11 5 10 7 14 15 13 9 1)) |}];
  let a = Array.init 16 ~f:(fun i -> Gh.(alpha **: gh i) |> Bits.to_int) in
  print_s [%message (a : int array)];
  [%expect {| (a (1 2 4 8 3 6 12 11 5 10 7 14 15 13 9 1)) |}]
;;

let%expect_test "log and anitlog tables" =
  let log = Array.init 16 ~f:(fun i -> Gh.log (gh i) |> Bits.to_int) in
  let antilog = Array.init 16 ~f:(fun i -> Gh.antilog (gh i) |> Bits.to_int) in
  print_s [%message (log : int array) (antilog : int array)];
  [%expect
    {|
    ((log (0 0 1 4 2 8 5 10 3 14 9 7 6 13 11 12))
     (antilog (1 2 4 8 3 6 12 11 5 10 7 14 15 13 9 1))) |}]
;;

let%expect_test "add" =
  for i = 0 to 15 do
    for j = 0 to 15 do
      let sw = Gs.(i +: j) in
      let hw = Gh.(gh i +: gh j |> Bits.to_int) in
      if sw <> hw then raise_s [%message (i : int) (j : int) (sw : int) (hw : int)]
    done
  done;
  [%expect]
;;

let%expect_test "sub" =
  for i = 0 to 15 do
    for j = 0 to 15 do
      let sw = Gs.(i -: j) in
      let hw = Gh.(gh i -: gh j |> Bits.to_int) in
      if sw <> hw then raise_s [%message (i : int) (j : int) (sw : int) (hw : int)]
    done
  done;
  [%expect]
;;

let%expect_test "mul" =
  for i = 0 to 15 do
    for j = 0 to 15 do
      let sw = Gs.(i *: j) in
      let hw = Gh.(gh i *: gh j |> Bits.to_int) in
      if sw <> hw then raise_s [%message (i : int) (j : int) (sw : int) (hw : int)]
    done
  done;
  [%expect]
;;

let%expect_test "cmul" =
  for i = 0 to 15 do
    for j = 0 to 15 do
      let sw = Gs.(i *: j) in
      let hw = Gh.(cmul i (gh j) |> Bits.to_int) in
      if sw <> hw then raise_s [%message (i : int) (j : int) (sw : int) (hw : int)]
    done
  done;
  [%expect]
;;

let%expect_test "div" =
  for i = 0 to 15 do
    for j = 1 to 15 do
      let sw = Gs.(i /: j) in
      let hw = Gh.(gh i /: gh j |> Bits.to_int) in
      if sw <> hw then raise_s [%message (i : int) (j : int) (sw : int) (hw : int)]
    done
  done;
  [%expect]
;;

let%expect_test "pow" =
  for i = 1 to 15 do
    for j = 0 to 15 do
      let sw = Gs.(i **: j) in
      let hw = Gh.(gh i **: gh j |> Bits.to_int) in
      if sw <> hw then raise_s [%message (i : int) (j : int) (sw : int) (hw : int)]
    done
  done;
  [%expect {||}]
;;

let%expect_test "cpow" =
  for i = 1 to 15 do
    for j = 0 to 15 do
      let sw = Gs.(i **: j) in
      let hw = Gh.(cpow (gh i) j |> Bits.to_int) in
      if sw <> hw then raise_s [%message (i : int) (j : int) (sw : int) (hw : int)]
    done
  done;
  [%expect {||}]
;;

let%expect_test "inv" =
  for i = 0 to 15 do
    let sw = Gs.(one /: i) in
    let hw = Gh.(inv (gh i) |> Bits.to_int) in
    if sw <> hw then raise_s [%message (i : int) (sw : int) (hw : int)]
  done;
  [%expect]
;;
