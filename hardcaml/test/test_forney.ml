open Core
open Hardcaml
open Hardcaml_waveterm
open Util

(* XXX generalize this testbench. *)

module Test (N : Parallelism) = struct
  module Decoder = Hw.Decoder (N)
  module Forney = Decoder.Forney.Parallel
  module Sim = Cyclesim.With_interface (Forney.I) (Forney.O)

  let display_rules =
    let module I = Display_rules.With_interface (Forney.I) in
    let module O = Display_rules.With_interface (Forney.O) in
    List.concat [ I.default (); O.default () ]
  ;;

  let test ?waves () =
    let sim = Sim.create (Forney.create (Scope.create ~flatten_design:true ())) in
    let waves, sim = waveform_opt ?waves sim in
    let i = Cyclesim.inputs sim in
    let _o = Cyclesim.outputs ~clock_edge:Before sim in
    let codeword = codeword (message ()) in
    let _ = error 2 in
    let _ = error 2 in
    let error = error 2 in
    let received = codeword ^. error in
    let syndromes = syndromes received in
    let v, l = berlekamp syndromes in
    let l' = Array.init ((t + 1) / 2) ~f:(fun i -> l.((i * 2) + 1)) in
    (* derivative of l, if evaluated at x^2 *)
    let ch = Array.of_list @@ chien l in
    let fy = Array.map ~f:(Sw.R.Sarwate.forney v l) ch in
    Printf.printf "c: ";
    dump codeword;
    Printf.printf "e: ";
    dump error;
    Printf.printf "r: ";
    dump received;
    Printf.printf "s: ";
    dump syndromes;
    Printf.printf "w: ";
    dump v;
    Printf.printf "l: ";
    dump l;
    Printf.printf "': ";
    dump l';
    Printf.printf "n: ";
    dump ch;
    Printf.printf "f: ";
    dump fy;
    (* forney debug gubbins *)
    Printf.printf
      "_: %i %i %i %i\n"
      Sw.G.(antilog 3)
      Sw.G.(antilog 3 **: 2)
      Sw.G.(antilog 9)
      Sw.G.(antilog 9 **: 2);
    Printf.printf
      "_: [v=%i l'=%i] [v=%i l'=%i] \n"
      (Sw.R.horner v Sw.G.(antilog 3))
      (Sw.R.horner l' Sw.G.(antilog 3 **: 2))
      (Sw.R.horner v Sw.G.(antilog 9))
      (Sw.R.horner l' Sw.G.(antilog 9 **: 2));
    Cyclesim.reset sim;
    i.enable := Bits.vdd;
    i.clocking.clear := Bits.vdd;
    Cyclesim.cycle sim;
    i.clocking.clear := Bits.gnd;
    for j = 0 to Array.length v - 1 do
      i.v.(j) := Bits.of_int ~width:sbits v.(j)
    done;
    for j = 0 to Array.length l' - 1 do
      i.l.(j) := Bits.of_int ~width:sbits l'.(j)
    done;
    for j = 0 to Array.length ch - 1 do
      i.x.(0) := Bits.of_int ~width:sbits ch.(j);
      Cyclesim.cycle sim
    done;
    (* some flush cycles *)
    for _ = 0 to 4 do
      Cyclesim.cycle sim
    done;
    waves
  ;;
end

let test ?waves parallelism =
  let module Test =
    Test (struct
      let n = parallelism
    end)
  in
  Test.test ?waves ()
;;

let%expect_test "forney - 1 symbol/cycle" =
  ignore (test 1 : _ option);
  [%expect
    {|
    c:  5 14 11  0 14 12  4  2  7  7  6  3  5  0  4
    e:  0  0  0  0  0  0  0  0  0 10  0  0 11  0  0
    r:  5 14 11  0 14 12  4  2  7 13  6  3 14  0  4
    s:  1 11 13  1
    w: 11  5
    l:  4  7  5
    ':  7
    n:  3  6
    f: 11 10
    _: 8 12 10 8
    _: [v=5 l'=7] [v=15 l'=7] |}]
;;
