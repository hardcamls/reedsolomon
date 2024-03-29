open Core
open Hardcaml
open Hardcaml_waveterm

module Test
    (Standard : Reedsolomon.Standards.Standard)
    (N : Hardcaml_reedsolomon.Parallelism.S) =
struct
  include Util.Make (Standard)
  module Decoder = Hw.Decoder (N)
  module Syndromes = Decoder.Syndromes
  module Sim = Cyclesim.With_interface (Syndromes.I) (Syndromes.O)

  let display_rules =
    let module I = Display_rules.With_interface (Syndromes.I) in
    let module O = Display_rules.With_interface (Syndromes.O) in
    List.concat [ I.default (); O.default () ]
  ;;

  let cycles_per_codeword = (n + N.n - 1) / N.n
  let offset = (cycles_per_codeword * N.n) - n

  let test_codeword ?(verbose = false) ?waves received =
    let sim =
      Sim.create (Syndromes.create ~scale:offset (Scope.create ~flatten_design:true ()))
    in
    let waves, sim = waveform_opt ?waves sim in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim in
    (* reset, clear, enable *)
    Cyclesim.reset sim;
    i.enable := Bits.vdd;
    i.clocking.clear := Bits.vdd;
    Cyclesim.cycle sim;
    i.clocking.clear := Bits.gnd;
    let recv = Array.concat [ rev received; Array.init offset ~f:(fun _ -> 0) ] in
    (* load received data *)
    i.first := Bits.vdd;
    for j = 0 to cycles_per_codeword - 1 do
      for k = 0 to N.n - 1 do
        i.x.(k) := Bits.of_int ~width:m recv.((j * N.n) + k)
      done;
      if j = cycles_per_codeword - 1 then i.last := Bits.vdd;
      Cyclesim.cycle sim;
      i.first := Bits.gnd;
      i.last := Bits.gnd
    done;
    (* wait for valid *)
    while Bits.to_int !(o.valid) = 0 do
      Cyclesim.cycle sim
    done;
    (* compare *)
    let syndromes_sw = syndromes received in
    let syndromes_tb = Array.map ~f:(fun x -> Bits.to_int !x) o.syndromes in
    let msg () =
      [%message
        (received : Sw.R.poly) (syndromes_sw : Sw.R.poly) (syndromes_tb : Sw.R.poly)]
    in
    if verbose
    then print_s (msg ())
    else if not ([%compare.equal: int array] syndromes_sw syndromes_tb)
    then raise_s (msg ());
    waves
  ;;

  let test ?waves () =
    let codeword = codeword (message ()) in
    let error = error 2 in
    let received = codeword ^. error in
    test_codeword ?waves received
  ;;
end

let test ?waves parallelism =
  let module Test =
    Test
      (Reedsolomon.Standards.BBCTest)
      (struct
        let n = parallelism
      end)
  in
  Test.test ?waves ()
;;

let%expect_test "syndromes - 1 symbol/cycle" =
  ignore (test 1 : _ option);
  [%expect]
;;

let%expect_test "syndromes - 3 symbols/cycle" =
  ignore (test 3 : _ option);
  [%expect]
;;

let%expect_test "syndromes - 6 symbols/cycle" =
  ignore (test 6 : _ option);
  [%expect]
;;
