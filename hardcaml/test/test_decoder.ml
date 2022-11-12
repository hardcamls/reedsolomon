open Core
open Hardcaml
open Hardcaml_waveterm
open Util

module Test (N : Parallelism) = struct
  module Decoder = Hw.Decoder (N)
  module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O)

  let display_rules =
    let module I = Display_rules.With_interface (Decoder.I) in
    let module O = Display_rules.With_interface (Decoder.O) in
    List.concat [ I.default (); O.default () ]
  ;;

  let test ?waves () =
    let sim = Sim.create (Decoder.create (Scope.create ~flatten_design:true ())) in
    let waves, sim = waveform_opt ?waves sim in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs ~clock_edge:Before sim in
    let codeword = codeword (message ()) in
    let error = error 2 in
    let received = codeword ^. error in
    let cycles_per_codeword = (n + N.n - 1) / N.n in
    let offset = (cycles_per_codeword * N.n) - n in
    Cyclesim.reset sim;
    i.enable := Bits.vdd;
    i.clocking.clear := Bits.vdd;
    Cyclesim.cycle sim;
    i.clocking.clear := Bits.gnd;
    let recv = Array.concat [ rev received; Array.init offset ~f:(fun _ -> 0) ] in
    (* load received data *)
    i.first := Bits.vdd;
    i.load := Bits.vdd;
    for j = 0 to cycles_per_codeword - 1 do
      for k = 0 to N.n - 1 do
        i.x.(k) := Bits.of_int ~width:sbits recv.((j * N.n) + k)
      done;
      if j = cycles_per_codeword - 1 then i.last := Bits.vdd;
      Cyclesim.cycle sim;
      i.first := Bits.gnd;
      i.last := Bits.gnd
    done;
    i.load := Bits.gnd;
    let ocnt = ref 0 in
    let corrected = Array.init (cycles_per_codeword * N.n) ~f:(fun _ -> 0) in
    while !ocnt < cycles_per_codeword do
      Cyclesim.cycle sim;
      if Bits.to_int !(o.ordy) <> 0
      then (
        for k = 0 to N.n - 1 do
          corrected.((!ocnt * N.n) + k) <- Bits.to_int !(o.corrected.(k))
        done;
        incr ocnt)
    done;
    let corrected = Sw.R.R.slice corrected (n - 1) in
    Cyclesim.cycle sim;
    if not ([%compare.equal: int array] corrected (rev codeword))
    then raise_s [%message (rev codeword : Sw.R.poly) (corrected : Sw.R.poly)];
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

let%expect_test "parallelism 1 cycle/code word" =
  ignore (test 1 : _ option);
  [%expect {||}]
;;

let%expect_test "parallelism 2 cycles/code word" =
  ignore (test 4 : _ option);
  [%expect {||}]
;;
