open Core
open Hardcaml
open Hardcaml_waveterm
open Util.Basic

module Test (N : Hardcaml_reedsolomon.Parallelism.S) = struct
  module Decoder = Hw.Decoder (N)
  module Chien = Decoder.Chien
  module Sim = Cyclesim.With_interface (Chien.I) (Chien.O)

  let display_rules =
    let module I = Display_rules.With_interface (Chien.I) in
    let module O = Display_rules.With_interface (Chien.O) in
    List.concat [ I.default (); O.default () ]
  ;;

  let test ?waves () =
    let sim = Sim.create (Chien.create (Scope.create ~flatten_design:true ())) in
    let waves, sim = waveform_opt ?waves sim in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs ~clock_edge:Before sim in
    let codeword = codeword (message ()) in
    let error = error 2 in
    (* XXX this seems to the the akward case for the testbench.  It breaks the parallelism=6 case for some strange reason. *)
    (* let error = [| 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1 |] in *)
    let received = codeword ^. error in
    let syndromes = syndromes received in
    let v, l = berlekamp syndromes in
    let ch = chien l in
    let el = List.map ~f:err_loc ch in
    Cyclesim.reset sim;
    i.enable := Bits.vdd;
    i.clocking.clear := Bits.vdd;
    Cyclesim.cycle sim;
    i.clocking.clear := Bits.gnd;
    i.start := Bits.vdd;
    for j = 0 to Array.length l - 1 do
      i.lambda.(j) := Bits.of_int ~width:sbits l.(j)
    done;
    let results = ref [] in
    for _ = 0 to ((n_elems + N.n) / N.n) - 1 do
      for p = 0 to N.n - 1 do
        if Bits.is_vdd !(o.eerr.(p)) then results := Bits.to_int !(o.eloc.(p)) :: !results
      done;
      Cyclesim.cycle sim;
      i.start := Bits.gnd
    done;
    for _ = 0 to 3 do
      Cyclesim.cycle sim
    done;
    if not ([%compare.equal: int list] ch (List.sort ~compare:Int.compare !results))
    then
      print_s
        [%message
          (codeword : Sw.R.poly)
            (error : Sw.R.poly)
            (received : Sw.R.poly)
            (syndromes : Sw.R.poly)
            (v : Sw.R.poly)
            (l : Sw.R.poly)
            (ch : int list)
            (el : int list)
            (!results : int list)];
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

let%expect_test "chien search - 1 symbol/cycle" =
  ignore (test 1 : _ option);
  [%expect {| |}]
;;

let%expect_test "chien search - 3 symbols/cycle" =
  ignore (test 3 : _ option);
  [%expect {| |}]
;;

let%expect_test "chien search - 6 symbols/cycle" =
  ignore (test 6 : _ option);
  [%expect {| |}]
;;

let%expect_test "random examples" =
  for _ = 1 to 100 do
    ignore (test 1 : _ option)
  done;
  [%expect {| |}]
;;
