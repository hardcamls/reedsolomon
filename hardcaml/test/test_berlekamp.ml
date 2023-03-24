open Core
open Hardcaml
open Hardcaml_waveterm
open Util.Basic

module Decoder = Hw.Decoder (struct
  let n = 1
end)

module RiBM = Decoder.RiBM
module Sim = Cyclesim.With_interface (RiBM.I) (RiBM.O)

let display_rules =
  let module I = Display_rules.With_interface (RiBM.I) in
  let module O = Display_rules.With_interface (RiBM.O) in
  List.concat [ I.default (); O.default () ]
;;

let test ?waves num_errors =
  let sim = Sim.create (RiBM.create (Scope.create ~flatten_design:true ())) in
  let waves, sim = waveform_opt ?waves sim in
  let i = Cyclesim.inputs sim in
  let _o = Cyclesim.outputs ~clock_edge:Before sim in
  let o = Cyclesim.outputs ~clock_edge:After sim in
  (* err'd message *)
  let codeword = codeword (message ()) in
  let error = error num_errors in
  let received = codeword ^. error in
  let syndromes = syndromes received in
  let bm_sw_w, bm_sw_l = berlekamp syndromes in
  Cyclesim.reset sim;
  i.enable := Bits.vdd;
  i.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clocking.clear := Bits.gnd;
  i.first := Bits.vdd;
  for j = 0 to (2 * t) - 1 do
    i.syndromes.(j) := Bits.of_int ~width:m syndromes.(j)
  done;
  for j = 0 to 2 * t do
    if j = 2 * t then i.last := Bits.vdd;
    Cyclesim.cycle sim;
    i.first := Bits.gnd;
    i.last := Bits.gnd
  done;
  Cyclesim.cycle sim;
  let w = Array.init (Array.length bm_sw_w) ~f:(fun j -> Bits.to_int !(o.w.(j))) in
  let l = Array.init (Array.length bm_sw_l) ~f:(fun j -> Bits.to_int !(o.l.(j))) in
  if (not ([%compare.equal: int array] bm_sw_w w))
     || not ([%compare.equal: int array] bm_sw_l l)
  then
    raise_s
      [%message
        (bm_sw_w : Sw.R.poly) (w : Sw.R.poly) (bm_sw_l : Sw.R.poly) (l : Sw.R.poly)];
  for _ = 0 to 2 do
    Cyclesim.cycle sim
  done;
  waves
;;

let%expect_test "berlekamp" =
  ignore (test 0 : _ option);
  ignore (test 1 : _ option);
  ignore (test 2 : _ option);
  [%expect]
;;
