open Core
open Hardcaml
open Hardcaml_waveterm

module Test (Standard : Reedsolomon.Standards.Standard) = struct
  include Util.Make (Standard)
  module Encoder = Hw.Encoder
  module Sim = Cyclesim.With_interface (Encoder.I) (Encoder.O)

  let display_rules =
    let module I = Display_rules.With_interface (Encoder.I) in
    let module O = Display_rules.With_interface (Encoder.O) in
    List.concat [ I.default (); O.default () ]
  ;;

  type t =
    { sim : Sim.t
    ; waves : Waveform.t option
    ; i : Bits.t ref Encoder.I.t
    ; o : Bits.t ref Encoder.O.t
    }
  [@@deriving fields]

  let create_and_reset ?waves () =
    let sim = Sim.create Encoder.create in
    let waves, sim = waveform_opt ?waves sim in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim in
    Cyclesim.reset sim;
    i.enable := Bits.vdd;
    i.clocking.clear := Bits.vdd;
    Cyclesim.cycle sim;
    i.clocking.clear := Bits.gnd;
    { sim; waves; i; o }
  ;;

  let simulate_message { sim; waves = _; i; o } data =
    (* load data *)
    i.ctrl := Bits.gnd;
    for j = 0 to Sw.Rp.k - 1 do
      i.d := Bits.of_int ~width:4 data.(Sw.Rp.k - j - 1);
      Cyclesim.cycle sim
    done;
    (* read parity *)
    i.ctrl := Bits.vdd;
    Array.init (Sw.Rp.t * 2) ~f:(fun _ ->
        let r = Bits.to_int !(o.q) in
        Cyclesim.cycle sim;
        r)
    |> Array.rev
  ;;

  let test ?waves num_tests =
    let sim = create_and_reset ?waves () in
    for j = 1 to num_tests do
      let data = message () in
      let parity_tb = simulate_message sim data in
      let parity_sw = parity data in
      if not ([%compare.equal: int array] parity_tb parity_sw)
      then
        print_s
          [%message
            "Encode mismatch"
              (j : int)
              (data : Sw.R.poly)
              (parity_tb : Sw.R.poly)
              (parity_sw : Sw.R.poly)]
    done;
    sim.waves
  ;;
end

include Test (Reedsolomon.Standards.BBCTest)

let%expect_test "test encoder" =
  ignore (test 10 : Waveform.t option);
  [%expect]
;;
