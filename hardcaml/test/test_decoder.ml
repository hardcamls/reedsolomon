open Core
open Hardcaml
open Hardcaml_waveterm

module Test
    (Standard : Reedsolomon.Standards.Standard)
    (N : Hardcaml_reedsolomon.Parallelism.S) =
struct
  include Util.Make (Standard)
  module Decoder = Hw.Decoder (N)
  module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O_debug)

  let display_rules =
    let module I = Display_rules.With_interface (Decoder.I) in
    let module O = Display_rules.With_interface (Decoder.O_debug) in
    List.concat [ I.default (); O.default () ]
  ;;

  type t =
    { sim : Sim.t
    ; waves : Waveform.t option
    ; i : Bits.t ref Decoder.I.t
    ; o : Bits.t ref Decoder.O_debug.t
    }
  [@@deriving fields]

  let create_and_reset ?waves () =
    let sim =
      Sim.create
        ?config:(Option.map waves ~f:(Fn.const Cyclesim.Config.trace_all))
        (Decoder.create_with_debug
           (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
    in
    let waves, sim = waveform_opt ?waves sim in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs ~clock_edge:Before sim in
    Cyclesim.reset sim;
    i.enable := Bits.vdd;
    i.clocking.clear := Bits.vdd;
    Cyclesim.cycle sim;
    i.clocking.clear := Bits.gnd;
    { sim; waves; i; o }
  ;;

  let simulate_codeword { sim; waves = _; i; o } received =
    let cycles_per_codeword = (n + N.n - 1) / N.n in
    let offset = (cycles_per_codeword * N.n) - n in
    let recv = Array.concat [ received; Array.init offset ~f:(fun _ -> 0) ] in
    (* load received data *)
    i.first := Bits.vdd;
    i.load := Bits.vdd;
    for j = 0 to cycles_per_codeword - 1 do
      for k = 0 to N.n - 1 do
        i.x.(k) := Bits.of_int ~width:m recv.((j * N.n) + k)
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
    corrected
  ;;

  let test_one_codeword ?(verbose = false) ?waves () =
    let sim = create_and_reset ?waves () in
    let codeword = Array.rev (codeword (Array.rev (message ()))) in
    let error = error 2 in
    let received = codeword ^. error in
    let corrected = simulate_codeword sim received in
    let msg () =
      [%message
        (codeword : int array)
          (error : int array)
          (received : int array)
          (corrected : int array)]
    in
    if verbose
    then print_s (msg ())
    else if not ([%compare.equal: int array] corrected codeword)
    then raise_s (msg ());
    sim.waves
  ;;
end

let test_one_codeword ?verbose ?waves parallelism =
  let module Test =
    Test
      (Reedsolomon.Standards.BBCTest)
      (struct
        let n = parallelism
      end)
  in
  Test.test_one_codeword ?verbose ?waves ()
;;

let%expect_test "parallelism 1 cycle/code word" =
  ignore (test_one_codeword 1 : _ option);
  [%expect{||}]
;;

let%expect_test "parallelism 2 cycles/code word" =
  ignore (test_one_codeword 4 : _ option);
  [%expect{||}]
;;
