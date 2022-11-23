open Core
open Hardcaml

module Make (Standard : Reedsolomon.Standards.Standard) = struct
  module Sw = Standard
  module Hw = Hardcaml_reedsolomon.Codec.Make (Sw.Gp) (Sw.Rp)
  module Gfb = Hardcaml_reedsolomon.Galois.Make (Bits) (Sw.Gp)

  module type Parallelism = Hardcaml_reedsolomon.Parallelism.S

  (* rs code params *)
  let k = Sw.Rp.k
  let t = Sw.Rp.t
  let b = Sw.Rp.b
  let n = k + t + t

  (* number of field elements *)
  let n_elems = Sw.G.n_elems

  (* number of bits per symbol *)
  let sbits = Int.ceil_log2 n_elems

  (* create a random message *)
  let message () = Array.init k ~f:(fun _ -> Random.int n_elems)

  let rev a =
    let len = Array.length a in
    Array.init len ~f:(fun i -> a.(len - i - 1))
  ;;

  (* codeword from message *)
  let codeword message = Sw.R.R.slice (Sw.R.encode message) (n - 1)

  (* parity of message *)
  let parity message = Sw.R.R.slice (Sw.R.parity message) ((2 * t) - 1)

  let rand_sort a =
    for i = Array.length a - 1 downto 1 do
      let swap = Random.int (i + 1) in
      let tmp = a.(i) in
      a.(i) <- a.(swap);
      a.(swap) <- tmp
    done
  ;;

  (* random error vector with 'e' errors *)
  let error e =
    let e =
      Array.init n ~f:(fun i -> if i < e then 1 + Random.int (n_elems - 1) else 0)
    in
    rand_sort e;
    e
  ;;

  let ( ^. ) a b = Array.init (Array.length a) ~f:(fun i -> a.(i) lxor b.(i))
  let syndromes recv = Sw.R.R.slice (Sw.R.syndromes recv) ((2 * t) - 1)
  let berlekamp syn = Sw.R.Sarwate.rriBM syn
  let chien l = Sw.R.chien l
  let err_loc l = Sw.R.error_location l

  let dump a =
    Array.iter a ~f:(printf "%2i ");
    Printf.printf "\n"
  ;;

  let waveform_opt ?(waves = false) sim =
    if waves
    then (
      let waves, sim = Hardcaml_waveterm.Waveform.create sim in
      Some waves, sim)
    else None, sim
  ;;
end

include Make (Reedsolomon.Standards.BBCTest)
