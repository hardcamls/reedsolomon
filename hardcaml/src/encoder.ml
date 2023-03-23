open Base
open Hardcaml
open Signal
module M = Encoder_intf.M

module Make (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Poly_codec.Params) =
struct
  module Gfh = Galois.Make (Signal) (Gp)
  module Rs = Reedsolomon.Poly_codec.Make (Gfh.G) (Rp)

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; enable : 'a [@bits 1]
      ; ctrl : 'a [@bits 1]
      ; d : 'a [@bits Gfh.bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a [@bits Gfh.bits] } [@@deriving sexp_of, hardcaml]
  end

  let create { I.clocking; enable; ctrl; d } =
    let spec = Clocking.spec clocking in
    let g = Array.to_list Rs.generator |> List.drop_last_exn in
    let b = Gfh.bits in
    let rec f d_in d_prev = function
      | [] -> []
      | cof :: t ->
        let r = reg spec ~enable Gfh.(cmul cof d_in +: d_prev) in
        (r -- "enc_state_reg ") :: f d_in r t
    in
    let w = wire b in
    let qs = f w (zero b) g in
    let q = List.last_exn qs in
    let () = w <== Gfh.(mux2 ctrl zero (d +: q)) in
    { O.q }
  ;;
end
