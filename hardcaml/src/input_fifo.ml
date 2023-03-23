open Base
open Hardcaml
open Signal
module M = Input_fifo_intf.M

module Make
    (Gp : Reedsolomon.Galois.Table_params)
    (Rp : Reedsolomon.Poly_codec.Params)
    (N : Parallelism.S) =
struct
  module Gfh = Galois.Make (Signal) (Gp)

  let rp_n = Rp.k + Rp.t + Rp.t
  let cycles_per_codeword = (rp_n + N.n - 1) / N.n

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; wr : 'a [@bits 1]
      ; d : 'a array [@length N.n] [@bits Gfh.bits]
      ; rd : 'a [@bits 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { q : 'a array [@length N.n] [@bits Gfh.bits] }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope { I.clocking; wr; d; rd } =
    let ( -- ) = Scope.naming scope in
    let spec = Clocking.spec clocking in
    let fbits = Signal.num_bits_to_represent cycles_per_codeword in
    let felems = 1 lsl fbits in
    let wa = reg_fb spec ~enable:wr ~width:fbits ~f:(fun d -> d +:. 1) -- "fifo_wa" in
    let ra = reg_fb spec ~enable:rd ~width:fbits ~f:(fun d -> d +:. 1) -- "fifo_ra" in
    let d = concat_lsb (Array.to_list d) in
    let q =
      ram_rbw
        felems
        ~write_port:
          { write_clock = clocking.clock
          ; write_address = wa
          ; write_data = d
          ; write_enable = wr
          }
        ~read_port:{ read_clock = clocking.clock; read_address = ra; read_enable = rd }
    in
    let q =
      Array.init N.n ~f:(fun i -> select q (((i + 1) * Gfh.bits) - 1) (i * Gfh.bits))
    in
    O.{ q }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"input_fifo" create
  ;;
end
