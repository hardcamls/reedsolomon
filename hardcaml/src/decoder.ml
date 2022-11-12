open Base
open Hardcaml
open Signal

module Make
  (Gp : Reedsolomon.Galois.Table_params)
  (Rp : Reedsolomon.Poly_codec.Params)
  (N : Parallelism.S) =
struct
  module Gfh = Galois.Make (Signal) (Gp)

  let rp_n = Rp.k + Rp.t + Rp.t
  let cycles_per_codeword = (rp_n + N.n - 1) / N.n
  let syndrome_inv_root_scale = (cycles_per_codeword * N.n) - rp_n

  module Syndromes = Syndromes.Make (Gp) (Rp) (N)

  (* berlekamp massey *)
  module RiBM = Berlekamp.Make (Gp) (Rp)

  (* chien search *)
  module Chien = Chien.Make (Gp) (Rp) (N)

  (* forney *)
  module Forney = Forney.Make (Gp) (Rp) (N)

  (* input codeword store *)
  module Input_fifo = Input_fifo.Make (Gp) (Rp) (N)

  (* decoder *)
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; enable : 'a [@bits 1]
      ; load : 'a [@bits 1]
      ; first : 'a [@bits 1]
      ; last : 'a [@bits 1]
      ; x : 'a array [@length N.n] [@bits Gfh.bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O_debug = struct
    type 'a t =
      { syn : 'a Syndromes.O.t
      ; bm : 'a RiBM.O.t
      ; ch : 'a Chien.O.t
      ; fy : 'a Forney.Parallel.O.t
      ; corrected : 'a array [@length N.n] [@bits Gfh.bits]
      ; ordy : 'a [@bits 1]
      ; error_count : 'a [@bits Gfh.bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { corrected : 'a array [@length N.n] [@bits Gfh.bits]
      ; ordy : 'a [@bits 1]
      ; error_count : 'a [@bits Gfh.bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_with_debug scope { I.clocking; enable; load; first; last; x } =
    let spec = Clocking.spec clocking in
    let reg d = reg spec ~enable d in
    let pipe ~n d = pipeline spec ~enable ~n d in
    (* syndromes *)
    let syn =
      Syndromes.hierarchy
        scope
        ~scale:syndrome_inv_root_scale
        { Syndromes.I.clocking; enable; first; last; x }
    in
    let fifo_re = wire 1 in
    let fifo =
      Input_fifo.hierarchy scope { Input_fifo.I.clocking; wr = load; d = x; rd = fifo_re }
    in
    (* berlekamp-massey *)
    let first = syn.valid in
    let last = pipe ~n:(2 * Rp.t) syn.valid in
    let bm =
      RiBM.hierarchy
        scope
        { RiBM.I.clocking; enable; first; last; syndromes = syn.syndromes }
    in
    (* chien search *)
    let start = reg last in
    let ch = Chien.hierarchy scope { Chien.I.clocking; enable; start; lambda = bm.l } in
    (* forney *)
    let l = Array.init ((Rp.t + 1) / 2) ~f:(fun i -> bm.l.((i * 2) + 1)) in
    let fy =
      Forney.Parallel.hierarchy
        scope
        { Forney.Parallel.I.clocking
        ; enable
        ; vld = ch.evld
        ; err = ch.eerr
        ; v = bm.w
        ; l
        ; x = ch.eloc
        }
    in
    (* correction *)
    let () = fifo_re <== fy.frdy.(0) in
    (* dont need array? *)
    let corrected =
      Array.init N.n ~f:(fun j ->
        mux2 (reg fy.ferr.(j)) (fifo.q.(j) ^: reg fy.emag.(j)) fifo.q.(j))
    in
    let ordy = reg fy.frdy.(0) in
    let error_count =
      reg_fb spec ~enable:(enable &: fifo_re) ~width:Gfh.bits ~f:(fun d ->
        let sum =
          reduce
            ~f:( +: )
            (Array.to_list (Array.map ~f:(fun x -> uresize x Gfh.bits) fy.ferr))
        in
        d +: sum)
    in
    (* for now all submodule outputs *)
    { O_debug.syn; bm; ch; fy; corrected; ordy; error_count }
  ;;

  let create scope i =
    let { O_debug.syn = _; bm = _; ch = _; fy = _; corrected; ordy; error_count } =
      create_with_debug scope i
    in
    { O.corrected; ordy; error_count }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"rsdecoder" create
  ;;
end
