open Base
open Hardcaml
open Signal

module Make
  (Gp : Reedsolomon.Galois.Table_params)
  (Rp : Reedsolomon.Poly_codec.Params)
  (N : Parallelism.S) =
struct
  module Gfh = Galois.Make (Signal) (Gp)
  module Gfs = Gfh.G
  module Rs = Reedsolomon.Poly_codec.Make (Gfs) (Rp)

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; enable : 'a [@bits 1]
      ; first : 'a [@bits 1]
      ; last : 'a [@bits 1]
      ; x : 'a array [@length N.n] [@bits Gfh.bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a [@bits 1]
      ; syndromes : 'a array [@length 2 * Rp.t] [@bits Gfh.bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let syndrome ~spec ~root ~enable ~first ~x =
    reg_fb spec ~enable ~width:Gfh.bits ~f:(fun d ->
      let d = mux2 first Gfh.zero d in
      Gfh.(cmul root d +: x))
  ;;

  let _syndromes ~spec ~enable ~first ~x =
    Array.init (2 * Rp.t) ~f:(fun i -> syndrome ~spec ~root:(Rs.root i) ~enable ~first ~x)
  ;;

  let create ~scale { I.clocking; enable; first; last; x } =
    let spec = Clocking.spec clocking in
    let n = Array.length x in
    let n_tree = 2 in
    let eval c x =
      let len = Array.length x in
      let cmul c i x = Gfh.cmul Gfs.(c **: i) x in
      let a = Array.mapi ~f:(fun i x -> cmul c (len - i - 1) x) x in
      let a = Array.map ~f:(fun x -> reg spec ~enable x) a in
      let add_p a = reg spec ~enable (reduce ~f:Gfh.( +: ) a) in
      tree ~arity:n_tree ~f:add_p (Array.to_list a)
    in
    let horner enable first c x =
      reg_fb spec ~enable ~width:Gfh.bits ~f:(fun d ->
        Gfh.(x +: cmul c (mux2 first zero d)))
    in
    let syndrome enable first root_n root x = horner enable first root_n (eval root x) in
    let first = pipeline spec ~enable ~n:(1 + Util.tree_depth n_tree n) first in
    let last = pipeline spec ~enable ~n:(2 + Util.tree_depth n_tree n) last in
    let syndromes =
      Array.init (2 * Rp.t) ~f:(fun i ->
        let root = Rs.root i in
        syndrome enable first Gfs.(root **: n) root x)
    in
    { O.valid = reg spec ~enable last
    ; syndromes =
        Array.init (Array.length syndromes) ~f:(fun i ->
          let iroot = Gfs.(inv (Rs.root i **: scale)) in
          reg
            spec
            ~enable:last
            (if scale = 0 then syndromes.(i) else Gfh.cmul iroot syndromes.(i)))
    }
  ;;
end
