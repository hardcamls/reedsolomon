open Base
open Hardcaml
open Signal

module Make (Gp : Reedsolomon.Galois.Table_params) (Rp : Reedsolomon.Poly_codec.Params) =
struct
  module Gfh = Galois.Make (Signal) (Gp)

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; enable : 'a [@bits 1]
      ; first : 'a [@bits 1]
      ; last : 'a [@bits 1]
      ; syndromes : 'a array [@length 2 * Rp.t] [@bits Gfh.bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { w : 'a array [@length Rp.t] [@bits Gfh.bits]
      ; l : 'a array [@length Rp.t + 1] [@bits Gfh.bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let lselect l lo hi =
    let rec ls l idx lo hi =
      if idx > hi
      then []
      else if idx < lo
      then ls (List.tl_exn l) (idx + 1) lo hi
      else List.hd_exn l :: ls (List.tl_exn l) (idx + 1) lo hi
    in
    ls l 0 lo hi
  ;;

  (* discrepancy unit (DC) *)
  let rDC scope spec enable load syndromes lambda =
    let module Gfh = (val Galois.of_scope (module Gp) scope) in
    (* order syndromes s[0],s[2t-1],...,s[2],s[1] *)
    let s = List.hd_exn syndromes :: List.rev (List.tl_exn syndromes) in
    (* connect up registers *)
    let rec f d s =
      match s with
      | [] -> []
      | s :: s' ->
        let d = reg spec ~enable (mux2 load s d) in
        d :: f d s'
    in
    let s0 = wire Gfh.bits in
    let s = f s0 s in
    let s_fb = List.hd_exn (List.rev s) in
    let () = s0 <== s_fb in
    let s = lselect s 0 Rp.t in
    (* generate multipliers *)
    let m = List.map2_exn ~f:Gfh.( *: ) s lambda in
    (* adder tree *)
    tree ~arity:2 ~f:(reduce ~f:Gfh.( +: )) m
  ;;

  (* PE0 unit *)
  let pe0 scope n spec enable init delta gamma mc b =
    let module Gfh = (val Galois.of_scope (module Gp) scope) in
    let ( -- ) = Scope.naming scope in
    let ( -- ) s name = s -- (name ^ Int.to_string n) in
    let spec = Reg_spec.override ~clear_to:init spec in
    (* lambda update *)
    let l =
      reg_fb spec ~enable ~width:Gfh.bits ~f:(fun l -> Gfh.((delta *: b) +: (l *: gamma)))
    in
    (* b update *)
    let b = reg spec ~enable (mux2 mc l b) in
    b -- "b", l
  ;;

  (* ELU unit *)
  let pe0s scope clear enable delta gamma mc =
    let module Gfh = (val Galois.of_scope (module Gp) scope) in
    let rec f b n =
      if n > Rp.t
      then []
      else (
        let init = if n = 0 then one else zero in
        let b, l = pe0 scope n clear enable (init Gfh.bits) delta gamma mc b in
        l :: f b (n + 1))
    in
    f (zero Gfh.bits) 0
  ;;

  (* PE1 unit *)
  let pe1 scope spec enable gamma syndrome delta delta' mc =
    let module Gfh = (val Galois.of_scope (module Gp) scope) in
    let spec = Reg_spec.override ~clear_to:syndrome spec in
    let theta =
      reg_fb spec ~enable ~width:Gfh.bits ~f:(fun theta -> mux2 mc delta' theta)
    in
    let delta' = reg spec ~enable Gfh.((delta *: theta) +: (delta' *: gamma)) in
    delta'
  ;;

  (* systolic array of pe1's *)
  let pe1s scope spec enable gamma syndromes delta mc =
    let module Gfh = (val Galois.of_scope (module Gp) scope) in
    let rec f delta' n = function
      | [] -> []
      | s :: s' ->
        let delta' = pe1 scope spec enable gamma s delta delta' mc in
        delta' :: f delta' (n + 1) s'
    in
    let delta' = List.(rev (f (zero Gfh.bits) 0 (rev syndromes))) in
    delta'
  ;;

  (* *)
  let pe1_2 scope spec enable first last gamma syndrome delta delta' mc =
    let module Gfh = (val Galois.of_scope (module Gp) scope) in
    let theta =
      reg_fb spec ~enable ~width:Gfh.bits ~f:(fun theta ->
          mux2 first syndrome (mux2 mc delta' theta))
    in
    let delta'' = Gfh.((delta *: theta) +: (delta' *: gamma)) in
    let delta' = reg spec ~enable (mux2 first syndrome delta'') in
    delta', reg spec ~enable:last delta''
  ;;

  (* *)
  let pe1s_2 scope clear enable first last gamma syndromes delta mc =
    let module Gfh = (val Galois.of_scope (module Gp) scope) in
    let rec f delta' n = function
      | [] -> []
      | s :: s' ->
        let delta' = pe1_2 scope clear enable first last gamma s delta delta' mc in
        delta' :: f (fst delta') (n + 1) s'
    in
    let delta' = List.rev (f (zero Gfh.bits) 0 (List.rev syndromes)) in
    delta'
  ;;

  (* control unit *)
  let ctrl scope spec enable delta =
    let ( -- ) = Scope.naming scope in
    let k_bits = Signal.num_bits_to_represent (2 * Rp.t) + 1 in
    let mc = wire 1 in
    let k =
      reg_fb spec ~enable ~width:k_bits ~f:(fun k -> mux2 mc ~:k (k +:. 1)) -- "K"
    in
    let () = mc <== (delta <>:. 0 &: ~:(msb k)) in
    let gamma =
      reg_fb
        (Reg_spec.override spec ~clear_to:(one Gfh.bits))
        ~enable
        ~width:Gfh.bits
        ~f:(fun gamma -> mux2 mc delta gamma)
    in
    mc -- "mc", gamma -- "gamma"
  ;;

  (* iBM hardware architecture *)
  let _iBM scope (clocking : 'a Clocking.t) ~enable ~start ~syndromes =
    let ( -- ) = Scope.naming scope in
    let spec = Clocking.spec clocking in
    let delta = wire Gfh.bits -- "delta" in
    let clear' = clocking.clear |: start in
    let spec_st = Reg_spec.override spec ~clear:clear' in
    let mc, gamma = ctrl scope spec_st enable delta in
    let lambda = pe0s scope spec_st enable delta gamma mc in
    let delta' = rDC scope spec enable start syndromes lambda in
    let () = delta <== delta' in
    lambda
  ;;

  (* riBM hardware architecture *)
  let _riBM scope (clocking : 'a Clocking.t) ~enable ~start ~syndromes =
    let delta0 = wire Gfh.bits in
    let clear = clocking.clear |: start in
    let spec = Clocking.spec clocking |> Reg_spec.override ~clear in
    let mc, gamma = ctrl scope spec enable delta0 in
    let lambda = pe0s scope spec enable delta0 gamma mc in
    let delta' = pe1s scope spec enable gamma syndromes delta0 mc in
    let () = delta0 <== List.hd_exn delta' in
    lambda
  ;;

  let rriBM scope (clocking : 'a Clocking.t) ~enable ~first ~last ~syndromes =
    let spec = Clocking.spec clocking in
    let syndromes =
      List.concat
        [ syndromes
        ; Array.to_list (Array.create ~len:Rp.t (zero Gfh.bits))
        ; [ one Gfh.bits ]
        ]
    in
    let delta0 = wire Gfh.bits in
    let mc, gamma =
      ctrl scope (Reg_spec.override spec ~clear:(clocking.clear |: first)) enable delta0
    in
    let delta' = pe1s_2 scope spec enable first last gamma syndromes delta0 mc in
    let () = delta0 <== fst (List.hd_exn delta') in
    ( List.map ~f:snd (lselect delta' 0 (Rp.t - 1))
    , List.map ~f:snd (lselect delta' Rp.t (2 * Rp.t)) )
  ;;

  let create scope { I.clocking; enable; first; last; syndromes } =
    let w, l =
      rriBM scope clocking ~enable ~first ~last ~syndromes:(Array.to_list syndromes)
    in
    O.{ w = Array.of_list w; l = Array.of_list l }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"berlekamp" create
  ;;
end
