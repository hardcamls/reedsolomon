(* hardware implementation of reed-solomon codec *)

open Hardcaml

module type S = sig
  open Signal

  module Clocking : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Encoder : sig
    module I : sig
      type 'a t =
        { clocking : 'a Clocking.t
        ; enable : 'a
        ; ctrl : 'a
        ; d : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
    end

    val create : t I.t -> t O.t
  end

  module type N = sig
    val n : int
  end

  module Decoder (N : N) : sig
    module Syndromes : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; first : 'a
          ; last : 'a
          ; x : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t =
          { valid : 'a
          ; syndromes : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      val create : scale:int -> t I.t -> t O.t
    end

    module RiBM : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; first : 'a
          ; last : 'a
          ; syndromes : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t =
          { w : 'a array
          ; l : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      val create : t I.t -> t O.t
    end

    module Chien : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; start : 'a
          ; lambda : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t =
          { eval : 'a array
          ; eloc : 'a array
          ; evld : 'a array
          ; eerr : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      val create : t I.t -> t O.t
    end

    module Forney_serial : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; start : 'a
          ; store : 'a
          ; ctrl : 'a
          ; tap : 'a
          ; x : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t = { e : 'a } [@@deriving sexp_of, hardcaml]
      end

      val create : t I.t -> t O.t
    end

    module Forney : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; vld : 'a
          ; err : 'a
          ; v : 'a array
          ; l : 'a array
          ; x : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t =
          { emag : 'a
          ; frdy : 'a
          ; ferr : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end

      val create : t I.t -> t O.t
    end

    module PForney : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; vld : 'a array
          ; err : 'a array
          ; v : 'a array
          ; l : 'a array
          ; x : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t =
          { emag : 'a array
          ; frdy : 'a array
          ; ferr : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      val create : t I.t -> t O.t
    end

    module Decode : sig
      module I : sig
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a
          ; load : 'a
          ; first : 'a
          ; last : 'a
          ; x : 'a array
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O_debug : sig
        type 'a t =
          { syn : 'a Syndromes.O.t
          ; bm : 'a RiBM.O.t
          ; ch : 'a Chien.O.t
          ; fy : 'a PForney.O.t
          ; corrected : 'a array [@length N.n] [@bits Gfh.bits]
          ; ordy : 'a [@bits 1]
          ; error_count : 'a [@bits Gfh.bits]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O : sig
        type 'a t =
          { corrected : 'a array
          ; ordy : 'a
          ; error_count : 'a
          }
        [@@deriving sexp_of, hardcaml]
      end

      val create_with_debug : t I.t -> t O_debug.t
      val create : t I.t -> t O.t
    end
  end
end

module Make (Gp : Reedsolomon.Galois.Table.Params) (Rp : Reedsolomon.Codec.RsParams) =
struct
  module B = Signal
  module Gfh = Galois.Make (B) (Gp)
  module Gfs = Gfh.G
  module Rs = Reedsolomon.Codec.MakePoly (Gfs) (Rp)

  module Clocking = struct
    open Signal

    type 'a t =
      { clock : 'a
      ; clear : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let spec ?clear_to ?extra_clear { clock; clear } =
      let spec = Reg_spec.create ~clock ~clear () in
      let spec =
        match extra_clear with
        | None -> spec
        | Some extra_clear -> Reg_spec.override spec ~clear:(clear |: extra_clear)
      in
      let spec =
        match clear_to with
        | None -> spec
        | Some clear_to -> Reg_spec.override spec ~clear_to
      in
      spec
    ;;
  end

  (** RS encoder *)
  module Encoder = struct
    open Signal

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
      let g = List.(Rs.generator |> Array.to_list |> rev |> tl |> rev) in
      let b = Gfh.bits in
      let rec f d_in d_prev = function
        | [] -> []
        | cof :: t ->
          let r = B.reg spec ~enable Gfh.(cmul cof d_in +: d_prev) in
          (r -- "enc_state_reg ") :: f d_in r t
      in
      let w = B.wire b in
      let qs = f w (B.zero b) g in
      let q = List.(qs |> rev |> hd) in
      let () = B.(w <== Gfh.(mux2 ctrl zero (d +: q))) in
      { O.q }
    ;;
  end

  module type N = sig
    val n : int
  end

  module Decoder (N : N) = struct
    open Signal

    let rec tree_depth n x =
      if x <= 1 then 0 else if x <= n then 1 else 1 + tree_depth n ((x + n - 1) / n)
    ;;

    let rp_n = Rp.k + Rp.t + Rp.t
    let cycles_per_codeword = (rp_n + N.n - 1) / N.n
    let syndrome_inv_root_scale = (cycles_per_codeword * N.n) - rp_n

    module Syndromes = struct
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

      let syndromes ~spec ~enable ~first ~x =
        Array.init (2 * Rp.t) (fun i ->
          syndrome ~spec ~root:(Rs.root i) ~enable ~first ~x)
      ;;

      let create ~scale { I.clocking; enable; first; last; x } =
        let spec = Clocking.spec clocking in
        let n = Array.length x in
        let n_tree = 2 in
        let eval c x =
          let len = Array.length x in
          let cmul c i x = Gfh.cmul Gfs.(c **: i) x in
          let a = Array.mapi (fun i x -> cmul c (len - i - 1) x) x in
          let a = Array.map (fun x -> reg spec ~enable x) a in
          let add_p a = reg spec ~enable (reduce ~f:Gfh.( +: ) a) in
          tree ~arity:n_tree ~f:add_p (Array.to_list a)
        in
        let horner enable first c x =
          reg_fb spec ~enable ~width:Gfh.bits ~f:(fun d ->
            Gfh.(x +: cmul c (mux2 first zero d)))
        in
        let syndrome enable first root_n root x =
          horner enable first root_n (eval root x)
        in
        let first = pipeline spec ~enable ~n:(1 + tree_depth n_tree n) first in
        let last = pipeline spec ~enable ~n:(2 + tree_depth n_tree n) last in
        let syndromes =
          Array.init (2 * Rp.t) (fun i ->
            let root = Rs.root i in
            syndrome enable first Gfs.(root **: n) root x)
        in
        { O.valid = reg spec ~enable last
        ; syndromes =
            Array.init (Array.length syndromes) (fun i ->
              let iroot = Gfs.(inv (Rs.root i **: scale)) in
              reg
                spec
                ~enable:last
                (if scale = 0 then syndromes.(i) else Gfh.cmul iroot syndromes.(i)))
        }
      ;;
    end

    (***********************************************************)
    (* berlekamp massey *)
    let lselect l lo hi =
      let rec ls l idx lo hi =
        if idx > hi
        then []
        else if idx < lo
        then ls (List.tl l) (idx + 1) lo hi
        else List.hd l :: ls (List.tl l) (idx + 1) lo hi
      in
      ls l 0 lo hi
    ;;

    (* discrepancy unit (DC) *)
    let rDC spec enable load syndromes lambda =
      (* order syndromes s[0],s[2t-1],...,s[2],s[1] *)
      let s = List.(hd syndromes :: rev (tl syndromes)) in
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
      let s_fb = List.(hd (rev s)) in
      let () = s0 <== s_fb in
      let s = lselect s 0 Rp.t in
      (* generate multipliers *)
      let m = List.map2 Gfh.( *: ) s lambda in
      (* adder tree *)
      tree ~arity:2 ~f:(reduce ~f:Gfh.( +: )) m
    ;;

    (* PE0 unit *)
    let pe0 n spec enable init delta gamma mc b =
      let ( -- ) s name = s -- (name ^ string_of_int n) in
      let spec = Reg_spec.override ~clear_to:init spec in
      (* lambda update *)
      let l =
        reg_fb spec ~enable ~width:Gfh.bits ~f:(fun l ->
          Gfh.((delta *: b) +: (l *: gamma)))
      in
      (* b update *)
      let b = reg spec ~enable (mux2 mc l b) in
      b -- "b", l
    ;;

    (* ELU unit *)
    let pe0s clear enable delta gamma mc =
      let rec f b n =
        if n > Rp.t
        then []
        else (
          let init = if n = 0 then one else zero in
          let b, l = pe0 n clear enable (init Gfh.bits) delta gamma mc b in
          l :: f b (n + 1))
      in
      f (zero Gfh.bits) 0
    ;;

    (* PE1 unit *)
    let pe1 n spec enable gamma syndrome delta delta' mc =
      let spec = Reg_spec.override ~clear_to:syndrome spec in
      let theta =
        reg_fb spec ~enable ~width:Gfh.bits ~f:(fun theta -> mux2 mc delta' theta)
      in
      let delta' = reg spec ~enable Gfh.((delta *: theta) +: (delta' *: gamma)) in
      delta'
    ;;

    (* systolic array of pe1's *)
    let pe1s spec enable gamma syndromes delta mc =
      let rec f delta' n = function
        | [] -> []
        | s :: s' ->
          let delta' = pe1 n spec enable gamma s delta delta' mc in
          delta' :: f delta' (n + 1) s'
      in
      let delta' = List.(rev (f (zero Gfh.bits) 0 (rev syndromes))) in
      delta'
    ;;

    (* *)
    let pe1_2 n spec enable first last gamma syndrome delta delta' mc =
      let theta =
        reg_fb spec ~enable ~width:Gfh.bits ~f:(fun theta ->
          mux2 first syndrome (mux2 mc delta' theta))
      in
      let delta'' = Gfh.((delta *: theta) +: (delta' *: gamma)) in
      let delta' = reg spec ~enable (mux2 first syndrome delta'') in
      delta', reg spec ~enable:last delta''
    ;;

    (* *)
    let pe1s_2 clear enable first last gamma syndromes delta mc =
      let rec f delta' n = function
        | [] -> []
        | s :: s' ->
          let delta' = pe1_2 n clear enable first last gamma s delta delta' mc in
          delta' :: f (fst delta') (n + 1) s'
      in
      let delta' = List.(rev (f (zero Gfh.bits) 0 (rev syndromes))) in
      delta'
    ;;

    (* control unit *)
    let ctrl spec enable delta =
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
    let iBM (clocking : 'a Clocking.t) ~enable ~start ~syndromes =
      let spec = Clocking.spec clocking in
      let delta = wire Gfh.bits -- "delta" in
      let clear' = clocking.clear |: start in
      let spec_st = Reg_spec.override spec ~clear:clear' in
      let mc, gamma = ctrl spec_st enable delta in
      let lambda = pe0s spec_st enable delta gamma mc in
      let delta' = rDC spec enable start syndromes lambda in
      let () = delta <== delta' in
      lambda
    ;;

    (* riBM hardware architecture *)
    let riBM (clocking : 'a Clocking.t) ~enable ~start ~syndromes =
      let delta0 = wire Gfh.bits in
      let clear = clocking.clear |: start in
      let spec = Clocking.spec clocking |> Reg_spec.override ~clear in
      let mc, gamma = ctrl spec enable delta0 in
      let lambda = pe0s spec enable delta0 gamma mc in
      let delta' = pe1s spec enable gamma syndromes delta0 mc in
      let () = delta0 <== List.hd delta' in
      lambda
    ;;

    (* RiBM hardware architecture *)
    let rriBM_old (clocking : 'a Clocking.t) ~enable ~start ~syndromes =
      let syndromes =
        List.concat
          [ syndromes; Array.(to_list (make Rp.t (zero Gfh.bits))); [ one Gfh.bits ] ]
      in
      let delta0 = wire Gfh.bits in
      let clear = clocking.clear |: start in
      let spec = Clocking.spec clocking |> Reg_spec.override ~clear in
      let mc, gamma = ctrl spec enable delta0 in
      let delta' = pe1s spec enable gamma syndromes delta0 mc in
      let () = delta0 <== List.hd delta' in
      lselect delta' 0 (Rp.t - 1), lselect delta' Rp.t (2 * Rp.t)
    ;;

    let rriBM (clocking : 'a Clocking.t) ~enable ~first ~last ~syndromes =
      let spec = Clocking.spec clocking in
      let syndromes =
        List.concat
          [ syndromes; Array.(to_list (make Rp.t (zero Gfh.bits))); [ one Gfh.bits ] ]
      in
      let delta0 = wire Gfh.bits in
      let mc, gamma =
        ctrl (Reg_spec.override spec ~clear:(clocking.clear |: first)) enable delta0
      in
      let delta' = pe1s_2 spec enable first last gamma syndromes delta0 mc in
      let () = delta0 <== fst (List.hd delta') in
      ( List.map snd (lselect delta' 0 (Rp.t - 1))
      , List.map snd (lselect delta' Rp.t (2 * Rp.t)) )
    ;;

    module RiBM = struct
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

      let create { I.clocking; enable; first; last; syndromes } =
        let w, l =
          I.(rriBM clocking ~enable ~first ~last ~syndromes:(Array.to_list syndromes))
        in
        O.{ w = Array.of_list w; l = Array.of_list l }
      ;;
    end

    (***********************************************************)
    (* chien search *)

    let chien clocking ~enable ~lambda =
      let f i l =
        reg_fb (Clocking.spec ~clear_to:l clocking) ~enable ~width:Gfh.bits ~f:(fun l ->
          Gfh.cmul (Gfs.antilog i) l)
      in
      let l = List.mapi f lambda in
      tree ~arity:2 ~f:(reduce ~f:Gfh.( +: )) l
    ;;

    module State = struct
      type t =
        | Start
        | Run
      [@@deriving sexp_of, enumerate, compare]
    end

    module Var = Always.Variable

    let chien_ctrl spec ~p ~enable ~start =
      let sm = Always.State_machine.create (module State) spec ~enable in
      let vld = Var.wire ~default:B.gnd in
      let cnt = Var.reg spec ~enable ~width:(Base.Int.ceil_log2 cycles_per_codeword) in
      let cnt_next = cnt.value +:. 1 in
      let cnt_last = cnt.value ==:. cycles_per_codeword - 1 in
      let () =
        Always.(
          compile
            [ sm.switch
                [ ( Start
                  , [ cnt <--. 0
                    ; when_ start [ vld <--. 1; cnt <-- cnt_next; sm.set_next Run ]
                    ] )
                ; ( Run
                  , [ vld <--. 1; cnt <-- cnt_next; when_ cnt_last [ sm.set_next Start ] ]
                  )
                ]
            ])
      in
      ( Array.init p (fun i ->
          if ((cycles_per_codeword - 1) * p) + i >= rp_n
          then mux2 cnt_last gnd vld.value
          else vld.value)
      , Array.init p (fun i ->
          let init = B.of_int ~width:Gfh.bits (1 + i) in
          reg_fb
            (Reg_spec.override spec ~clear_to:init)
            ~enable:(enable &: vld.value)
            ~width:Gfh.bits
            ~f:(fun d -> mux2 cnt_last init (Gfh.modfs (ue d +:. p)))) )
    ;;

    module Chien = struct
      module I = struct
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a [@bits 1]
          ; start : 'a [@bits 1]
          ; lambda : 'a array [@length Rp.t + 1] [@bits Gfh.bits]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t =
          { eval : 'a array [@length N.n] [@bits Gfh.bits]
          ; eloc : 'a array [@length N.n] [@bits Gfh.bits]
          ; evld : 'a array [@length N.n] [@bits 1]
          ; eerr : 'a array [@length N.n] [@bits 1]
          }
        [@@deriving sexp_of, hardcaml]
      end

      (* produces error location results in reverse order ie from [n_elem-2 ... 0] *)
      let create { I.clocking; enable; start; lambda } =
        let spec = Clocking.spec clocking in
        let p = N.n in
        let lambda' = Array.map (fun _ -> wire Gfh.bits) lambda in
        let f i j l = Gfh.cmul (Gfs.antilog (i * j)) l in
        let c = Array.init p (fun i -> Array.mapi (f (i + 1)) lambda') in
        let fb = c.(p - 1) in
        let () =
          Array.iteri
            (fun i l -> lambda'.(i) <== mux2 start l (reg spec ~enable fb.(i)))
            lambda
        in
        let eval =
          Array.map (fun c -> tree ~arity:2 ~f:(reduce ~f:Gfh.( +: )) (Array.to_list c)) c
        in
        let evld, eloc = chien_ctrl spec ~p ~enable ~start in
        let eerr = Array.init p (fun j -> eval.(j) ==:. 0 &: evld.(j)) in
        { O.eval; eloc; evld; eerr }
      ;;
    end

    (***********************************************************)
    (* forney *)

    module Forney_serial = struct
      module I = struct
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a [@bits 1]
          ; start : 'a [@bits 1]
          ; store : 'a [@bits 1]
          ; ctrl : 'a [@bits 1]
          ; tap : 'a [@bits Gfh.bits]
          ; x : 'a [@bits Gfh.bits]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t = { e : 'a [@bits Gfh.bits] } [@@deriving sexp_of, hardcaml]
      end

      let create { I.clocking; enable; start; store; ctrl; tap; x } =
        let spec = Clocking.spec clocking in
        let ghorner ~enable ~tap ~x =
          reg_fb
            (Reg_spec.override ~clear:start ~clear_to:tap spec)
            ~enable
            ~width:Gfh.bits
            ~f:Gfh.(fun d -> tap +: (x *: d))
        in
        let regce = reg spec ~enable in
        (* 2 stages, compute various values from x *)
        let x' = regce Gfh.(antilog x) -- "x1" in
        let x'2 = regce (Gfh.rom (fun i -> Gfs.(i **: 2)) x') -- "x2" in
        let x = regce Gfh.(cpow x' (Rp.b + (2 * Rp.t) - 1)) -- "x3" in
        let x_m = regce (mux2 ctrl x'2 x') -- "x_m" in
        (* evaluate polys, latching results *)
        let h = ghorner ~enable ~tap ~x:x_m -- "h" in
        let v = reg spec ~enable:(enable &: ctrl &: start) h -- "v" in
        (* 2 stages, compute error *)
        let e = reg spec ~enable Gfh.(v /: h) -- "v_div_l" in
        let e = reg spec ~enable:(enable &: store) Gfh.(x *: e) -- "x_mul_e" in
        { O.e }
      ;;
    end

    module Forney = struct
      module I = struct
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a [@bits 1]
          ; vld : 'a [@bits 1]
          ; err : 'a [@bits 1]
          ; v : 'a array [@length Rp.t] [@bits Gfh.bits]
          ; l : 'a array [@length (Rp.t + 1) / 2] [@bits Gfh.bits]
          ; x : 'a [@bits Gfh.bits]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t =
          { emag : 'a [@bits Gfh.bits]
          ; frdy : 'a [@bits 1]
          ; ferr : 'a [@bits 1]
          }
        [@@deriving sexp_of, hardcaml]
      end

      let create { I.clocking; enable; vld; err; v; l; x } =
        let spec = Clocking.spec clocking in
        let n_tree = 4 in
        let reg d = reg spec ~enable d in
        let pipe n d = pipeline spec ~enable ~n d in
        (* parallel polynomial evaluation *)
        let eval enable poly x =
          let a = Array.mapi Gfh.(fun pow coef -> coef *: cpow x pow) poly in
          let a = Array.map (fun d -> reg d) a in
          let add_p a = reg (reduce ~f:Gfh.( +: ) a) in
          tree ~arity:n_tree ~f:add_p (Array.to_list a)
        in
        let v_depth = tree_depth n_tree (Array.length v) in
        let l_depth = tree_depth n_tree (Array.length l) in
        assert (v_depth >= l_depth);
        let xv = reg Gfh.(antilog x) in
        let xl = reg Gfh.(rom (fun i -> Gfs.(i **: 2)) xv) -- "xl" in
        let xv = reg xv -- "xv" in
        let v = eval enable v xv -- "ev" in
        let l = eval enable l xl -- "el_" in
        let l = pipe (v_depth - l_depth) l -- "el" in
        let x = pipe (1 + v_depth) Gfh.(cpow xv (Rp.b + (2 * Rp.t) - 1)) -- "xp" in
        { O.emag = reg Gfh.(x *: (v /: l))
        ; frdy = pipe (4 + v_depth) vld
        ; ferr = pipe (4 + v_depth) err
        }
      ;;
    end

    module PForney = struct
      module I = struct
        type 'a t =
          { clocking : 'a Clocking.t
          ; enable : 'a [@bits 1]
          ; vld : 'a array [@length N.n] [@bits 1]
          ; err : 'a array [@length N.n] [@bits 1]
          ; v : 'a array [@length Rp.t] [@bits Gfh.bits]
          ; l : 'a array [@length (Rp.t + 1) / 2] [@bits Gfh.bits]
          ; x : 'a array [@length N.n] [@bits Gfh.bits]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t =
          { emag : 'a array [@length N.n] [@bits Gfh.bits]
          ; frdy : 'a array [@length N.n] [@bits 1]
          ; ferr : 'a array [@length N.n] [@bits 1]
          }
        [@@deriving sexp_of, hardcaml]
      end

      let create { I.clocking; enable; vld; err; v; l; x } =
        let o =
          Array.init N.n (fun j ->
            Forney.create
              { Forney.I.clocking; enable; vld = vld.(j); err = err.(j); v; l; x = x.(j) })
        in
        O.
          { emag = Array.map (fun { Forney.O.emag; _ } -> emag) o
          ; frdy = Array.map (fun { Forney.O.frdy; _ } -> frdy) o
          ; ferr = Array.map (fun { Forney.O.ferr; _ } -> ferr) o
          }
      ;;
    end

    (***********************************************************)
    (* input codeword store *)

    module Fifo = struct
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

      let f { I.clocking; wr; d; rd } =
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
            ~read_port:
              { read_clock = clocking.clock; read_address = ra; read_enable = rd }
        in
        let q =
          Array.init N.n (fun i -> select q (((i + 1) * Gfh.bits) - 1) (i * Gfh.bits))
        in
        O.{ q }
      ;;
    end

    (***********************************************************)
    (* decoder *)

    module Decode = struct
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
          ; fy : 'a PForney.O.t
          ; corrected : 'a array [@length N.n] [@bits Gfh.bits]
          ; ordy : 'a [@bits 1]
          ; error_count : 'a [@bits Gfh.bits]
          }
        [@@deriving sexp_of, hardcaml]
      end

      module O = struct
        type 'a t =
          { (*(syn : PSyndromes.O)
            (bm : RiBM.O)
            (ch : PChien.O)
            (fy : PForney.O)*)
            corrected : 'a array [@length N.n] [@bits Gfh.bits]
          ; ordy : 'a [@bits 1]
          ; error_count : 'a [@bits Gfh.bits]
          }
        [@@deriving sexp_of, hardcaml]
      end

      let create_with_debug { I.clocking; enable; load; first; last; x } =
        let spec = Clocking.spec clocking in
        let reg d = reg spec ~enable d in
        let pipe ~n d = pipeline spec ~enable ~n d in
        (* syndromes *)
        let syn =
          Syndromes.create
            ~scale:syndrome_inv_root_scale
            { Syndromes.I.clocking; enable; first; last; x }
        in
        let fifo_re = wire 1 in
        let fifo = Fifo.f { Fifo.I.clocking; wr = load; d = x; rd = fifo_re } in
        (* berlekamp-massey *)
        let first = syn.Syndromes.O.valid in
        let last = pipe ~n:(2 * Rp.t) syn.Syndromes.O.valid in
        let bm =
          RiBM.create
            { RiBM.I.clocking
            ; enable
            ; first
            ; last
            ; syndromes = syn.Syndromes.O.syndromes
            }
        in
        (* chien search *)
        let start = reg last in
        let ch = Chien.create { Chien.I.clocking; enable; start; lambda = bm.RiBM.O.l } in
        (* forney *)
        let l = Array.init ((Rp.t + 1) / 2) (fun i -> bm.RiBM.O.l.((i * 2) + 1)) in
        let fy =
          PForney.create
            { PForney.I.clocking
            ; enable
            ; vld = ch.Chien.O.evld
            ; err = ch.Chien.O.eerr
            ; v = bm.RiBM.O.w
            ; l
            ; x = ch.Chien.O.eloc
            }
        in
        (* correction *)
        let () = fifo_re <== fy.PForney.O.frdy.(0) in
        (* dont need array? *)
        let corrected =
          Array.init N.n (fun j ->
            mux2
              (reg fy.PForney.O.ferr.(j))
              (fifo.Fifo.O.q.(j) ^: reg fy.PForney.O.emag.(j))
              fifo.Fifo.O.q.(j))
        in
        let ordy = reg fy.PForney.O.frdy.(0) in
        let error_count =
          reg_fb spec ~enable:(enable &: fifo_re) ~width:Gfh.bits ~f:(fun d ->
            let sum =
              reduce
                ~f:( +: )
                (Array.to_list
                   (Array.map (fun x -> uresize x Gfh.bits) fy.PForney.O.ferr))
            in
            d +: sum)
        in
        (* for now all submodule outputs *)
        { O_debug.syn; bm; ch; fy; corrected; ordy; error_count }
      ;;

      let create i =
        let { O_debug.syn; bm; ch; fy; corrected; ordy; error_count } =
          create_with_debug i
        in
        { O.corrected; ordy; error_count }
      ;;
    end
  end
end
