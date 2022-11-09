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

  module Serial = struct
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
      let eval poly x =
        let a = Array.mapi ~f:Gfh.(fun pow coef -> coef *: cpow x pow) poly in
        let a = Array.map ~f:(fun d -> reg d) a in
        let add_p a = reg (reduce ~f:Gfh.( +: ) a) in
        tree ~arity:n_tree ~f:add_p (Array.to_list a)
      in
      let v_depth = Util.tree_depth n_tree (Array.length v) in
      let l_depth = Util.tree_depth n_tree (Array.length l) in
      assert (v_depth >= l_depth);
      let xv = reg Gfh.(antilog x) in
      let xl = reg Gfh.(rom (fun i -> Gfs.(i **: 2)) xv) -- "xl" in
      let xv = reg xv -- "xv" in
      let v = eval v xv -- "ev" in
      let l = eval l xl -- "el_" in
      let l = pipe (v_depth - l_depth) l -- "el" in
      let x = pipe (1 + v_depth) Gfh.(cpow xv (Rp.b + (2 * Rp.t) - 1)) -- "xp" in
      { O.emag = reg Gfh.(x *: (v /: l))
      ; frdy = pipe (4 + v_depth) vld
      ; ferr = pipe (4 + v_depth) err
      }
    ;;
  end

  module Parallel = struct
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
        Array.init N.n ~f:(fun j ->
          Forney.create
            { Forney.I.clocking; enable; vld = vld.(j); err = err.(j); v; l; x = x.(j) })
      in
      O.
        { emag = Array.map ~f:(fun { Forney.O.emag; _ } -> emag) o
        ; frdy = Array.map ~f:(fun { Forney.O.frdy; _ } -> frdy) o
        ; ferr = Array.map ~f:(fun { Forney.O.ferr; _ } -> ferr) o
        }
    ;;
  end
end
