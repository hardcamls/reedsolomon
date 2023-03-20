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

  let _chien clocking ~enable ~lambda =
    let f i l =
      reg_fb (Clocking.spec ~clear_to:l clocking) ~enable ~width:Gfh.bits ~f:(fun l ->
          Gfh.cmul (Gfs.antilog i) l)
    in
    let l = List.mapi ~f lambda in
    tree ~arity:2 ~f:(reduce ~f:Gfh.( +: )) l
  ;;

  module State = struct
    type t =
      | Start
      | Run
    [@@deriving sexp_of, enumerate, compare]
  end

  module Var = Always.Variable

  let rp_n = Rp.k + Rp.t + Rp.t
  let cycles_per_codeword = (rp_n + N.n - 1) / N.n

  let chien_ctrl scope spec ~p ~enable ~start =
    let module Gfh = (val Galois.of_scope (module Gp) scope) in
    let sm = Always.State_machine.create (module State) spec ~enable in
    let vld = Var.wire ~default:gnd in
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
                , [ vld <--. 1; cnt <-- cnt_next; when_ cnt_last [ sm.set_next Start ] ] )
              ]
          ])
    in
    ( Array.init p ~f:(fun i ->
          if ((cycles_per_codeword - 1) * p) + i >= rp_n
          then mux2 cnt_last gnd vld.value
          else vld.value)
    , Array.init p ~f:(fun i ->
          let init = of_int ~width:Gfh.bits (1 + i) in
          reg_fb
            (Reg_spec.override spec ~clear_to:init)
            ~enable:(enable &: vld.value)
            ~width:Gfh.bits
            ~f:(fun d -> mux2 cnt_last init (Gfh.modfs (ue d +:. p)))) )
  ;;

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
  let create scope { I.clocking; enable; start; lambda } =
    let module Gfh = (val Galois.of_scope (module Gp) scope) in
    let spec = Clocking.spec clocking in
    let p = N.n in
    let lambda' = Array.map ~f:(fun _ -> wire Gfh.bits) lambda in
    let f i j l = Gfh.cmul (Gfs.antilog (i * j)) l in
    let c = Array.init p ~f:(fun i -> Array.mapi ~f:(f (i + 1)) lambda') in
    let fb = c.(p - 1) in
    let () =
      Array.iteri
        ~f:(fun i l -> lambda'.(i) <== mux2 start l (reg spec ~enable fb.(i)))
        lambda
    in
    let eval =
      Array.map ~f:(fun c -> tree ~arity:2 ~f:(reduce ~f:Gfh.( +: )) (Array.to_list c)) c
    in
    let evld, eloc = chien_ctrl scope spec ~p ~enable ~start in
    let eerr = Array.init p ~f:(fun j -> eval.(j) ==:. 0 &: evld.(j)) in
    { O.eval; eloc; evld; eerr }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"chien" create
  ;;
end
