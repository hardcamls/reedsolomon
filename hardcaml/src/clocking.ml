open! Base
open Hardcaml
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
