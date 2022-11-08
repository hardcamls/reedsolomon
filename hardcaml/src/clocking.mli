open! Base
open Hardcaml

type 'a t =
  { clock : 'a
  ; clear : 'a
  }
[@@deriving sexp_of, hardcaml]

val spec : ?clear_to:Signal.t -> ?extra_clear:Signal.t -> Signal.t t -> Reg_spec.t
