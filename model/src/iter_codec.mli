type params =
  { m : int (* bits per symbol *)
  ; k : int (* message symbols *)
  ; t : int (* correction capability, 2t=parity symbols *)
  ; n : int (* codeword symbols *)
  ; b : int (* starting root of generator *)
  ; prim_poly : int (* primitive polynomial *)
  ; prim_elt : int (* primitive element *)
  }
[@@deriving sexp_of]

type poly = int array

val rspoly : int -> poly

type t =
  { params : params
  ; encode : poly -> poly -> unit
  ; decode : poly -> poly -> int
  }

val init : params -> t
