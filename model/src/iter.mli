type rsparams =
  { m : int (* bits per symbol *)
  ; k : int (* message symbols *)
  ; t : int (* correction capability, 2t=parity symbols *)
  ; n : int (* codeword symbols *)
  ; b : int (* starting root of generator *)
  ; prim_poly : int (* primitive polynomial *)
  ; prim_elt : int (* primitive element *)
  }
[@@deriving sexp_of]

type rspoly = int array

val rspoly : int -> rspoly

type rscodec =
  { params : rsparams
  ; encode : rspoly -> rspoly -> unit
  ; decode : rspoly -> rspoly -> int
  }

val init : rsparams -> rscodec
