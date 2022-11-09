module type S = Poly_intf.S

(** Basic polynomial representations.  Coefficients are members of
    Ops.OpsBase (which effectively provide '+' and '*') *)
module Make (E : Ops.S) : S with type t = E.t array and type elt = E.t
