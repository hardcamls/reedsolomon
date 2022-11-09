module type S = Matrix_intf.S

module Make (Ops : Ops.S) : S with type elt = Ops.t
