module type S = Ops_intf.S
module type Extended = Ops_intf.Extended

module Int : Extended with type t = int
module Int32 : Extended with type t = int32
module Int64 : Extended with type t = int64
module Float : Extended with type t = float
