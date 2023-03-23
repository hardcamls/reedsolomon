open Base

module type S = sig
  val n : int
end

module type Parallelism = sig
  module type S = S
end
