open Base

let rec tree_depth n x =
  if x <= 1 then 0 else if x <= n then 1 else 1 + tree_depth n ((x + n - 1) / n)
;;
