(* some simple matrix routines for addition, multiplication etc *)

module type S = Matrix_intf.S

module Make (Ops : Ops.S) = struct
  include struct
    open Base

    type t = Ops.t [@@deriving sexp_of]
    type matrix = t array array [@@deriving sexp_of]
  end

  let rows m = Array.length m
  let cols m = Array.length m.(0)

  (*let print ?(c=stdout) conv width m = 
      let open Printf in
      for row=0 to rows m - 1 do
          for col=0 to cols m - 1 do
              fprintf c "%*s " width (conv m.(row).(col))
          done;
          fprintf c "\n"
      done*)

  let init rows cols f = Array.init rows (fun r -> Array.init cols (fun c -> f r c))
  let create rows cols = init rows cols (fun _ _ -> Ops.zero)
  let copy m = init (rows m) (cols m) (fun r c -> m.(r).(c))
  let identity n = init n n (fun r c -> if r = c then Ops.one else Ops.zero)
  let transpose m = init (cols m) (rows m) (fun c r -> m.(r).(c))
  let map f m = init (rows m) (cols m) (fun r c -> f m.(r).(c))
  let map2 f m0 m1 = init (rows m0) (cols m1) (fun r c -> f m0.(r).(c) m1.(r).(c))
  let col_vector a = init (Array.length a) 1 (fun c _ -> a.(c))
  let row_vector a = init 1 (Array.length a) (fun _ r -> a.(r))

  let ( >> ) a b =
    assert (rows a = rows b);
    let ca, cb = cols a, cols b in
    init (rows a) (ca + cb) (fun row col ->
      if col < ca then a.(row).(col) else b.(row).(col - ca))
  ;;

  let ( ^^ ) a b =
    assert (cols a = cols b);
    let ra, rb = rows a, rows b in
    init (ra + rb) (cols a) (fun row col ->
      if row < ra then a.(row).(col) else b.(row - ra).(col))
  ;;

  let sub row_off col_off rows cols m =
    init rows cols (fun r c -> m.(row_off + r).(col_off + c))
  ;;

  let ( +: ) a b =
    assert (rows a = rows b);
    assert (cols a = cols b);
    init (rows a) (cols a) (fun r c -> Ops.(a.(r).(c) +: b.(r).(c)))
  ;;

  let ( -: ) a b =
    assert (rows a = rows b);
    assert (cols a = cols b);
    init (rows a) (cols a) (fun r c -> Ops.(a.(r).(c) -: b.(r).(c)))
  ;;

  let ( *: ) a b =
    let ca, cb = cols a, cols b in
    let ra, rb = rows a, rows b in
    assert (ca = rb);
    let mult row col =
      let rec f i x =
        if i = ca then x else f (i + 1) Ops.(x +: (a.(row).(i) *: b.(i).(col)))
      in
      f 0 Ops.zero
    in
    init ra cb mult
  ;;

  let ( *:. ) a b = init (rows a) (cols a) (fun r c -> Ops.(a.(r).(c) *: b))

  let minor row col m =
    assert (row < rows m);
    assert (col < cols m);
    init
      (rows m - 1)
      (cols m - 1)
      (fun r c ->
        let r = if r < row then r else r + 1 in
        let c = if c < col then c else c + 1 in
        m.(r).(c))
  ;;

  let rec det m =
    assert (rows m = cols m);
    let n = cols m in
    if n = 1
    then m.(0).(0) (*else if n=2 then det2 m*)
    else (
      let rec f i =
        if i = n
        then Ops.zero
        else (
          let m' = minor 0 i m in
          let op = if i mod 2 = 0 then Ops.( -: ) else Ops.( +: ) in
          Ops.(op (m.(0).(i) *: det m') (f (i + 1))))
      in
      f 0)
  ;;

  let adjoint_inverse m =
    let d = det m in
    if d = Ops.zero
    then failwith "cannot invert"
    else (
      let m =
        init (rows m) (cols m) (fun r c ->
          let inv = (r + c) mod 2 = 1 in
          let d = det (minor r c m) in
          if inv then Ops.(zero -: d) else d)
      in
      d, transpose m)
  ;;

  let gauss_jordan m =
    let open Ops in
    let m = copy m in
    (* scale row r by s *)
    let scale_row r s =
      for i = 0 to Array.length r - 1 do
        r.(i) <- r.(i) /: s
      done
    in
    (* scale row y by s and subtract from x *)
    let sub_scale_row x y s =
      for i = 0 to Array.length x - 1 do
        x.(i) <- x.(i) -: (y.(i) *: s)
      done
    in
    (* find the biggest element at the diagonal position between row r and the
       rows below. move the largest element to the pivot position. at worst this
       will avoid 0 pivots. it should also help numerical stability a bit *)
    let pivot r =
      let max = ref r in
      for i = r + 1 to rows m - 1 do
        if m.(i).(r) > m.(!max).(r) then max := i
      done;
      let a, b = m.(r), m.(!max) in
      m.(r) <- b;
      m.(!max) <- a
    in
    for row = 0 to rows m - 1 do
      pivot row;
      scale_row m.(row) m.(row).(row);
      for i = 0 to rows m - 1 do
        if i <> row then sub_scale_row m.(i) m.(row) m.(i).(row)
      done
    done;
    m
  ;;

  let gauss_jordan_inverse m =
    let n = rows m in
    let m = m >> identity n in
    let m = gauss_jordan m in
    sub 0 n n n m
  ;;

  (* elementary row operations *)
  module Row = struct
    let swap n i j =
      init n n (fun r c ->
        if r = i
        then if c = j then Ops.one else Ops.zero
        else if r = j
        then if c = i then Ops.one else Ops.zero
        else if r = c
        then Ops.one
        else Ops.zero)
    ;;

    let mult n i m =
      init n n (fun r c -> if r = c then if r = i then m else Ops.one else Ops.zero)
    ;;

    let madd n i j m =
      init n n (fun r c ->
        if r = j && c = i then m else if r = c then Ops.one else Ops.zero)
    ;;
  end
end
