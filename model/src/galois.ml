module type Primitive_field_prime = Galois_intf.Primitive_field_prime

(* GF(n) primitive finite field *)
module Primitive_field (Prime : Primitive_field_prime) = struct
  open Prime

  (* Note: [n] must be prime.  We should add a test. *)

  type t = Base.int [@@deriving sexp_of]

  let zero = 0
  let one = 1
  let ( +: ) a b = (a + b) mod n
  let ( -: ) a b = (a - b + n) mod n
  let ( *: ) a b = a * b mod n

  (* this builds a potentially huge table to construct inverses. We could also
       use the ext_gcd algorithm. see poly.ml *)
  let inv =
    let rec find x v =
      if v = 0
      then 0
      else if x = n
      then failwith "couldn't find inverse"
      else if v *: x = 1
      then x
      else find (x + 1) v
    in
    Array.init n (find 0)
  ;;

  let ( /: ) a b = a *: inv.(b)
  let to_string = string_of_int
end

(* GF(2) *)
module GF2 = struct
  type t = Base.int [@@deriving sexp_of]

  let zero = 0
  let one = 1
  let ( +: ) = ( lxor )
  let ( -: ) = ( lxor )
  let ( *: ) = ( land )

  let ( /: ) a b =
    (* This matches the GFN construction. If we raised on division by [0], we
         could just return [a]. *)
    (a + b) lsr 1
  ;;

  let to_string = string_of_int
end

module type Extension_field_generator = Galois_intf.Extension_field_generator

(* GF extension fields built from primitive fields and polynomials *)
module Extension_field (G : Extension_field_generator) = struct
  type t = G.Poly.t [@@deriving sexp_of]

  let zero = G.Poly.zero
  let one = G.Poly.one
  let ( +: ) = G.Poly.( +: )
  let ( -: ) = G.Poly.( -: )
  let ( *: ) a b = snd G.Poly.(a *: b /: G.pp)

  let ( /: ) a b =
    let _, b' = G.Poly.ext_gcd G.pp b in
    a *: b'
  ;;

  let to_string = G.Poly.to_string ~down:true ~str:G.Poly.(string_format true poly_format)
end

module type GF2_extension_field_generator = Galois_intf.GF2_extension_field_generator

module GF2N (P : sig
  val pp : int array
end) =
Extension_field (struct
  module Poly = Poly.Make (GF2)

  let pp = P.pp
end)

(* list of primitive polynomials for various GF(2) extension fields *)
let gf2_prim_polys =
  let mk_poly pp =
    Array.init
      (1 + List.fold_left (fun a v -> max a v) 0 pp)
      (fun i -> if List.mem i pp then 1 else 0)
  in
  Array.map
    mk_poly
    [| []
     ; (* 0 to 2 we dont have primitive polys listed *)
       []
     ; [ 0; 1; 2 ]
     ; (* wasnt in the table, but appears to be correct. *)
       [ 0; 1; 3 ]
     ; [ 0; 1; 4 ]
     ; [ 0; 2; 5 ]
     ; [ 0; 1; 6 ]
     ; [ 0; 3; 7 ]
     ; [ 0; 2; 3; 4; 8 ]
     ; [ 0; 4; 9 ]
     ; [ 0; 3; 10 ]
     ; [ 0; 2; 11 ]
     ; [ 0; 1; 4; 6; 12 ]
     ; [ 0; 1; 3; 4; 13 ]
     ; [ 0; 1; 6; 10; 14 ]
     ; [ 0; 1; 15 ]
     ; [ 0; 1; 3; 12; 16 ]
     ; [ 0; 3; 17 ]
     ; [ 0; 7; 18 ]
     ; [ 0; 1; 2; 5; 19 ]
     ; [ 0; 3; 20 ]
     ; [ 0; 2; 21 ]
     ; [ 0; 1; 22 ]
     ; [ 0; 5; 23 ]
     ; [ 0; 1; 2; 7; 24 ]
    |]
;;

module type Table_generator = Galois_intf.Table_generator
module type Table_ops = Galois_intf.Table_ops
module type Table_params = Galois_intf.Table_params

module Table (G : Table_generator) = struct
  include G.Ops

  module PM = Map.Make (struct
    type t = G.Ops.t

    let compare = compare
  end)

  module IM = Map.Make (struct
    type t = int

    let compare = compare
  end)

  let alpha = G.alpha

  (* map poly to corresponding power of alpha *)
  let pow_map =
    let rec f a n map =
      (* element already in map, so we are done *)
      try
        let _ = PM.find a map in
        map
      with
      (* add power to map *)
      | Not_found -> f (a *: alpha) (n + 1) (PM.add a n map)
      (* some other problem *)
      | _ -> failwith "log_map exception"
    in
    f alpha 1 (PM.add one 0 PM.empty)
  ;;

  (* map power of alpha to poly *)
  let ipow_map = PM.fold (fun k v m -> IM.add v k m) pow_map IM.empty
  let n_elems = PM.cardinal pow_map + 1
  let log a = PM.find a pow_map
  let antilog a = IM.find a ipow_map

  let ( *: ) a b =
    if a = zero || b = zero
    then zero
    else (
      let a, b = log a, log b in
      antilog ((a + b) mod (n_elems - 1)))
  ;;

  let ( /: ) a b =
    if a = zero || b = zero
    then zero (*else if b = zero then failwith "divide by zero"*)
    else (
      let a, b = log a, log b in
      antilog ((a - b + n_elems - 1) mod (n_elems - 1)))
  ;;

  let inv a =
    if a = zero
    then zero (* cant invert *)
    else (
      let a = log a in
      antilog ((n_elems - 1 - a) mod (n_elems - 1)))
  ;;

  let rec ( **: ) a n =
    if a = zero
    then zero
    else if n < 0
    then inv a **: -n
    else (
      let a = log a in
      antilog (a * n mod (n_elems - 1)))
  ;;
end

module To_int_table_field (Ops : Table_ops with type t = int array) = struct
  type t = Base.int [@@deriving sexp_of]

  let zero = 0
  let one = 1
  let ( +: ) = ( lxor )
  let ( -: ) = ( lxor )
  let to_string = string_of_int
  let n_elems = Ops.n_elems

  let to_int x =
    fst (List.fold_left (fun (a, p) x -> (x * p) + a, p * 2) (0, 1) (Array.to_list x))
  ;;

  let to_poly x =
    let rec f v =
      if v = 0 then [] else if v land 1 = 1 then 1 :: f (v lsr 1) else 0 :: f (v lsr 1)
    in
    if x = 0 then [| 0 |] else Array.of_list (f x)
  ;;

  let alpha = to_int Ops.alpha

  let mk_table min max f =
    let len = max - min + 1 in
    let a = Array.init len (fun i -> f (i + min)) in
    fun i -> a.(i - min)
  ;;

  let rec index i = if i < 0 then index (i + n_elems - 1) else i mod (n_elems - 1)
  let log' i = if i = 0 then 0 else Ops.log (to_poly i)
  let log = mk_table 0 (n_elems - 1) log'

  let antilog' i' =
    let i = index i' in
    to_int (Ops.antilog i)
  ;;

  let antilog = mk_table (2 - n_elems) ((2 * (n_elems - 2)) + 1) antilog'

  let inv a =
    if a = 0
    then 0
    else (
      let a = log a in
      antilog (n_elems - 1 - a))
  ;;

  let ( *: ) a b =
    if a = zero || b = zero
    then zero
    else (
      let a, b = log a, log b in
      antilog (a + b))
  ;;

  let ( /: ) a b =
    if a = zero || b = zero
    then zero (*else if b = zero then failwith "divide by zero"*)
    else (
      let a, b = log a, log b in
      antilog (a - b))
  ;;

  let rec ( **: ) a n =
    if n = 0
    then one
    else if a = zero
    then zero
    else (
      let n = index (log a * n) in
      antilog n)
  ;;
end

module Generator_of_table_params (P : Table_params) = struct
  let mk_gf_param x =
    let rec f x = if x = 0 then [] else (if x land 1 = 0 then 0 else 1) :: f (x lsr 1) in
    Array.of_list (f x)
  ;;

  module Ops = Extension_field (struct
    module Poly = Poly.Make (GF2)

    let pp = mk_gf_param P.pp
  end)

  let alpha = mk_gf_param P.pe
end

module Int_table_of_params (P : Table_params) =
  To_int_table_field (Table (Generator_of_table_params (P)))
