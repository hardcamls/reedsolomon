(* galois fields of type 2^n in hardware *)
open Base
open Hardcaml

module Make (B : Comb.S) (P : Reedsolomon.Galois.Table_params) = struct
  module G = Reedsolomon.Galois.Int_table_of_params (P)

  type t = B.t

  let n_elems = G.n_elems
  let bits = Base.Int.ceil_log2 G.n_elems
  let alpha = B.of_int ~width:bits G.alpha
  let zero = B.of_int ~width:bits 0
  let one = B.of_int ~width:bits 1
  let enum n f = Array.to_list (Array.init n ~f:(fun i -> B.of_int ~width:bits (f i)))
  let log x = B.mux x (enum n_elems G.log)
  let antilog x = B.mux x (enum n_elems G.antilog)
  let inv x = B.mux x (enum n_elems G.inv)
  let ( +: ) = B.( ^: )
  let ( -: ) = B.( ^: )

  let ( *: ) a b =
    (* a +: b = 0..28 *)
    let a', b' = log a, log b in
    let c =
      B.mux
        B.(ue a' +: ue b')
        (Array.to_list
           (Array.init
              ((2 * n_elems) - 3)
              ~f:(fun i -> B.of_int ~width:bits (G.antilog i))))
    in
    B.mux2 B.(a ==:. 0 |: (b ==:. 0)) zero c
  ;;

  let ( /: ) a b =
    (* a +: b = -14..14 *)
    let a', b' = log a, log b in
    let x =
      Array.to_list
        (Array.init (n_elems - 1) ~f:(fun i -> B.of_int ~width:bits (G.antilog i)))
    in
    let c = B.mux B.(ue a' -: ue b') (x @ [ zero; zero ] @ x) in
    B.mux2 B.(a ==:. 0) zero c
  ;;

  let rec modu s x =
    let open B in
    let w = width x in
    let modv = (1 lsl s) - 1 in
    let cs, cz = of_int ~width:s modv, B.zero s in
    if w < s
    then uresize x w
    else if w = s
    then mux2 (x ==: cs) cz x
    else if w = s + 1
    then
      mux2
        (msbs x ==: cs)
        (B.zero (s - 1) @: lsb x)
        (mux2 (lsbs x ==: cs) cz (lsbs x +: (B.zero (s - 1) @: msb x)))
    else (
      let rec split x =
        if width x <= s
        then [ x ]
        else select x (s - 1) 0 :: split (select x (width x - 1) s)
      in
      let ( +: ) a b =
        let wa, wb = width a, width b in
        let w = max wa wb + 1 in
        uresize a w +: uresize b w
      in
      let sums =
        tree
          ~arity:2
          ~f:
            (function
             | [ a ] -> a
             | [ a; b ] -> a +: b
             | _ -> failwith "bad tree")
          (split x)
      in
      modu s sums)
  ;;

  let modfs = modu bits

  let ( **: ) a n =
    let a' = log a in
    let n = B.(a' *: n) in
    B.mux2 B.(a ==:. 0) (B.mux2 B.(n ==:. 0) one zero) (antilog (modfs n))
  ;;

  let to_string = B.to_string

  let cmul_gates c =
    let n = bits in
    let m = (2 * n) - 1 in
    (* create shifted vectors based on the constant *)
    let shf =
      Array.init m ~f:(fun j ->
        Array.init n ~f:(fun i ->
          let x = j - i in
          if j < i || j >= i + n || c land (1 lsl x) = 0 then 0 else 1))
    in
    (* for powers > n convert modulo the generator poly *)
    for i = n to m - 1 do
      let x = G.antilog i in
      let x = Bits.(of_int ~width:n x |> bits_lsb |> List.rev |> concat_lsb) in
      for j = 0 to n - 1 do
        if Bits.(is_vdd x.:(j))
        then
          for k = 0 to n - 1 do
            shf.(j).(k) <- (shf.(j).(k) + shf.(i).(k)) land 1
          done
      done
    done;
    let shf = Array.init n ~f:(fun i -> shf.(i)) in
    fun x ->
      let x =
        Array.map
          ~f:(fun s ->
            let x = Array.mapi ~f:(fun j s -> if s = 0 then [] else [ B.bit x j ]) s in
            let x = x |> Array.to_list |> List.concat in
            if List.is_empty x then B.gnd else B.reduce ~f:B.( ^: ) x)
          shf
      in
      x |> Array.to_list |> B.concat_lsb
  ;;

  let rom f x = B.mux_init x n_elems ~f:(fun i -> B.of_int ~width:bits (f i))
  let cmul_rom c x = rom (fun i -> G.(c *: i)) x
  let cpow x c = if c = 0 then one else if c = 1 then x else rom (fun i -> G.(i **: c)) x
  let cmul ?(rom = true) = if rom then cmul_rom else cmul_gates
end
