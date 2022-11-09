module type RsParams = sig
  val k : int
  val t : int
  val b : int
end

module type RsPoly = sig
  type elt

  module M : Matrix.S with type t = elt
  module R : Poly.S with type t = elt array and type elt = elt

  type poly = R.t [@@deriving sexp_of]
  type loc = int [@@deriving sexp_of]

  val root : int -> elt
  val generator : poly
  val xn : int -> poly
  val x2t : poly
  val parity : poly -> poly
  val encode : poly -> poly
  val horner : poly -> elt -> elt
  val syndromes : poly -> poly
  val key_equations : poly -> int -> M.matrix * M.matrix
  val solve_key_equations : M.matrix * M.matrix -> M.matrix
  val peterson : poly -> poly
  val euclid_inner : poly * poly -> poly * poly -> poly * poly
  val euclid : ?norm:bool -> ?lim:int -> poly -> poly * poly
  val berlekamp_massey_iter : poly -> int -> poly * poly * int -> poly * poly * int
  val berlekamp_massey : poly -> poly

  module Sarwate : sig
    val iBM : poly -> poly
    val riBM : poly -> poly * poly
    val rriBM : poly -> poly * poly
    val forney : poly -> poly -> loc -> elt
  end

  val chien : poly -> loc list
  val error_location : loc -> int
  val error_magnitude : int -> poly -> poly -> poly
  val deriv : poly -> poly
  val forney : poly -> poly -> loc -> elt
  val error : elt list -> loc list -> poly
  val correct : poly -> poly -> poly
  val decode_euclid : poly -> poly
  val decode_berlekamp_massey : poly -> poly
  val decode_peterson : poly -> poly
  val decode : poly -> poly
  val erasure_locator : int list -> poly
  val zero_erasures : poly -> int list -> poly
  val error_and_erasure : elt list -> loc list -> elt list -> loc list -> poly
  val decode_erasures_euclid : poly -> int list -> poly
  val decode_erasures : poly -> int list -> poly
  val decode_errors_and_erasures_euclid : poly -> int list -> poly

  (*val decode_errors_and_erasures_berlekamp_massey : poly -> int list -> poly*)
  val decode_errors_and_erasures : poly -> int list -> poly
end

module MakePoly (G : Galois.Table_ops) (P : RsParams) = struct
  type elt = G.t

  module M = Matrix.Make (G)
  module R = Poly.Make (G)

  type poly = R.t [@@deriving sexp_of]
  type loc = Base.int [@@deriving sexp_of]

  let root i = G.(alpha **: (P.b + i))

  let generator =
    let roots = Array.init (2 * P.t) (fun i -> [| root i; G.one |]) in
    List.fold_left (fun p' p -> R.(p' *: p)) R.one (Array.to_list roots)
  ;;

  let xn n = R.(one ^: n)
  let x2t = xn (2 * P.t)
  let parity d = R.(snd (d *: x2t /: generator))
  let encode d = R.(trim ((d *: x2t) +: parity d))

  let horner p a =
    let p = List.rev (Array.to_list p) in
    List.fold_left G.(fun acc x -> (acc *: a) +: x) G.zero p
  ;;

  let syndromes r = Array.init (2 * P.t) (fun i -> horner r (root i))

  let key_equations s v =
    M.init v v (fun r c -> s.(v - (c + 1) + r)), M.init v 1 (fun r c -> s.(v + r))
  ;;

  (* K.L = S => K^-1.S = L, iff det(S) <> 0 *)
  let solve_key_equations (k, s) =
    let k' = M.gauss_jordan_inverse k in
    M.(k' *: s)
  ;;

  let peterson s =
    let rec p v =
      if v = 0
      then R.zero
      else (
        let km, kv = key_equations s v in
        (* rather than check the determinant (which is VERY slow), 
         * invert the matrix, then check it actually worked *)
        let km' = M.gauss_jordan_inverse km in
        if M.(km' *: km = identity v)
        then (
          let v = M.(km' *: kv) in
          Array.init (M.rows v + 1) (fun i -> if i = 0 then G.one else v.(i - 1).(0)))
        else p (v - 1))
    in
    p P.t
  ;;

  let euclid_inner (r0, r1) (s0, s1) =
    let q, r = R.(r1 /: r0) in
    let s = R.(s1 -: (q *: s0)) in
    r, s
  ;;

  (* calculate error locator and value polys using extended gcd *)
  let euclid ?(norm = false) ?(lim = P.t) s =
    let open R in
    let rec f (r0, r1) (s0, s1) =
      if degree r0 < lim
      then r0, s0
      else (
        let r, s = euclid_inner (r0, r1) (s0, s1) in
        f (r, r0) (s, s0))
    in
    let v, l = f (R.trim s, x2t) (one, zero) in
    if norm
    then (
      let inv' = G.(one /: l.(0)) in
      Array.map G.(( *: ) inv') v, Array.map G.(( *: ) inv') l)
    else v, l
  ;;

  let berlekamp_massey_iter s k (n, c, l) =
    let get a n = if n >= Array.length a then G.zero else a.(n) in
    let rec e i v =
      if i > l then v else e (i + 1) G.(v +: (get n i *: get s (k - 1 - i)))
    in
    let e = e 1 s.(k - 1) in
    let n, c, l =
      if e = G.zero
      then n, c, l
      else (
        let n' = R.(n +: (c *:. e)) in
        let c, l = if 2 * l < k then R.(n /:. e), k - l else c, l in
        n', c, l)
    in
    n, R.(c ^: 1), l
  ;;

  (* calculate error locator poly using the berlekamp-massey algorithm *)
  let berlekamp_massey s =
    let one, x = R.(one, one ^: 1) in
    let rec f k ((n, c, l) as x) =
      if k > 2 * P.t
      then n
      else (
        let x = berlekamp_massey_iter s k x in
        f (k + 1) x)
    in
    f 1 (one, x, 0)
  ;;

  let deriv l =
    (* set even powers to 0 *)
    let l =
      Array.init (Array.length l) (fun i -> if i mod 2 = 0 then G.zero else l.(i))
    in
    (* divide by x *)
    Array.init (Array.length l - 1) (fun i -> l.(i + 1))
  ;;

  module Sarwate = struct
    let t = P.t
    let get a i = if i < 0 then G.zero else if i >= Array.length a then G.zero else a.(i)

    let foldi f z n =
      let acc = ref z in
      for i = 0 to n - 1 do
        acc := f !acc i
      done;
      !acc
    ;;

    let iteri a f =
      for i = 0 to Array.length a - 1 do
        a.(i) <- f i
      done
    ;;

    let copy t f =
      for i = 0 to Array.length t - 1 do
        t.(i) <- f.(i)
      done
    ;;

    let shiftup a b =
      for i = Array.length a - 1 downto 0 do
        a.(i) <- get a (i - 1)
      done
    ;;

    let shiftdown a b =
      for i = 0 to Array.length a - 1 do
        a.(i) <- get b (i + 1)
      done
    ;;

    let iBM =
      let z, o = G.zero, G.one in
      let lambda = Array.make (t + 1) z in
      let lambda' = Array.make (t + 1) z in
      let b = Array.make (t + 1) z in
      let k = ref 0 in
      let gamma = ref o in
      let delta = ref z in
      let init () =
        let f i = if i = 0 then o else z in
        iteri lambda f;
        iteri b f;
        k := 0;
        gamma := o;
        delta := z
      in
      let iter s =
        let update_delta r d i = G.(d +: (get s (r - i) *: lambda.(i))) in
        let update_lambda i = G.((!gamma *: lambda.(i)) -: (!delta *: get b (i - 1))) in
        for r = 0 to (2 * t) - 1 do
          (* step 1 *)
          delta := foldi (update_delta r) z (t + 1);
          (* step 2 *)
          copy lambda' lambda;
          iteri lambda update_lambda;
          (* step 3 *)
          if !delta <> z && !k >= 0
          then (
            copy b lambda';
            (* previous lambda *)
            gamma := !delta;
            k := - !k - 1)
          else (
            shiftup b b;
            gamma := !gamma;
            k := !k + 1)
        done
        (* XXX step 4 *)
      in
      fun s ->
        init ();
        iter s;
        Array.init (t + 1) (fun i -> lambda.(i))
    ;;

    let riBM =
      let z, o = G.zero, G.one in
      let lambda = Array.make (t + 1) z in
      let lambda' = Array.make (t + 1) z in
      let b = Array.make (t + 1) z in
      let delta = Array.make (2 * t) z in
      let delta' = Array.make (2 * t) z in
      let theta = Array.make (2 * t) z in
      let gamma = ref o in
      let k = ref 0 in
      let init s =
        let f i = if i = 0 then o else z in
        iteri lambda f;
        iteri b f;
        copy delta s;
        copy theta s;
        gamma := o;
        k := 0
      in
      let iter () =
        let update_lambda i =
          G.((!gamma *: lambda.(i)) -: (delta'.(0) *: get b (i - 1)))
        in
        let update_delta i =
          G.((!gamma *: get delta' (i + 1)) -: (delta'.(0) *: theta.(i)))
        in
        for r = 0 to (2 * t) - 1 do
          (* step 1 *)
          copy lambda' lambda;
          copy delta' delta;
          iteri lambda update_lambda;
          iteri delta update_delta;
          (* step 2 *)
          if delta'.(0) <> z && !k >= 0
          then (
            copy b lambda';
            (* previous lambda *)
            shiftdown theta delta';
            gamma := delta'.(0);
            k := - !k - 1)
          else (
            shiftup b b;
            (*copy theta theta;
              gamma := !gamma;*)
            k := !k + 1)
        done
      in
      fun s ->
        init s;
        iter ();
        Array.init t (fun i -> delta.(i)), Array.init (t + 1) (fun i -> lambda.(i))
    ;;

    let rriBM =
      let z, o = G.zero, G.one in
      let delta = Array.make ((3 * t) + 1) z in
      let delta' = Array.make ((3 * t) + 1) z in
      let theta = Array.make ((3 * t) + 1) z in
      let gamma = ref o in
      let k = ref 0 in
      let init s =
        let f i = if i < 2 * t then s.(i) else if i = 3 * t then o else z in
        iteri delta f;
        copy theta delta;
        gamma := o;
        k := 0
      in
      let iter () =
        let update_delta i =
          G.((!gamma *: get delta' (i + 1)) -: (delta'.(0) *: theta.(i)))
        in
        for i = 0 to (2 * t) - 1 do
          (* step 1 *)
          copy delta' delta;
          iteri delta update_delta;
          (* step 2 *)
          if delta'.(0) <> z && !k >= 0
          then (
            shiftdown theta delta';
            gamma := delta'.(0);
            k := - !k - 1)
          else (*copy theta theta;
              gamma := !gamma;*)
            k := !k + 1
        done
      in
      fun s ->
        init s;
        iter ();
        Array.init t (fun i -> delta.(i)), Array.init (t + 1) (fun i -> delta.(t + i))
    ;;

    let forney v l =
      let l' = deriv l in
      fun x' ->
        let x' = G.antilog x' in
        let x = G.(x' **: (P.b + (2 * P.t) - 1)) in
        G.(x *: (horner v x' /: horner l' x'))
    ;;
  end

  let chien l =
    let rec f n =
      if n = G.n_elems - 1
      then []
      else if horner l (G.antilog n) = G.zero
      then n :: f (n + 1)
      else f (n + 1)
    in
    f 0
  ;;

  let error_location n = G.(log (inv (antilog n)))

  let error_magnitude v l s =
    let get a n = if n >= Array.length a then G.zero else a.(n) in
    Array.init v (fun i ->
      let a = Array.init (i + 1) (fun j -> G.(get s j *: get l (i - j))) in
      Array.fold_left G.( +: ) G.zero a)
  ;;

  let forney v l =
    let l' = deriv l in
    fun x' ->
      let x' = G.antilog x' in
      let x = G.(x' **: (P.b - 1)) in
      G.(x *: (horner v x' /: horner l' x'))
  ;;

  let error v l =
    let x = List.map2 (fun a b -> error_location a, b) l v in
    let n = P.k + (P.t * 2) in
    R.to_poly
      (Array.init n (fun i ->
         try List.assoc i x with
         | _ -> G.zero))
  ;;

  let correct r e = R.(r +: e)

  (* error correction *)

  let decode_euclid r =
    let s = syndromes r in
    if Array.fold_left (fun b s -> b && s = G.zero) true s
    then r
    else (
      let v, l = euclid s in
      let el = chien l in
      let ev = List.map (forney v l) el in
      let e = error ev el in
      correct r e)
  ;;

  let decode_berlekamp_massey r =
    let s = syndromes r in
    if Array.fold_left (fun b s -> b && s = G.zero) true s
    then r
    else (
      let l = berlekamp_massey s in
      let el = chien l in
      let v = error_magnitude (List.length el) l s in
      let ev = List.map (forney v l) el in
      let e = error ev el in
      correct r e)
  ;;

  let decode_peterson r =
    let s = syndromes r in
    if Array.fold_left (fun b s -> b && s = G.zero) true s
    then r
    else (
      let l = peterson s in
      let el = chien l in
      let v = error_magnitude (List.length el) l s in
      let ev = List.map (forney v l) el in
      let e = error ev el in
      correct r e)
  ;;

  let decode = decode_euclid

  (* erasure correction *)

  let erasure_locator y =
    let terms = List.map (fun y -> [| G.one; G.(alpha **: y) |]) y in
    List.fold_left (fun a x -> R.(a *: x)) R.one terms
  ;;

  let zero_erasures r y =
    let r = Array.copy r in
    List.iter (fun y -> r.(y) <- G.zero) y;
    r
  ;;

  let error_and_erasure ev el fv fl =
    let e = List.map2 (fun a b -> error_location a, b) el ev in
    let f = List.map2 (fun a b -> a, b) fl fv in
    let x = e @ f in
    let n = P.k + (P.t * 2) in
    R.to_poly
      (Array.init n (fun i ->
         try List.assoc i x with
         | _ -> G.zero))
  ;;

  let decode_erasures_euclid r y =
    if y = []
    then r
    else (
      let f = List.length y in
      let tau = erasure_locator y in
      let r = zero_erasures r y in
      let s = syndromes r in
      let xi = R.(slice (tau *: s) ((2 * P.t) - 1)) in
      let omega, lambda = euclid ~norm:true ~lim:(P.t + (f / 2) + 0) xi in
      let phi = R.(tau *: lambda) in
      let forney = forney omega phi in
      let fv = List.map forney (List.map (fun y -> G.(log (inv (alpha **: y)))) y) in
      let e = error_and_erasure [] [] fv y in
      correct r e)
  ;;

  let decode_erasures = decode_erasures_euclid

  let decode_errors_and_erasures_euclid r y =
    let f = List.length y in
    let tau = erasure_locator y in
    let r = zero_erasures r y in
    let s = syndromes r in
    let xi = R.(slice (tau *: s) ((2 * P.t) - 1)) in
    let omega, lambda = euclid ~norm:true ~lim:(P.t + (f / 2) + 0) xi in
    let el = chien lambda in
    let phi = R.(tau *: lambda) in
    let forney = forney omega phi in
    let ev = List.map forney el in
    let fv = List.map forney (List.map (fun y -> G.(log (inv (alpha **: y)))) y) in
    let e = error_and_erasure ev el fv y in
    correct r e
  ;;

  let decode_errors_and_erasures_berlekamp_massey r y =
    (*let f = List.length y in*)
    let tau = erasure_locator y in
    let r = zero_erasures r y in
    let s = syndromes r in
    let xi = R.(slice (tau *: s) ((2 * P.t) - 1)) in
    (*let omega, lambda = euclid ~norm:true ~lim:(P.t + (f / 2) + 0) xi in*)
    let lambda = berlekamp_massey xi in
    let omega = R.(slice (lambda *: xi) ((2 * P.t) - 1)) in
    let el = chien lambda in
    let phi = R.(tau *: lambda) in
    let forney = forney omega phi in
    let ev = List.map forney el in
    let fv = List.map forney (List.map (fun y -> G.(log (inv (alpha **: y)))) y) in
    let e = error_and_erasure ev el fv y in
    correct r e
  ;;

  let decode_errors_and_erasures = decode_errors_and_erasures_euclid
end
