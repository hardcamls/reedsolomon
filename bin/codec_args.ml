open Core

let default_poly idx =
  if idx < 2
  then raise_s [%message "number of bits must be > 1"]
  else
    Array.foldi Reedsolomon.Galois.gf2_prim_polys.(idx) ~init:0 ~f:(fun idx acc b ->
      acc + if b = 0 then 0 else b lsl idx)
;;

let args =
  [%map_open.Command
    let n = flag "-n" (optional int) ~doc:"NUM_SYMBOLS number of symbols per codeword"
    and t = flag "-t" (optional int) ~doc:"PARITY_BYTES numbers of parity bytes"
    and b =
      flag
        "-b"
        (optional_with_default 0 int)
        ~doc:"INITAIL_ROOT starting root of generator"
    and prim_poly = flag "-poly" (optional int) ~doc:"POLY primitive polynomial"
    and prim_elt =
      flag "-elt" (optional_with_default 2 int) ~doc:"ELT primitive element"
    in
    let n = Option.value ~default:255 n in
    if not (Int.is_pow2 (n + 1))
    then
      raise_s
        [%message
          "n must be a equal to a power of two minus 1 (ie 3, 7, 15...255 etc)" (n : int)];
    let t = Option.value ~default:8 t in
    if 2 * t >= n then raise_s [%message "t is too large for given b" (t : int) (n : int)];
    let m = Int.ceil_log2 (n + 1) in
    let prim_poly =
      match prim_poly with
      | None -> default_poly m
      | Some prim_poly -> prim_poly
    in
    { Reedsolomon.Iter_codec.m; k = n - (2 * t); t; n; b; prim_poly; prim_elt }]
;;
