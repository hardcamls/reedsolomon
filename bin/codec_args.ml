open Core

let default_poly = Reedsolomon.Galois.int_of_gf2_prim_poly

let args =
  [%map_open.Command
    let n =
      flag
        "-n"
        (optional int)
        ~doc:"NUM_SYMBOLS number of symbols per codeword (default 255)"
    and t =
      flag
        "-t"
        (optional int)
        ~doc:"CORRECTION_CAPACITY error corrections capacity of code (default 8)"
    and b =
      flag
        "-b"
        (optional_with_default 0 int)
        ~doc:"INITAIL_ROOT starting root of generator (default 0)"
    and prim_poly =
      flag
        "-poly"
        (optional int)
        ~doc:[%string "POLY primitive polynomial (default %{default_poly 8#Int})"]
    and prim_elt =
      flag "-elt" (optional_with_default 2 int) ~doc:"ELT primitive element (default 2)"
    in
    let n = Option.value ~default:255 n in
    if not (Int.is_pow2 (n + 1))
    then
      raise_s
        [%message
          "n must be a equal to a power of two minus 1 (ie 3, 7, 15...255 etc)" (n : int)];
    let t = Option.value ~default:8 t in
    if 2 * t >= n then raise_s [%message "t is too large for given n" (t : int) (n : int)];
    let m = Int.ceil_log2 (n + 1) in
    let prim_poly =
      match prim_poly with
      | None -> default_poly m
      | Some prim_poly -> prim_poly
    in
    { Reedsolomon.Iter_codec.m; k = n - (2 * t); t; n; b; prim_poly; prim_elt }]
;;
