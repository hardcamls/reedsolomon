open Core
open Hardcaml

module Bit_block = struct
  let print_block b =
    if Fn.non Bits.is_empty b
    then (
      let b = Bits.split_lsb ~part_width:4 b in
      let b = List.map b ~f:Bits.to_int in
      List.iter b ~f:(fun b -> printf "%.1x " b);
      printf "\n")
  ;;

  let get_blocks enc file block_size =
    (* size of file in bits *)
    let size =
      Core_unix.((fstat (descr_of_in_channel file)).st_size |> Int64.to_int_exn) * 8
    in
    (* bit buffer *)
    let buffer = if enc then ref (Bits.of_int ~width:32 size) else ref Bits.empty in
    (* re-fill buffer *)
    let rec fill () =
      if Bits.width !buffer >= block_size
      then ()
      else (
        match In_channel.input_byte file with
        | Some x ->
          buffer := Bits.concat_msb_e [ Bits.of_int ~width:8 x; !buffer ];
          fill ()
        | None ->
          (* In_channel.close file; *)
          ())
    in
    let get () =
      fill ();
      let width = Bits.width !buffer in
      if width = 0
      then (* finished *)
        Bits.empty
      else if width < block_size
      then (
        (* pad *)
        let r = Bits.concat_msb_e [ Bits.zero (block_size - width); !buffer ] in
        buffer := Bits.empty;
        r)
      else (
        (* return buffer *)
        let r = Bits.select_e !buffer (block_size - 1) 0 in
        buffer := Bits.select_e !buffer (width - 1) block_size;
        r)
    in
    let debug = false in
    let get () =
      let r = get () in
      if debug then print_block r;
      r
    in
    get
  ;;
end

module Encode_file = struct
  let run
      ({ m; k; t; n; b; prim_poly; prim_elt } : Reedsolomon.Iter_codec.params)
      in_file
      out_file
    =
    let module Gp = struct
      let pp = prim_poly
      let pe = prim_elt
    end
    in
    let module Rp = struct
      let k = k
      let t = t
      let b = b
    end
    in
    let module Encoder = Hardcaml_reedsolomon.Encoder.Make (Gp) (Rp) in
    let module Sim = Cyclesim.With_interface (Encoder.I) (Encoder.O) in
    let sim = Sim.create Encoder.create in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim in
    let in_file = In_channel.create in_file in
    let get_block = Bit_block.get_blocks true in_file (m * k) in
    let () = printf "m=%i k=%i t=%i n=%i\n" m k t n in
    let file_out = Out_channel.create out_file in
    let enc_buffer = ref Bits.empty in
    let add_enc_buffer d =
      enc_buffer := Bits.concat_msb_e [ d; !enc_buffer ];
      while Bits.width !enc_buffer >= 8 do
        Out_channel.output_byte file_out Bits.(to_int (select_e !enc_buffer 7 0));
        enc_buffer := Bits.(select_e !enc_buffer (width !enc_buffer - 1) 8)
      done
    in
    Cyclesim.reset sim;
    i.enable := Bits.vdd;
    Cyclesim.in_port sim "clear" := Bits.vdd;
    Cyclesim.cycle sim;
    Cyclesim.in_port sim "clear" := Bits.gnd;
    let rec encode_blocks () =
      let block = get_block () in
      if Bits.is_empty block
      then ()
      else (
        (* load data *)
        i.ctrl := Bits.gnd;
        for j = 0 to k - 1 do
          let data = Bits.select block (((j + 1) * m) - 1) (j * m) in
          add_enc_buffer data;
          i.d := data;
          Cyclesim.cycle sim
        done;
        (* read parity *)
        i.ctrl := Bits.vdd;
        let parity = ref [] in
        for _ = 0 to (t * 2) - 1 do
          add_enc_buffer !(o.q);
          parity := Bits.to_int !(o.q) :: !parity;
          Cyclesim.cycle sim
        done;
        print_s [%message (parity : int list ref)];
        encode_blocks ())
    in
    encode_blocks ();
    (* flush and close file *)
    if Bits.width !enc_buffer > 0
    then add_enc_buffer Bits.(zero (8 - Bits.width !enc_buffer));
    Out_channel.close file_out
  ;;

  let command =
    Command.basic
      ~summary:"RS encode a file"
      [%map_open.Command
        let args = Codec_args.args
        and in_file = anon ("IN_FILE" %: string)
        and out_file = anon ("OUT_FILE" %: string) in
        fun () -> run args in_file out_file]
  ;;
end

module Decode_file = struct
  let run
      ({ m; k; t; n; b; prim_poly; prim_elt } : Reedsolomon.Iter_codec.params)
      parallelism
      in_file
      out_file
    =
    let module Gp = struct
      let pp = prim_poly
      let pe = prim_elt
    end
    in
    let module Rp = struct
      let k = k
      let t = t
      let b = b
    end
    in
    let module Parallelism = struct
      let n = parallelism
    end
    in
    let module Decoder = Hardcaml_reedsolomon.Decoder.Make (Gp) (Rp) (Parallelism) in
    let module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O) in
    let sim = Sim.create (Decoder.create (Scope.create ~flatten_design:true ())) in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim in
    let in_file = In_channel.create in_file in
    let bits_per_code_word = m * n in
    let cycles_per_codeword = (n + parallelism - 1) / parallelism in
    let bits_per_data_block = m * k in
    let maxcodes = 0 in
    let get_block = Bit_block.get_blocks false in_file bits_per_code_word in
    let () =
      let open Printf in
      printf "m=%i k=%i t=%i n=%i\n" m k t n;
      printf "parallelism=%i\n" parallelism;
      printf "bits_per_code_word=%i\n" bits_per_code_word;
      printf "cycles_per_code_word=%i\n" cycles_per_codeword
    in
    (* output buffer/file handling *)
    let file_out = Out_channel.create out_file in
    let dec_buffer = ref Bits.empty in
    let file_size_bits = ref 0 in
    let decoded_bits = ref 0 in
    let flushed_bits = ref 0 in
    let flush_dec_buffer () =
      while !flushed_bits < !file_size_bits && Bits.width !dec_buffer >= 8 do
        Out_channel.output_byte file_out Bits.(to_int (select_e !dec_buffer 7 0));
        (dec_buffer := Bits.(select_e !dec_buffer (width !dec_buffer - 1) 8));
        flushed_bits := !flushed_bits + 8
      done
    in
    let add_dec_buffer d =
      dec_buffer := Bits.concat_msb_e [ d; !dec_buffer ];
      flush_dec_buffer ()
    in
    Cyclesim.reset sim;
    i.enable := Bits.vdd;
    let decode_block () =
      let block = get_block () in
      i.clocking.clear := Bits.vdd;
      Cyclesim.cycle sim;
      i.clocking.clear := Bits.gnd;
      (* load received data *)
      i.first := Bits.vdd;
      i.load := Bits.vdd;
      for j = 0 to cycles_per_codeword - 1 do
        for k = 0 to parallelism - 1 do
          let l = (j * parallelism) + k in
          let sym = Bits.select_e block (((l + 1) * m) - 1) (l * m) in
          i.x.(k) := if Bits.is_empty sym then Bits.zero m else sym
        done;
        if j = cycles_per_codeword - 1 then i.last := Bits.vdd;
        Cyclesim.cycle sim;
        i.first := Bits.gnd;
        i.last := Bits.gnd
      done;
      i.load := Bits.gnd;
      let ccnt = ref 0 in
      (* count cycles per code word *)
      let scnt = ref 0 in
      (* count extracted data symbols *)
      while !ccnt < cycles_per_codeword do
        if Bits.to_int !(o.ordy) <> 0
        then (
          for p = 0 to parallelism - 1 do
            if !scnt < k
            then (
              add_dec_buffer !(o.corrected.(p));
              incr scnt)
          done;
          incr ccnt);
        Cyclesim.cycle sim
      done;
      Bits.to_int !(o.error_count)
    in
    let n_codewords = ref 0 in
    let maxcodes () = maxcodes = 0 || !n_codewords < maxcodes in
    (* decode until we have read the header *)
    while maxcodes () && Bits.width !dec_buffer < 32 do
      let error_count = decode_block () in
      Printf.printf
        "decoded %i bits for header [%i errors]\n%!"
        (Bits.width !dec_buffer)
        error_count;
      incr n_codewords
    done;
    (* extract the file size header - also starts flushing process *)
    (file_size_bits := Bits.(to_int (select !dec_buffer 31 0)));
    (dec_buffer := Bits.(select_e !dec_buffer (width !dec_buffer - 1) 32));
    (decoded_bits := Bits.(width !dec_buffer));
    (* decode rest of file *)
    while maxcodes () && !decoded_bits < !file_size_bits do
      let error_count = decode_block () in
      decoded_bits := !decoded_bits + bits_per_data_block;
      Printf.printf
        "decoded %i / %i bits [%i errors]\n%!"
        !decoded_bits
        !file_size_bits
        error_count;
      incr n_codewords
    done;
    (* flush and close file *)
    add_dec_buffer Bits.(zero 8);
    Out_channel.close file_out
  ;;

  let command =
    Command.basic
      ~summary:"RS decode a file"
      [%map_open.Command
        let args = Codec_args.args
        and parallelism =
          flag
            "-parallelism"
            (optional_with_default 1 int)
            ~doc:"PARALLISM code word parallelism"
        and in_file = anon ("IN_FILE" %: string)
        and out_file = anon ("OUT_FILE" %: string) in
        fun () -> run args parallelism in_file out_file]
  ;;
end

let command =
  Command.group
    ~summary:"Encode and decode from files using hardware simulation"
    [ "encode", Encode_file.command; "decode", Decode_file.command ]
;;

module New = struct
  (* Can we choose a better encoding format? 

  10 byte Header

  [0] m [1 byte]
  [1] t [1 bytes]
  [2..9] total_length_in_bits [8 bytes]
*)

  module Header = struct
    type 'a t =
      { m : 'a [@bits 8]
      ; t : 'a [@bits 8]
      ; total_length_in_bits : 'a [@bits 64]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Encode = struct
    type t =
      { buffer_in : Bits.t ref
      ; buffer_out : Bits.t ref
      ; get_byte : unit -> Char.t
      ; put_byte : Char.t -> unit
      ; params : Reedsolomon.Iter_codec.params
      ; message_bits : int
      ; total_length_in_bits : int
      }

    let header (header : int Header.t) =
      Header.Of_bits.of_ints header |> Header.Of_bits.pack
    ;;

    let create
        ~(params : Reedsolomon.Iter_codec.params)
        ~get_byte
        ~put_byte
        ~total_length_in_bits
      =
      { buffer_in = ref Bits.empty
      ; buffer_out =
          ref (header { Header.m = params.m; t = params.t; total_length_in_bits })
      ; get_byte =
          (fun () ->
            try get_byte () with
            | _ -> '\000')
      ; put_byte
      ; params
      ; message_bits = params.m * params.k
      ; total_length_in_bits
      }
    ;;

    let flush_buffer_out { buffer_out; put_byte; _ } =
      while Bits.width !buffer_out >= 8 do
        if Bits.width !buffer_out = 8
        then (
          put_byte (Bits.to_char !buffer_out);
          buffer_out := Bits.empty)
        else (
          let rest, chr = Bits.drop_bottom !buffer_out 8, Bits.sel_bottom !buffer_out 8 in
          put_byte (Bits.to_char chr);
          buffer_out := rest)
      done
    ;;

    let fill_buffer_in { buffer_in; get_byte; message_bits; _ } =
      let d = ref [] in
      let level = ref (Bits.width !buffer_in) in
      while !level < message_bits do
        d := Bits.of_char (get_byte ()) :: !d;
        level := !level + 8
      done;
      buffer_in
        := Bits.concat_msb (if Bits.is_empty !buffer_in then !d else !buffer_in :: !d)
    ;;

    let get_message ({ buffer_in; params = { m; k; _ }; _ } as t) =
      fill_buffer_in t;
      let message =
        Array.init k ~f:(fun j -> Bits.(to_int !buffer_in.:+[m * j, Some m]))
      in
      if Bits.width !buffer_in = t.message_bits
      then buffer_in := Bits.empty
      else buffer_in := Bits.drop_bottom !buffer_in t.message_bits;
      message
    ;;

    let put_codeword ({ buffer_out; params; _ } as t) codeword =
      let codeword =
        Array.map codeword ~f:(Bits.of_int ~width:params.m)
        |> Array.to_list
        |> Bits.concat_lsb
      in
      if Bits.is_empty !buffer_out
      then buffer_out := codeword
      else buffer_out := Bits.concat_lsb [ codeword; !buffer_out ];
      flush_buffer_out t
    ;;

    let encode t =
      let bits_encoded = ref 0 in
      while !bits_encoded < t.total_length_in_bits do
        let message = get_message t in
        let codeword = (* encode *) message in
        put_codeword t codeword
      done
    ;;
  end
end
