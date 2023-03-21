# Reedsolomon

OCaml library implementing a Reed-Solomon error correction CODEC and corresponding 
implementation in Hardcaml.

![build status](https://github.com/hardcamls/reedsolomon/actions/workflows/main.yml/badge.svg)

# About RS coding

A Reed-Solomon (RS) CODEC takes blocks of symbols and adds some number of parity symbols
to form a code word.  The code word may become corrupted (by some transmission or storage
medium) and the original message can be recovered by the RS decoder if the number of
corrupted symbols is less than or equal to half the number of parity symbols added.

The following parameters define the RS code:

* A symbol is some number of bits in size defined by the parameter `m`.
* The code word is specified by the parameter `n` which is equal to `(2^m) - 1`.
* The error correction capability of the codec is `t`.
* The size of the message block is `n - 2*t`.

The symbols in the codec are elements of a Galois field.  Although important to the
mathematics behind RS coding, they are not particularly important for using it.

# Project structure

- `model` A software reference model with both an abstract polynomial implementation and fast itertive version.
- `hardcaml` A Hardcaml implementation of encoding and decoding.
- `bin` Top level application with various test utilities for the sofware model, hardware testbenches and Verilog generation.


