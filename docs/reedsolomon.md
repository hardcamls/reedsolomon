# Reed-Solomon Coding

<!--
```ocaml
open Base
open Reedsolomon
```
-->

## Introduction

Reed-Solomon is a forward error correction CODEC.  Forward error correction provides a way to protect data
from errors incurred during transmission or storage - an inevitability in out imperfect world.  We will 
introduce the main concepts of Reed-Solomon coding but for a much more thorough discussion please refer
to this rather execellent [BBC white paper](https://downloads.bbc.co.uk/rd/pubs/whp/whp-pdf-files/WHP031.pdf).

## Block structure

We start with the message we wish to send (or store).  The message is of length `k` and comprised of `m` bit symbols. 
To this the Reed-Solomon encoder will add `2*t` parity protection symbols to form a code word of length `n`.

> block diagram here

The decoder will receive the code word which may have been corrupted.  If the number of errors is less than
some threshold it will recover the original message intact.

## Parameter relationships

The parameters are related by a couple of equations:

$n <= 2^m - 1$

Thus the number of bits per symbol limits the length of the code word.  Many practical codes in choose $m=8$ 
allowing codewords of up to 255 bytes.

$n = k + 2*t$

`t` defines how many symbol errors we can correct and `2*t` is how many parity protection symbols need 
to be added to the message.  Note that since the parity symbols must be sent or stored along with the 
original message there is a cost to simply increasing `t` arbitrarily.

Codes are often defined in terms of `(n, k)` where 

$t = (n-k)/2$

## An example code

We will use the code `(15, 4)` as an example throughout this tutorial.

> block diagram here

> `m=4`, `n=15`, `k=11`, `t=2`

Encoding and decoding would look like this

|-------------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| message           | 12| 8 | 1 | 5 | 7 | 3 | 13| 4 | 2 | 9 | 11|   |   |   |   |
| code word         | 12| 9 | 1 | 3 | 7 | 3 | 13| 4 | 2 | 9 | 11|12 | 5 | 0 | 1 |
| received code word| 12| 9 | 15| 3 | 7 | 3 | 13| 4 | 2 | 9 | 11|12 | 5 | 8 | 1 |
| decoded code word | 12| 9 | 1 | 3 | 7 | 3 | 13| 4 | 2 | 9 | 11|12 | 5 | 0 | 1 |

> Redo as a block diagram

In the table 2 symbols have been corrupted, but since this is within the correction capacity of 
the code they are corrected.

## Polynomials

The code word can be thought of as a polynomial of degree $2^m-1$ where the symbols are elements
of the Galois extension field $GF2^m$.

Our previous example code word may be rewritten as the following polynomial:

$12x^15 + 8x^14 + 8x^13 + 1x^12 + 3x^11 + 7x^10 + 3x^9 + 13x^8 + 4x^7 + 2x^6 + 9x^5 + 11x^4 + 12x^3 + 5x^2 + 0x^1 + 1$

The polynomial form will be used to describe the various mathematical formula involved with Reed-Solomon coding.

