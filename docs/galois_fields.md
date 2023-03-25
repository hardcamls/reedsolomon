# Galois Fields 

<!--
```ocaml
open Core
open Reedsolomon
let printf = Caml.Printf.printf
```
-->

Galois fields are fields with a finite number of elements over which operations
such as addition, subtraction, multiplication and division may be defined.

Simple Galois fields may be constructed by simply perfoming arithmetic operations
modulo some prime number.

## GF7

We can explore Galois fields using the `Reedsolomon.Galois` module.

```ocaml
# module GF7 = Galois.Primitive_field(struct let n = 7 end)
module GF7 :
  sig
    type t = int
    val sexp_of_t : t -> Core.Sexp.t
    val compare : t Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    val zero : t
    val one : t
    val ( +: ) : t -> t -> t
    val ( -: ) : t -> t -> t
    val ( *: ) : t -> t -> t
    val ( /: ) : t -> t -> t
    val to_string : t -> string
  end
```

This field is made up of the numbers `0...6` and provides general mathematical operations.  
Lets see what happens if we add 3 to every element of the field.

```ocaml
# Array.init 7 ~f:(fun i -> GF7.(i +: 3))
- : int array = [|3; 4; 5; 6; 0; 1; 2|]
```

We can see this is simple integer addition taken modulo the 7 (the size of the field).

## Primitive or generator elements 

A primitive element is a value within the field whose successive powers
generate all non-zeros values in the field.

```ocaml
# let primitive_element = 3
val primitive_element : int = 3
# let rec powers g n = 
    if n=6 then []
    else 
       g :: powers GF7.(g *: primitive_element) (n+1);;
val powers : int -> int -> int list = <fun>
# let powers = powers primitive_element 0
val powers : int list = [3; 2; 6; 4; 5; 1]
```

We can therefore rewrite non-zero elements of the field as a power of the primitive 
element.

```ocaml
# let as_powers = Array.init 7 ~f:(fun i -> List.findi powers ~f:(fun power value -> value=i)) 
val as_powers : (int * int) option array =
  [|None; Some (5, 1); Some (1, 2); Some (0, 3); Some (3, 4); Some (4, 5);
    Some (2, 6)|]
# Array.iter as_powers ~f:(function
    | None -> printf "Not a power (zero element)\n"
    | Some(index, value) -> printf "%i = %i ^ %i\n" value primitive_element index);;
Not a power (zero element)
1 = 3 ^ 5
2 = 3 ^ 1
3 = 3 ^ 0
4 = 3 ^ 3
5 = 3 ^ 4
6 = 3 ^ 2
- : unit = ()
```

This way of expressing field elements will allow us to optimise the multiplication and division
operations:

$$a * b = pe^i * pe^j = pe^(i+j)$$

## Extension fields

So far we have seen how to build Galois fields from a prime number.  We can we also build them
from so called extension fields.

```ocaml
module GF3 = Galois.Primitive_field(struct let n = 3 end);;
module Poly = Poly.Make(GF3);;
```

```ocaml
# module GF3_2 = Galois.Extension_field(struct 
        module Poly = Poly 
        (* XXX whats going on here?  mdx isn't finding the cmis for base/core properly. *)
        let pp = Poly.to_poly (Base.Array.init 3 ~f:(function 0|1 -> 2 | _ -> 1))
    end)
module GF3_2 :
  sig
    type t = int array
    val sexp_of_t : t -> Sexp.t
    val compare : t Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    val zero : t
    val one : t
    val ( +: ) : t -> t -> t
    val ( -: ) : t -> t -> t
    val ( *: ) : t -> t -> t
    val ( /: ) : t -> t -> t
    val to_string : t -> string
  end
```

Here we have defined an extension field of degree 2 over the primitive field modulo 3 
containing $3^2 = 9$ elements.

The elements of this field are polynomials with coefficients modulo 3, where the polynomial itself
is modulo the so called irreducible polynomial ($x^2 + 2x + 2$ and called `pp` above).

The polynomial $x$ is a primitive element of this field and we can use it to generate all non-zero
elements.

```ocaml
# List.fold ~init:Poly.x (List.init 9 ~f:(Fn.const Poly.x)) ~f:(fun a b -> 
    printf "%s\n" (Poly.to_string a);
    GF3_2.(a *: b))
x
x + 1
2.x + 1
2
2.x
2.x + 2
x + 2
1
x
- : Poly.t = [|1; 1|]
```

## Practical extension fields

All the extension fields we will consider will have the form $GF(2^n)$.  This 
is very conventient bacause:

1. The base field of order 2 can be represented with a single bit (0 or 1).
2. Operations on the base field just become simple xor/and.
3. Polymonials over the field can be represented as simple integer valeus.

For example, $GF(2^8)$ (also writtem as $GF(256)$ ) has field elements which can 
be represented by a single 8 bit char type.

The galois module offers an optimised implementation of such fields.

```ocaml
# module GF256 = Galois.Int_table_of_params(struct 
    (* The primitive polynomial of the field *)
    let pp = 285
    (* The primitive element of the field *)
    let pe = 2 
  end)
module GF256 :
  sig
    type t = int
    val sexp_of_t : t -> Sexp.t
    val compare : t Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    val zero : t
    val one : t
    val ( +: ) : t -> t -> t
    val ( -: ) : t -> t -> t
    val to_string : t -> string
    val alpha : t
    val n_elems : t
    val log : t -> t
    val antilog : t -> t
    val ( *: ) : t -> t -> t
    val ( /: ) : t -> t -> t
    val ( **: ) : t -> t -> t
    val inv : t -> t
  end
```
