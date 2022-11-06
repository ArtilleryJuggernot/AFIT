(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n = let rec pow_rec x n =
                match n with
                  |[] -> [0;1]
                  |_ -> mult_b x (pow_rec x (diff_b n [0;1]))
                                  in pow_rec x n;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  let rec pwr x n=
  if compare_b n []=0 then [0;1]
  else if (compare_b n [0;1]=0) then x
  else if compare_b (mod_b n [0;0;1])([])=0 then
    pwr (mult_b x x) (quot_b n [0;0;1])
  else mult_b x (pwr(mult_b x x)(quot_b(diff_b n [0;1])[0;0;1]))
  in pwr x n;;
to_int (power (from_int 5) (from_int 2));;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m = let rec mod_power_rec x n m =
                        match n with
                          |[] -> mod_b (from_int 1) m
                          |_ -> if n = from_int 1
                            then mod_b x m
                            else
                              if (mod_b n (from_int 2)) = []
                              then mod_b (mult_b((mod_power_rec x (quot_b n (from_int 2)) m)) (mod_power_rec x (quot_b n (from_int 2)) m)) m
                              else mod_b (mult_b(mod_power_rec x (diff_b n (from_int 1)) m) x) m
                      in mod_power_rec x n m;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p= if compare_b x [] =0 then []
  else mod_power x (mod_b n(diff_b p [0;1])) p;;
 
