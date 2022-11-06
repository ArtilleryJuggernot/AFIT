(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n = 
  let rec pow_rec x n = match n with
  |0 -> 1
  |_ -> x * pow_rec x (n - 1)
in pow_rec x n;;


(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 **)
let power x n = let rec power_rec x n = if n = 0 then
    1
  else if n = 1 then
    x
  else if n mod 2 = 0 then
    power_rec (x * x) (n / 2)
  else
    x * power_rec (x * x) (n / 2)
                in power_rec x n;;
(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 **)
let mod_power x n m =
  let rec rec_mod xrec nrec =
      if n <= nrec  then xrec
       else rec_mod (modulo (xrec*x) m) (nrec+1)
  in rec_mod 1 0 ;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 **)
let prime_mod_power x n p = if x= 0 then 0
  else mod_power x (modulo n (p-1)) p  ;;
  (*P-1 est permis avec p -> Fermat*)
