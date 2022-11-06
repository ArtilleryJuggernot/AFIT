(** Testing for primality *)

open Z
open Z_power

(** Deterministic primality test
    @param n a big integer bigger or equal to 2.
 *)
let is_prime n =
  let rec is_prime2 x=
    if x=n then true
  else if n  mod x <> zero then is_prime2(x+one)
  else false 
in is_prime2 (one+one);;
