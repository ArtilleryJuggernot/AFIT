(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = let (n,p) = key in
        let rec broken e =
          if e>>n then ([],[])
          else if compare_b (mod_b n e)[] =  0 then (e,quot_b n e)
          else broken (add_b e [0;1])
        in broken [0;0;1];;
