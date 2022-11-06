(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = 
let (n,p) = key in
		let rec broken e = match e with
		|e when e>n -> (0,0)
		|e when n mod e = 0 -> (e,n/e)
		|_ -> broken (e+1)

in broken 2;
