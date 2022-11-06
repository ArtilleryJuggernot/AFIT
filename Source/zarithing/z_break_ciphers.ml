(** Factoring big integers into primes *)

open Z

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = 
let (n,p) = key in
        let rec broken e = match e with
        |e when e>n -> (zero,zero)
        |e when n mod e = zero -> (e,(n / e))
        |_ -> broken (e+one)

in broken (one+one);

