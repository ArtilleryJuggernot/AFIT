(** Ciphers
    Big integers based ciphers.
*)

open Z
open Z_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
  let generate_keys_rsa p q =
  let p = p + zero in
  let n = p * q in
     let phi_de_n = ((p-one) * (q-one)) in
       let e = of_int(65537)  in
        let (d,l,m) = gcdext e phi_de_n in 
            ((n,e),(n,d));;

(** Encryption using RSA cryptosystem.
    @param m big integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n,e)= powm  m e n ;;

(** Decryption using RSA cryptosystem.
    @param m big integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = powm m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p = 
    let a = to_int(p) in 
    let g =  of_int(Random.int a) 
in (g,p);;

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = 
    let z = to_int(p-one) (*int*) in 
    let a = of_int(Random.int (z)) + one
in (powm g a p, a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) y =
    let z = to_int(p-one) (*int*) in 
    let a = of_int(Random.int (z)) + one in 
  (powm g a p, (msg*(powm y a p)) mod p)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let k = powm msgA a p in
   (msgB * mod_power k (p- (one+one)) p) mod p;;