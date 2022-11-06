(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power


let generate_keys_rsa p q =
  let n = mult_b p q in
     let phi_de_n = mult_b (diff_b p [0;1])(diff_b q [0;1]) in
       let e =(from_int 65537)  in
        let (d,l,m) = bezout_b e phi_de_n in 
            ((n,e),(n,d));;



let encrypt_rsa m (n, e) = mod_power m e n;;


let decrypt_rsa m (n , d) = mod_power m d n;;
 
let  public_data_g p = let g = from_int(Random.int(to_int(p))) in let rec public_data_g_rec p =
                                                            match g with
                                                              |g when mod_power g [0;0;1] p != [0;1] -> (g,p)
                                                              |_ -> public_data_g_rec  p
                                                          in public_data_g_rec  p;;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = let rec generate_keys_g_rec (g, p) =
                               let a = from_int(Random.int(to_int(p)))
                               in match a with
                                  |a when mod_power a [0;0;1] p != [0;1] -> (mod_power g a p, a)
                                  |_ -> generate_keys_g_rec (g, p)
                             in generate_keys_g_rec (g, p);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =  let k = add_b [0;1] (from_int(max_int / 2))
                                    in (mod_power g k p, mod_b (mult_b msg (mod_power kA k p)) p);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let k = mod_power msgA a p in mod_b (mult_b msgB
                   (mod_power k (diff_b p [0;0;1]) p) ) p;;
