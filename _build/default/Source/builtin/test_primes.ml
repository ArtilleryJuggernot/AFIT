(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)

let is_prime n =
  let rec is_prime2 x=
    if x=n then true
  else if modulo n x <> 0 then is_prime2(x+1)
  else false 
in is_prime2 2;;


let is_pseudo_prime p test_seq = 
  let rec pseudo_rec test_rec = match test_rec with
    |[]->true      
    |e::l-> if mod_power e (p-1) p = 1 || p<=2 then pseudo_rec l
      else false (*Fermat*)
  in  pseudo_rec test_seq ;;

