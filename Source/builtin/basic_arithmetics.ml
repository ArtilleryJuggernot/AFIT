(** Basic arithmetics with builtin integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let gcd a b = 
    let rec gcd_rec a b = 
    let r = a mod b in
    if r = 0 then b
    
    else 
    if gcd_rec b r < 0 then -1 * gcd_rec b r
  
    else gcd_rec b r

    in gcd_rec a b;;
;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b = let rec bezout2 (r,u,v, rp,up,vp)=
    if rp=0 then (u,v,r)
     else    bezout2 (rp,up,vp, (r-((quot r rp)*rp)) , (u-((quot r rp)*up)), (v-((quot r rp)*vp)))
      in  bezout2 (a, 1, 0, b, 0, 1) ;;

