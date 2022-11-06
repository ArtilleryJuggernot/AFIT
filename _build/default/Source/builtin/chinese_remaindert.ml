(** Chinese remainder theorem *)

open Builtin
open Basic_arithmetics

(*#use "builtin.ml";;

#use "basic_arithmetics.ml";;
*)
(** Image of the Chinese Remainder map
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
_ *)

let crt_image x l =
  let k = []
  in let rec div x l k = match l with
      |[] -> k
      |e::l -> div x l ((x mod e)::k)
  in div x l k


let crt_solver m l y = 
    let rec process_y y = match y with
        []-> 1
        |e::s -> process_y s
    in
    let sous = process_y l in 
    let rec process_x m l y = match (l,y) with
        ([],[]) -> 0
        |([],_) -> 0
        |(_,[]) -> 0
        |(e::s,f::l1) -> (m)+process_x (quot m e) s l1
    in 
    modulo (process_x m l y) sous;;



