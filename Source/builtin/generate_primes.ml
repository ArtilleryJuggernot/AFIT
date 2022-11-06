(* Generating primes *)

open Builtin
open Basic_arithmetics


let init_eratosthenes n =
  let n = abs(n)
  in let rec erat n i =
       match n with
       |n when i > n -> []
       |_ -> i::erat n (i+2)
           in 2::erat n 3;;


let rec remove_nth n l1 l2 = match l1 with
          [] -> l2
      | e1::l when (modulo e1 n) = 0 && e1 <> n -> remove_nth n l l2
      | e1::l -> e1::remove_nth n l l2;;


let eratosthenes n = if n < 0 then invalid_arg "impossible n < 0" else
      let rec era n i l = 
          if i*i > n then l
        else era n (i+1) (remove_nth i l [])
      in era n 3 (init_eratosthenes n);;

let write_list li file = let oc = open_out file in
  let rec aux = function
      [] -> close_out oc
    | e::s1 -> Printf.fprintf oc "%d\n" e; aux s1
  in aux li;;


let write_list_primes n file = write_list (eratosthenes n) file;;



let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None


let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in _create_list in_c
let read_list_primes file = create_list (open_in file);;


let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t


let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t


let length l =
  let rec len l c = match l with
    |[] -> c
    |e::l -> len l (c+1)
  in len l 0;;

let double_primes limit isprime = 
  let rec double_primes_rec i =match i with
  |i when i > limit -> []
  |_ -> if (isprime i) && (isprime ((i * 2) + 1))
  then (i, i * 2 + 1)::double_primes_rec (i + 1)
  else double_primes_rec (i + 1)
in double_primes_rec 2;;

let twin_primes limit isprime =
  let pl = eratosthenes limit in (*Listes des premiers*)
  let rec test pl =
    match pl with 
      | [] -> []
      | e :: l -> if e = 2 then test l
  else if  isprime (e+2) then (e,(e+2)):: test l
     else  test l
  in test pl;;