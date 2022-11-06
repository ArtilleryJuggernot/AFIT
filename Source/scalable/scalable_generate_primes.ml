(** Generating prime bitarrays *)
open Scalable
open Scalable_basic_arithmetics


let init_eratosthenes n =

  let rec initrec e=
      if  e>>=(add_b n [0;1]) then []
      else if compare_b e [0;0;1]=0 then  [0;0;1]::initrec (add_b e [0;1])
      else if compare_b (mod_b e [0;0;1])[] <>0
      then e::initrec (add_b e [0;1])
      else initrec (add_b e [0;1])
  in initrec [0;0;1];;  
  init_eratosthenes (from_int 3)
(* Eratosthenes sieve. *)



let rec remove_nth n l1 l2=
  match l1 with
    []->l2
    |e1::l when compare_b (mod_b e1 n)[]=0 &&compare_b e1 n <>0
       -> remove_nth n l l2
    |e1::l-> e1::remove_nth n l l2;;
let tail l= match l with
  |[]->[]
  |e::l->l;;
let reverse list =
  let rec rev accu = function
      []   -> accu
    | e::l -> rev (e::accu) l
  in
  rev [] list ;;

let eratosthenes n =
  if sign_b n =(-1) then invalid_arg "bruh"else
    let rec era ni i l= if (mult_b i i)>>(diff_b ni[0;1]) then
    if n >>[0;0;1;0;1] then
      reverse (tail(reverse l))
    else l
  else  era ni (add_b ni [0;1]) (remove_nth i l[])
in era (diff_b n [0;1]) [0;1;1] (init_eratosthenes n);;

eratosthenes (from_int 25);;




let write_list li file =() ;;


let write_list_primes n file = write_list (eratosthenes n) file;;


let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None


let rec create_list in_c =();;
              


let read_list_primes file =[];;




let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t;;


let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;


let double_primes limit isprime =
  let rec double_rec l= match l with
      []->[]
    |e1::s1 when isprime (add_b(mult_b e1 [0;0;1])[0;1])&&isprime e1->
    (e1,(add_b(mult_b e1 [0;0;1])[0;1]))::double_rec s1
    |e1::s1-> double_rec s1
  in double_rec (eratosthenes limit);;



let twin_primes limit isprime =
  let primes= eratosthenes limit in
  let rec test primes =
    match primes with
      |[]->[]
      |e::l-> if compare_b e [0;0;1]=0 then test l
else if isprime (add_b e [0;0;1])
then (e,add_b e [0;0;1])::test l
else test l
  in test primes;;