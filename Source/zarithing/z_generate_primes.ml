(** Generating primes *)

open Z

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
   A light version done in-class.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n upper limit of elements in the list of big integers.
 *)
let init_eratosthenes n =
  let n = abs(n)
  in let rec erat n i =
       match n with
       |n when i > n -> []
       |_            -> i::erat n (i+ (one+one))
           in (one+one)::erat n (one+one+one);;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let rec remove_nth n l1 l2 = match l1 with
          [] -> l2
      | e1::l when (e1 mod n) = zero && e1 <> n -> remove_nth n l l2
      | e1::l -> e1::remove_nth n l l2;;


let eratosthenes n = if n < zero then invalid_arg "b < 0" else
      let rec era n i l = 
          if i*i > n then l
        else era n (i+one) (remove_nth i l [])
      in era n (one+one+one) (init_eratosthenes n);;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file = ()
(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)


let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in _create_list in_c


(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(* Generating couples of primes numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive big integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime = let rec double_primes_rec i =
                                    match i with
                                    |i when i > limit -> []
                                    |_ -> if (isprime i) && (isprime ((i * (one+one)) + one))
                                          then (i, i * (one+one) + one)::double_primes_rec (one + one)
                                          else double_primes_rec (i + one)
in double_primes_rec (one+one);;