(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)

(*Fast Exp pour scalable*)
let rec power x n =
  if n = 0 then 1
  else if n=1 then x
  else if n mod 2 = 0 then power (x*x)  (n/ 2)
  else x * power (x*x) ((n-1)/ 2);;


let from_int x = 
    let rec bitarrays x = let a = abs(x) in
                        match x with
                        |0 -> []
                        |_ -> (a mod 2)::bitarrays (a/2)
                            in if x > 0 then 0::bitarrays x else if x < 0 then 1::bitarrays x else [] ;;



let to_int bA = let i= match bA with
  |[]->1
  |e::l->if e =0 then 1 else -1
in let rec to_int_rec bA i2= match bA with
          |[]-> 0
          |e::l->(to_int_rec l (i2+1))+(power 2 i2)*e
   in let i3= i*(to_int_rec bA 0)in i3/2;;
to_int [0;0;1];;



(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA = let rec print_b_rec bA i =
                   match bA with
                   |[] -> ""
                   |a::bA when i = 0 -> if a = 1 then "-"^print_b_rec bA (i+1) else print_b_rec bA (i+1)
                   |a::bA -> print_b_rec bA (i+1)^string_of_int(a)
            in print_string(print_b_rec bA 0);;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let compare_n nA nB = if List.length nA < List.length nB then (-1) else
                        if List.length nA > List.length nB then 1 else
  let nA = List.rev(nA) and nB = List.rev(nB) in
  let rec compare_n_rec nA nB =
    match (nA,nB) with
      |(e1::nA,e2::nB) -> if e1 = e2
        then compare_n_rec nA nB
        else
          if e1 > e2
          then 1
          else (-1)
      |_ -> 0
  in compare_n_rec nA nB ;;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = if compare_n nA nB = 1 then true else false;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = if compare_n nA nB = (-1) then true else false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = if compare_n nA nB = -1 then false else true;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = if compare_n nA nB = 1 then false else true;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB = match (bA,bB) with
  |([],e2::bB) -> if e2 = 1 then 1 else (-1)
  |(e1::bA,[]) -> if e1 = 1 then (-1) else 1
  |(e1::bA,e2::bB) when e1 != e2 -> if e1 < e2 then 1 else (-1)
  |(e1::bA,e2::bB) when e1 = 1 && e2 = 1 -> if compare_n bA bB = 1
    then (-1)
    else
      if compare_n bA bB= (-1)
      then 1
      else 0
  |(e1::bA,e2::bB) when e1 = 0 && e2 = 0 -> compare_n bA bB
  |_ -> 0;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = if compare_b bA bB = 1 then true else false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = if compare_b bA bB = (-1) then true else false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = if compare_b bA bB = (-1) then false else true;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = if compare_b bA bB = 1 then false else true;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
  |e::bA -> if e = 0 then 1 else (-1)
  |_ -> 0;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = match bA with
  |e::bA -> if e = 1 then 0::bA else e::bA
  |_ -> [];;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)

let add_n nA nB = let rec add_n_rec nA nB r =
                    match (nA,nB) with
                    |([],[]) -> if r = 1 then 1::[] else []
                    |(e1::nA,e2::nB) when e1 + e2 + r = 0 -> 0::add_n_rec nA nB 0
                    |(e1::nA,e2::nB) when e1 + e2 + r = 1 -> 1::add_n_rec nA nB 0
                    |(e1::nA,e2::nB) when e1 + e2 + r = 2 -> 0::add_n_rec nA nB 1
                    |(e1::nA,e2::nB) when e1 + e2 + r = 3 -> 1::add_n_rec nA nB 1
                    |(e1::nA,[]) -> add_n_rec (e1::nA) [0] r
                    |([],e2::nB) -> add_n_rec [0] (e2::nB) r
                    |_ -> []
                  in add_n_rec nA nB 0;;


let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB = 
    let rec diff_n_rec nA nB r =
                    match (nA,nB) with
                      |([],[])                                   -> []
                      |(e1::[],e2::[])                           -> if e1 - (e2 + r) = 1 then e1::[] else []
                      |(e1::nA,e2::nB) when e1 - (e2 + r) = 0    -> 0::diff_n_rec nA nB 0
                      |(e1::nA,e2::nB) when e1 - (e2 + r) = 1    -> 1::diff_n_rec nA nB 0
                      |(e1::nA,e2::nB) when e1 - (e2 + r) = (-1) -> 1::diff_n_rec nA nB 1
                      |(e1::nA,e2::nB) when e1 - (e2 + r) = (-2) -> 0::diff_n_rec nA nB 1
                      |(e1::nA,[])                               -> diff_n_rec (e1::nA) [0] r
                      |_                                         -> []
                   in diff_n_rec nA nB 0;;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_n nA nB =
  let rec addimalou nA nB acc= match (nA,nB) with
    |([],[])->if acc=1 then [1] else [] 
    |([],e::l)->if e+acc>1 then 0::(addimalou [] l 1) else (acc+e)::l
    |(e::l,[])->if e+acc>1 then 0::(addimalou l [] 1) else (acc+e)::l
    |(e::l,e1::l1) when e+e1+acc<=1 ->e+e1+acc::(addimalou l l1 0) 
    |(e::l,e1::l1) when e+e1+acc>1 -> if e1+e+acc=2 then 0::(addimalou l l1 1)
      else 1::(addimalou l l1 1)
    |(_,_)->[]
  in addimalou nA nB 0;;

to_int (add_n (from_int (200))(from_int 300));;
from_int 532;;
from_int 500;;



(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
(*Fonction Reverse pour liste*)
let reverse l =
  let rec rev l1 l2 = match l1 with
    | []   -> l2
    | e::l -> rev l (e::l2) 
  in rev l []  ;;

(*Fonction clear *)
let cls list= let rec clear l=match l with
   []->[]
  |e::l->if e=0 then clear l else e::l
        in reverse (clear(reverse list));;





let add_b bA bB = match (bA,bB) with
    |(bA,[])->bA
    |([],bB)->bB
    |(e::l,e1::l1) when sign_b bA=(-1) && sign_b bB =(-1) -> 1::add_n l l1
    |(e::l,e1::l1) when sign_b bA=  1  && sign_b bB =  1  -> 0::add_n l l1
    |(e::l,e1::l1) when sign_b bA=(-1) && sign_b bB=1 && compare_n l l1 = 1 ->  1::diff_n l l1
    |(e::l,e1::l1) when sign_b bA=(-1) && sign_b bB=1 && compare_n l l1 =(-1)->0::diff_n l1 l
    |(e::l,e1::l1) when compare_n l l1=1 ->0::diff_n l l1
    |(e::l,e1::l1) when compare_n l l1=(-1)->1::diff_n  l1 l
    |(e::l,e1::l1)->[];;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)

let diff_b bA bB = match (bA,bB) with
  |([],[])->[]
  |(e::l,[])->cls(e::l)
  |([],e::l)->[]
  |(e::l,e1::l1)->if e=0 && e1=0 then if l>=!l1 then cls(0::diff_n l l1)
    else cls (1::diff_n l1 l)
    else if e=0 && e1=1 then cls(0::add_n l l1)
    else if e=1 && e1=0 then cls(1::add_n l l1) 
    else if l>=!l1 then cls(1::diff_n l l1)
    else cls(0::diff_n l1 l);;


let rec shift bA d = 
    if d = 0
  then bA
  else
    if bA = []
    then []
    else let rec shift_rec bA d i =
           match (d,bA) with
             |(d,a::bA) when i = 0 -> a::(shift_rec bA d 1)
             |(1,bA) -> 0::bA
             |(d,bA) -> 0::(shift_rec bA (d - 1) 1)
         in shift_rec bA d 0;;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_n nA nB = let rec mult_n_rec nA nB =
                     match (nA,nB) with
                       |(nA,[]) -> []
                       |(nA,e::nB) when e = 1 -> let nC = 0::nA in (add_n nA (mult_n_rec nC nB))
                       |(nA,e::nB) when e = 0 -> let nC = 0::nA in mult_n_rec nC nB
                       |_ -> []
                   in mult_n_rec nA nB;;


let mult_b bA bB = let rec mult_b_rec bA bB =
                     match (bA,bB) with
                       |(e1::bA,e2::bB) when e1 = e2 -> 0::(mult_n bA bB)
                       |(e1::bA,e2::bB) -> 1::(mult_n bA bB)
                       |_ -> []
                   in mult_b_rec bA bB;;
(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
(*Quotient dans N*)
let quot_n nA nB = let rec quot_n_rec nA nB i =
                     match (nA,nB) with
                       |(nA,nB) when compare_n nA nB = (-1) -> i
                       |_ -> quot_n_rec (diff_n nA nB) nB (add_n i [1])
                   in quot_n_rec nA nB [];;



let quot_b bA bB = 
    let rec quot_b_rec bA bB =
    match (bA,bB) with
    |(e1::bA,e2::bB) when e1 = e2 -> if compare_n bA bB = (-1) then [] else 0::(quot_n (bA) (bB))
    |(e1::bA,e2::bB)-> if mult_n (quot_n bA bB) bB = bA then 1::(quot_n (bA) (bB)) else 1::(add_n (quot_n bA bB) [1])
    |_-> []
in quot_b_rec bA bB;;
(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB =
  if (mult_b bB (quot_b bA bB)) =bA then []  
else (diff_b bA (mult_b (quot_b bA bB)bB));;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = (quot_b bA bB, mod_b bA bB);;
