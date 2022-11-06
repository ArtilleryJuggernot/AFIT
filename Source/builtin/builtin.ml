
(*PROJET AFIT : builtin.ml*)

let sign x = match x with
  | x when x<0 -> -1
  | x -> 1


let quot a b = if b = 0 then failwith "b = 0 error"
               else if a mod b<0 then a/b -1
               else a/b

(*wthout mod*)

(** Quotient of two integers. Fully Recursive.
    General case ; explicit by-hand computations. Not efficient enough as
    it is not a low level implementation.
*)

(** Modulo of two integers.
    Following euclidean division NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.

    OCAML DEFAULT : For negative numbers eucldean result - modulo base.

    @param a input integer
    @param b moduli integer.
 *)
let modulo a b = 
    if sign b = -1 then failwith "b doit etre naturel"
  
  else let r = a mod b in if sign r = -1 then r + b else r;;

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in [0, abs b[.
    @param a dividend
    @param b integer you divide by.
*)
let div a b = if sign b = -1 then failwith "b doit etre naturel"
  else (quot a b,modulo a b);;


