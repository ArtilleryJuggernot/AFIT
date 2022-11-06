(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
  let lg=String.length str in
  let rec encodb str lg i a res =
  if  lg =0 then res
  else let scal = power [0;0;1] (from_int bits)
       in encodb str (lg-1)(i-1)(mult_b scal a)
       (add_b res (mult_b a(from_int(Char.code str.[i]))))
  in encodb str lg (lg-1) [0;1] [];;



(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
*)
(* I had to find a new way to do decode because my builtin function too complicated to transfer into scalable*)
let decode msg bits =
  
  let rec decode_rec msg bits=
    let i=mod_b msg(power [0;0;1] bits)in
    if compare_b [] msg = 0 then ""else
      decode_rec (quot_b msg(power [0;0;1]bits))bits
      ^(Char.escaped (char_of_int (to_int i)))
      in decode_rec msg (from_int bits)