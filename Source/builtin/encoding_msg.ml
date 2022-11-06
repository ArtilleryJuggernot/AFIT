(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let lg = String.length str in
  let rec encodb str lg i a res =
    if lg = 0 then res
    else
      let scal = power 2 bits in
      encodb str (lg-1) (i-1) (scal * a) (Char.code str.[i]*a+res)
        in encodb str lg (lg-1) 1 0;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits =
    let rec div n x i = 
            if  n mod x = 0 then i
            else div (n - 1) x (i + 1)
        in
        let rec decode_rec bits msg =
        let i = div msg (power 2 bits) 0 in
        let msg_rec = (msg - i) / (power 2 bits) in
        if msg = 0 then ""
        else
          ( decode_rec bits msg_rec) ^ (Char.escaped (char_of_int i))
        in decode_rec bits msg;;