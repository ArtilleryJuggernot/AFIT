(** Encoding Strings *)

open Z
open Z_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let lg = String.length str in
  let rec encodb str lg i a res =
    if lg = zero then res
    else
      let scal = pow (one+one) bits in
      let o = to_int(i) in 
      let chr = of_int(Char.code str.[o]) in 
      encodb str (lg-one) (i-one) (scal * a) (chr*a+res) in 
      let lg = of_int(lg) in 
    encodb str lg (lg-one) one zero;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
    let rec div n x i = 
    
            if  n mod x = zero then i
            else div (n - one) x (i + one)
        in
        let rec decode_rec bits msg =
        let i = div msg (pow (one+one) bits) zero in
        let msg_rec = (msg - i) / (pow (one+one) bits) in
        let o = to_int(i) in 
        if msg = zero then ""
        else
          ( decode_rec bits msg_rec) ^ (Char.escaped (char_of_int o))
        in decode_rec bits msg;;
