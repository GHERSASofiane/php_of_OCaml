
 let x_0 = int_of_float 13.6;; (* oui *)

let x_1 = float_of_int 13;; (* oui *)

let x_2 = int_of_char 'A';; (* oui *)

let x_3 = char_of_int 97;; (* oui *)

let x_4 = string_of_bool true;;(* oui *)

let x_5 = string_of_bool false;; (* oui *) 

let x_6 = bool_of_string "true" ;; (* "false" ????? *)

let x_7 =float_of_string "13.37";;(* oui *) 

let x_8 = string_of_float 13.37;;(* oui *) 


let x = 5 ;;
let x = 10;;
let y = 5 + x;;
print_int y ;;