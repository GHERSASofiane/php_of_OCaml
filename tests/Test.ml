
let tup_1_elem = ( 88)
let tup_2_elem = ( 'a', "ocaml")
let tup_3_elem = ( 'a', "ocaml", 20)
let tup_4_elem = ( 88, 5.20, 'a', "ocaml")

(************************************************)

let tab_vide = [||]
let tab_int = [|4;6;87|]
let tab_float = [|4.4;6.5;8.7;0.5|]
let tab_char = [|'a';'b';'c';'d'|]
let tab_string = [|"GHERSA";"Sofiane";"KASDI";"Hacene"|]



(************************************************)

let notre_int = 5 ;;(* entier *)
let notre_char = 'C' ;;(* caractère *)
let notre_string = "notre string en OCAML qui traduit en PHP" ;;(* chaîne de caractère *)
let notre_float = 10.5 ;;(* réel *)

(************************************************)
if 5<3 then 2 else 4;;

if true then 1 else 0;;

(************************************************)

let func_sans_args () = 5+2
let func_avec_un_arg x = x = 2
let func_avec_deux_args x y = x * y
let average a b =  (a +. b) /. 2.0
let square x = x * x
let valeur_absolue x = if x >= 0 then x else - x
let max a b =
  if a > b then a else b
  
let rec fact x =
  if x <= 1 then 1 else x * fact (x - 1)
let rec range a b =
    if a > b then []
    else a :: range (a+1) b
let rec fib x = if x <= 1 then 1 else fib (x - 1) + fib (x - 2) 

(************************************************)



