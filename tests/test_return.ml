
if 5<3 then 2 else 4;;

if true then 1 else 0;;

if true then 1+5 else 0+8 ;;

if true then [| 10; 20; 30 |] else [| 100; 200; 300 |] ;;

let x = ref  [| 1; 2; 3 |] in  if true then x := [| 10; 20; 30 |] else x := [| 100; 200; 300 |] ;;

if false then ("GHERSA","KASDI","hacene","sofiane") else  ("hacene","sofiane","GHERSA","KASDI") ;;

let x = ref 0 in  if true then x := 8 else  x := 9 ;;

let x = ref 0 in  if true then x := 8 else  x := 9 ;;

if false then print_endline " la condition est true" else  print_endline " la condition est false" ;;

let add x y =   x +y  ;;

let affiche x =  print_string " l'argument que vous avez entre est : ";  print_endline x ;;

let rr = add 4 5;;

let add x y = if true then  x :=  max 5 6 else  y :=   min 7 9 ;;

