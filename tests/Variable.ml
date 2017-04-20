


let x = let y = 5 in let z = 10 in let p = 20 in p+z + y ;;

let x = let y = 5 in let z = 10 in let p = 20 in let g = 5 in g+ p+z + y ;;


(* TAKES A LAST VARIABLE IF WE HAVE THE SAME IDENT NAME *)
let x = let y = 4+1 in let z = 10 in let p = 20 in let x = 10 in let p = 1 in x+ p+z + y ;; 

(* EQUIVALENT IN PHP
 $y =  (4 + 9 ) ;
 $z = 10 ;
 $p = 20 ;
 $x = 51 ;
 $x22 =  ( ( ( $x  +  $p  ) +  $z  ) +  $y  ) ; 
 echo $x; 
THE TEST RETURN 26 
WORNING UNUSED VARIABLE p in p=20
*)

let add x y = let z = 5 in let x = 10 in z + x + y;;

(* EQUIVALENT IN PHP
		function add (  $x, $y ){
		   $z = 5 ;
		 $x = 10 ;
		 return ( ( $z  +  $x  ) +  $y  ) ;
		 }
		 echo add (155,10);;
THE TEST RETURN 25
 *) 

(* CONCATENATION *)
let x = let y = "ocaml " in let z = "php" in y^z;;

(* IF THEN ELSE IN TEXP_APPLY *)
let x = let y = 4 in let z = 6 in let o = y + z in if (o > 0) 
then print_int o else print_int o;;

(* X UNUSED *)
let x = let y = 4 in let z = 6 in let o = y + z in print_int o ;;

(* CONVERT INT TO STRING IN TEXP_APPLY *)
let u = let x = 4 in let o = string_of_int x in let z = "we have " in z ^ o ;;
let addme x y = let z = x + y in z;;
let mult x y = let z = x + y in z*2;;
