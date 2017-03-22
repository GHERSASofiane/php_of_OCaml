<?php


 $tup_1_elem = 88;

 $tup_2_elem = array ( 'a' , "ocaml" ) ;

 $tup_3_elem = array ( 'a' , "ocaml" , 20 ) ;

 $tup_4_elem = array ( 88 , 5.20 , 'a' , "ocaml" ) ;

 $tab_vide = array ( );

 $tab_int = array ( 4 , 6 , 87);

 $tab_float = array ( 4.4 , 6.5 , 8.7 , 0.5);

 $tab_char = array ( 'a' , 'b' , 'c' , 'd');

 $tab_string = array ( "GHERSA" , "Sofiane" , "KASDI" , "Hacene");

 $notre_int = 5;

 $notre_char = 'C';

 $notre_string = "notre string en OCAML qui traduit en PHP";

 $notre_float = 10.5;

 if( 5 < 3 ) {
 	  return 2 ;
 }else{
 	  return 4 ;
 }

 if(  $bool  ) {
 	  return 1 ;
 }else{
 	  return 0 ;
 }

 function func_sans_args (  ){
  	   return 5 + 2 ;
 }


 function func_avec_un_arg (  $x ){
  	   return  $x  == 2 ;
 }


 function func_avec_un_arg (  $x ){
  	   return  $x  < 2 ;
 }


 function func_avec_un_arg (  $x ){
  	   return  $x  > 2 ;
 }


 function func_avec_un_arg (  $x ){
  	   return  $x  <= 2 ;
 }


 function func_avec_un_arg (  $x ){
  	   return  $x  >= 2 ;
 }


 function func_avec_deux_args (  $x, $y ){
  	   return  $x  *  $y  ;
 }


 function average (  $a, $b ){
  	   return  $a  +  $b  / 2.0 ;
 }


 function square (  $x ){
  	   return  $x  *  $x  ;
 }


 function valeur_absolue (  $x ){
  
 if(  $x  >= 0 ) {
 	  return  $x  ;
 }else{
 	  return  -  (  $x  )  ;
 }

 }


 function max (  $a, $b ){
  
 if(  $a  >  $b  ) {
 	  return  $a  ;
 }else{
 	  return  $b  ;
 }

 }


 if( 3 > 4 ) {
 	  return 1 + 2 - 4 ;
 }else{
 	  return 0 ;
 }

 function fact (  $x ){
  
 if(  $x  <= 1 ) {
 	  return  $x  ;
 }else{
 	  return  $x  *  fact  (  $x  - 1 )  ;
 }

 }


 function range (  $a, $b ){
  
 if(  $a  >  $b  ) {
 	   $list 
 }else{
 	   $list 
 }

 }


 function fib (  $x ){
  
 if(  $x  <= 1 ) {
 	  return 1 ;
 }else{
 	  return  fib  (  $x  - 1 )  +  fib  (  $x  - 2 )  ;
 }

 }


?>
