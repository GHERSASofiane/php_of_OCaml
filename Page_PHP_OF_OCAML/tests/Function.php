<?php


 function func_sans_args(){
  	   return 5 + 2 ;
 }


 function func_avec_un_arg (  $x ){
  	   return  $x  == 2 ;
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


 function fact (  $x ){
  
 if(  $x  <= 1 ) {
 	  return 1 ;
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
