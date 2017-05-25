<?php


	 function func_sans_args  (  ){
   return (5+2);
 }




	 function func_avec_un_arg  (  $x ){
   return ($x == 2);
 }




	 function func_avec_deux_args  (  $x, $y ){
   return ($x*$y);
 }




	 function average  (  $a, $b ){
   return (($a + $b) / 2.0);
 }




	 function square  (  $x ){
   return ($x*$x);
 }




	 function valeur_absolue  (  $x ){
  	if ( ($x>=0) ) {
  return $x;
 }	else{
  return  - ($x);
 }


 }



	 function max  (  $a, $b ){
  	if ( ($a>$b) ) {
  return $a;
 }	else{
  return $b;
 }


 }



	 function fact  (  $x ){
  	if ( ($x<=1) ) {
  return 1;
 }	else{
  return ($x*	fact($x-1));
 }


 }



	 function range  (  $a, $b ){
  	if ( ($a>$b) ) {
  return ;
 }	else{
  return  array_unshift( range(($a+1),$b) , $a ) ;
 }


 }



	 function fib  (  $x ){
  	if ( ($x<=1) ) {
  return 1;
 }	else{
  return (	fib($x-1)+	fib($x-2));
 }


 }



	 function func_rec_sans_args  (  ){
  	if ( (1<5) ) {
  return 1;
 }	else{
 	func_rec_sans_args()
 }



	 function func_rec_avec_un_arg  (  $a ){
  	if ( ($a<10) ) {
 	func_rec_avec_un_arg($a-1));
 }
	else{
 	func_rec_avec_un_arg($a-2));
 }


 }



	 function func_rec_avec_deux_args  (  $a, $b ){
  	if ( ($a<$b) ) {
 func_rec_avec_deux_args($a,($b-2)));
 }
	else{
 func_rec_avec_deux_args(($a-2),$b)
 }



	 function func_rec_avec_trois_args  (  $a, $b, $c ){
  	if ( ($a<$c) ) {
  return $a;
 }	else{
 func_rec_avec_trois_args($a,($b-1),($c*10))
 }



?>
