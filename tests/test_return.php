<?php

	if ( (5<3) ) {
  return 2;
 }	else{
  return 4;
 }

	if ( true  ) {
  return 1;
 }	else{
  return 0;
 }

	if ( true  ) {
  return (1+5);
 }

	else{
  return (0+8);
 }

	if ( true  ) {
  return array ( 10 , 20 , 30);
 }	else{
  return array ( 100 , 200 , 300);
 }

	 $x =(array ( 1 , 2 , 3)); 
	if ( true  ) {
  ( $x = array ( 10 , 20 , 30) );
 
 }
	else{
  ( $x = array ( 100 , 200 , 300) );
 
 }
	if ( false  ) {
  return array ( "GHERSA" , "KASDI" , "hacene" , "sofiane" ) ;
 }	else{
  return array ( "hacene" , "sofiane" , "GHERSA" , "KASDI" ) ;
 }

	 $x =""; 
	if ( true  ) {
  ( $x = 8 );
 
 }
	else{
  ( $x = 9 );
 
 }
	 $x =""; 
	if ( true  ) {
  ( $x = 8 );
 
 }
	else{
  ( $x = 9 );
 
 }
	if ( false  ) {
 echo   ( " la condition est true"." \n " ); 

 }
	else{
 echo   ( " la condition est false"." \n " ); 

 }


	 function add  (  $x, $y ){
   return ($x+$y);
 }




	 function affiche  (  $x ){
  echo   ( " l'argument que vous avez entre est : " ); 
echo   ( $x." \n " ); 

 }


	$rr = add(4,5);
;

	 function add  (  $x, $y ){
  	if ( true  ) {
  ( $x =  max(5 , 6 ) );
 
 }
	else{
  ( $y =  min(7 , 9 ) );
 
 }

 }



?>
