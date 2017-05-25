<?php

   $x = array (88,55,3 );
   
 	 if (sizeof($x) == 0) {
 	 	$hd= $x[0];
 	 	$x = array_splice($x,1);

 	echo   ( "Vide" ); 

 	 }else{
 	 	$hd= $x[0];
 	 	$x = array_splice($x,1);

 	 if (sizeof($x) >= 1) {
 	 	$hd= $x[0];
 	 	$x = array_splice($x,1);

 	echo   ( "Vide" ); 

 	 }else{
 	 	$hd= $x[0];
 	 	$x = array_splice($x,1);
	echo   ( "Autre" ); 

 	 	 } } 
 

?>
