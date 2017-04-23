<?php

 $x = array (88 );
   
 	 if (sizeof($x ) == 0) {
 	echo   ( 7 ); 

 	 }else{

 	 if (sizeof($x ) == 1) {
 	echo   ( 88 ); 

 	 }else{

 	 if (sizeof($x ) == 2) {
 	echo   ( 19 ); 

 	 }else{

 	 if (sizeof($x ) >= 2) {
 	echo   ( 39 ); 

 	 }else{

 	 if (sizeof($x ) == 4) {
 	echo   ( 77 ); 

 	 }else{
	echo   ( 98 ); 

 	 	 } } } } } 
 

	 function is_a_vowel (  $c ){
  	 switch ($c ) { 
 
  
 	 case  'a' : 	true ;
 	 case  'R' : 	true ;
 	 default : false ;
 	 	 }
 
 }


$usr1 = 
 	[ 
	 	 'id' => 1,
 	 	 'nom' => "ghersa",
 	 	 'date_naissance' => "01/04/1993",
 	 	 'mail' => "m.ghersa.s@gmail.com",
 	 	 'telephone' => "000  000 000 000"
  	] ;

	 function generate_search (  $tab, $id, $usr_search ){
   
 	 if (sizeof($tab ) == 0) {
 	
 	 }else{

 	 if (sizeof($tab ) >= 1) {
 		if ( ($hd [4] == $id ) ) {
 
 ($usr_search )['nom']=$hd [3]; 

 ($usr_search )['date_naissance']=$hd [2]; 

 ($usr_search )['mail']=$hd [1]; 

 ($usr_search )['telephone']=$hd [0]; 

 }	else{
 generate_search($rst ,$id ,$usr_search );

 	 }else{

 	 	 }
 
 }



?>
