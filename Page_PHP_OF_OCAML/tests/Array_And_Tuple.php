<?php


 $tab_vide = array ( );

 $tab_int = array ( 4 , 6 , 87);

 $tab_float = array ( 4.4 , 6.5 , 8.7 , 0.5);

 $tab_char = array ( 'a' , 'b' , 'c' , 'd');

 $tab_string = array ( "GHERSA" , "Sofiane" , "KASDI" , "Hacene");

 $tup_1_elem = 88;

 $tup_2_elem = array ( 'a' , "ocaml" ) ;

 $tup_3_elem = array ( 'a' , "ocaml" , 20 ) ;

 $tup_4_elem = array ( 88 , 5.20 , 'a' , "ocaml" ) ;
 
 		print_r($tab_vide);echo '<br><br>'; 
 		print_r($tab_int);echo '<br><br>'; 
 		print_r($tab_float);echo '<br><br>'; 
 		print_r( $tab_char);echo '<br><br>'; 
 		print_r( $tab_string);echo '<br><br>';
		echo $tup_1_elem.'<br><br>'; 
 		print_r( $tup_2_elem);echo '<br><br>'; 
 		print_r($tup_3_elem);echo '<br><br>'; 
 		print_r($tup_4_elem);echo '<br><br>'; 
?>
