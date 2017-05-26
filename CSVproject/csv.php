<?php



	 function split  (  $chaine, $separateur ){
  	 $result =array(); 
	 $ch =""; 
	for ($i=0; $i <= (strlen($chaine)-1); $i++) 
 	 { 
 	 		if ( ( ($chaine[$i])  == $separateur) ) {
  array_unshift( ($result) , ($ch) ) ;
  ( $ch = "" );
 
 }	else{
  ( $ch = (($ch). ( ($chaine[$i]) ))  );
 
 }
	 }
 array_unshift( ($result) , ($ch) ) ;
 
 	 return ($result) ; 
 }



	 function lire_file  (  $file ){
  	 $entree =fopen($file , "r" ); 
	 $result =array(); 

 	 try{ 
 	 $ligne =split(fgets($entree),';'); 
	 $nb_champs =sizeof(($ligne)); 
	while (true  )
 	 {
 	 		if ( (sizeof(($ligne))!=$nb_champs) ) {
  throw new ParseError("End_of_file");
 	 	 }
	else{
 array_unshift(($result),($ligne)) ;
 }
 ( $ligne = split(fgets($entree),';') );
 
 	 }

 	 }
 	 	  catch(ParseError $e){ 
 	fclose($entree);
 	 	 } 

 	 return ($result) ; 
 }



	 function buffer_file  (  $file ){
  	 $entree =fopen($file , "r" ); 
	 $result =""; 

 	 try{ 
 	 $ligne =fgets($entree); 
	while (true  )
 	 {
 	 		if ( (($ligne)!="") ) {
  ( $result = (($result).($ligne))  );
  ( $ligne = fgets($entree) );
 
 }	else{
  throw new ErrorException("Exit");
 	 	 }

 	 }

 	 }
 	 	  catch(ParseError $e){ 
 	fclose($entree);
 	 	 } 

 	 	 catch(ErrorException $e){ 
 	echo   ( "nnn"." \n " ); 

 	 	 } 

 	 return ($result) ; 
 }



	 function write  (  $usr ){
  	 $id =strval($usr['id']); 
	$file = "Inscription.csv";
	$x = ($id.(";".($usr['nom'].(";".($usr['date_naissance'].(";".($usr['mail'].(";".($usr['telephone']."\n") ) ) ) ) ) ) ) ) ;
	 $buff =	buffer_file($file); 
	 $out_chanel =fopen($file , "w" ); 
 ( $buff = (($buff).$x)  );
 
fputs($out_chanel,($buff));
fclose($out_chanel);
 }



	 function write_for_delete  (  $usr ){
  	 $id =strval($usr['id']); 
	$file = "Inscription.csv";
	$x = ($id.(";".($usr['nom'].(";".($usr['date_naissance'].(";".($usr['mail'].(";".$usr['telephone']) ) ) ) ) ) ) ) ;
	 $buff =	buffer_file($file); 
	 $out_chanel =fopen($file , "w" ); 
 ( $buff = (($buff).$x)  );
 
fputs($out_chanel,($buff));
fclose($out_chanel);
 }



	 function clean  (  $file ){
  	 $out_chanel =fopen($file , "w" ); 

fputs($out_chanel,"");
fclose($out_chanel);
 }



	 function delete  (  $id ){
  	$file = "Inscription.csv";
	 $id =strval($id); 
	 $list_ligne =	lire_file($file); 
	 $l =($list_ligne); 
	clean($file);
	for ($i=0; $i <= (sizeof(($l))-1); $i++) 
 	 { 
 	 		$subList = ($l)[$i];
	if ( ($subList[4]<>$id) ) {
 	 $usr_search =
 	[ 
	 	 'id' => intval($subList[4]),
 	 	 'nom' => $subList[3],
 	 	 'date_naissance' => $subList[2],
 	 	 'mail' => $subList[1],
 	 	 'telephone' => $subList[0]
  	] ; 
	write_for_delete(($usr_search));

 }	else{
 
 }

	 }

 }



	 function search  (  $id_usr ){
  	 $usr_search =
 	[ 
	 	 'id' => -1,
 	 	 'nom' => "",
 	 	 'date_naissance' => "",
 	 	 'mail' => "",
 	 	 'telephone' => ""
  	] ; 
	 $id =strval($id_usr); 
	$file = "Inscription.csv";
	 $list_ligne =	lire_file($file); 
	 $l =($list_ligne); 
	for ($i=0; $i <= (sizeof(($l))-1); $i++) 
 	 { 
 	 		$subList = ($l)[$i];
	if ( ($subList[4] == $id) ) {
 
 ($usr_search)['id']=$id_usr; 

 ($usr_search)['nom']=$subList[3]; 

 ($usr_search)['date_naissance']=$subList[2]; 

 ($usr_search)['mail']=$subList[1]; 

 ($usr_search)['telephone']=$subList[0]; 

 }

	 }

 	 return ($usr_search) ; 
 }



	 function get_list  (  ){
  	 $list_users =array(); 
	$file = "Inscription.csv";
	 $list_ligne =	lire_file($file); 
	 $l =($list_ligne); 
	for ($i=0; $i <= (sizeof(($l))-1); $i++) 
 	 { 
 	 		 $usr_search =
 	[ 
	 	 'id' => intval(($l)[$i][4]),
 	 	 'nom' => ($l)[$i][3],
 	 	 'date_naissance' => ($l)[$i][2],
 	 	 'mail' => ($l)[$i][1],
 	 	 'telephone' => ($l)[$i][0]
  	] ; 
 array_unshift( ($list_users) , $usr_search ) ;
 	 }

 	 return ($list_users) ; 
 }



?>
