<?php


$usr1 = 
 	[ 
	 	 'id' => 1,
 	 	 'nom' => "ghersa",
 	 	 'date_naissance' => "01/04/1993",
 	 	 'mail' => "m.ghersa.s@gmail.com",
 	 	 'telephone' => "000  000 000 000"
  	] ;
$usr2 = 
 	[ 
	 	 'id' => 2,
 	 	 'nom' => "kasdi",
 	 	 'date_naissance' => "01/03/1994",
 	 	 'mail' => "m.kasdi.h@gmail.com",
 	 	 'telephone' => "111 111 111 111"
  	] ;
$usr3 = 
 	[ 
	 	 'id' => 3,
 	 	 'nom' => "toto",
 	 	 'date_naissance' => "00/11/2016",
 	 	 'mail' => "toto.tata@gmail.com",
 	 	 'telephone' => "333 333 333 333"
  	] ;
$s = ("");
$sNew = ("");
$buf = (array());
$list_users = (array());

	 function split (  $chaine, $separateur ){
  $result = (array());
$ch = ("");
for ($i=0; $i <= (strlen($chaine )-1); $i++) 
 	 { 
 	 		if ( ( ($chaine [$i ])  == $separateur ) ) {
 ($ch )($result );
  ( $ch  = "" );
 
 }	else{
  ( $ch  = (($ch ). ( ($chaine [$i ]) ))  );
 
 }
	 }
 array_unshift ( ($result ) , ($ch ) ) ;
 ($result )
 }



	 function lire_file (  $file ){
  $entree = fopen($file  , "r" ));
$result = (array());

 	 try{ 
 $ligne = (split(input_line($entree ),';');
;
$nb_champs = strlen(($ligne ));
while (true  )
 	 {
 	 		if ( (strlen(($ligne ))!=$nb_champs ) ) {
 echo   ( "Erreur de lecture"." \n " ); 

 }
	else{
  ( $result  = array_merge(($result ),($ligne )) );
 
 }
 ( $ligne  = split(input_line($entree ),';');
 );
 
 	 }

 	 fclose($entree );

 catch (Exception $e){} 
 ($result )
 }



	 function split (  $chaine, $separateur ){
  $result = (array());
$ch = ("");
for ($i=0; $i <= (strlen($chaine )-1); $i++) 
 	 { 
 	 		if ( ( ($chaine [$i ])  == $separateur ) ) {
  array_unshift ( ($result ) , ($ch ) ) ;
  ( $ch  = "" );
 
 }	else{
  ( $ch  = (($ch ). ( ($chaine [$i ]) ))  );
 
 }
	 }
 array_unshift ( ($result ) , ($ch ) ) ;
 ($result )
 }



	 function generate_str (  $lst ){
   
 	 if (sizeof($lst ) == 0) {
 	
 	 }else{

 	 if (sizeof($lst ) >= 1) {
 	for ($i=(strlen($hd )-1); $i >= 0; $i--) 
 	 { 
 	 		if ( ($i  == 0) ) {
  ( $s  = (($s ).($hd [$i ]."\n") )  );
 
 }
	else{
  ( $s  = (($s ).($hd [$i ].";") )  );
 
 }
	 }
 array_unshift ( ($buf ) , ($s ) ) ;
  ( $s  = "" );
 	generate_str ($rst)
 	 }else{

 	 	 }
 
 }



	 function add (  $tab ){
  for ($i=0; $i <= (strlen($tab );
-1); $i++) 
 	 { 
 	 		if ( ($i  == (strlen($tab )-1)) ) {
  ( $sNew  = (($sNew ).($tab [$i ]."\n") )  );
 
 }
	else{
  ( $sNew  = (($sNew ).($tab [$i ].";") )  );
 
 }
	 }

 }



	 function write (  $usr ){
  $id = strval($usr ['id']);
  $file = "Inscription.csv";
$list_ligne = 	lire_file ($file);
$l = ($list_ligne );
	generate_str (($l));
$x = ($usr ['telephone'].(";".($usr ['mail'].(";".($usr ['date_naissance'].(";".($usr ['nom'].(";".$id ) ) ) ) ) ) ) ) ;
$u = split($x ,';');
	add ($u);
 array_unshift ( ($buf ) , ($sNew ) ) ;
 $out_chanel = fopen($file  , "w" ));
for ($i=0; $i <= (strlen(($buf ))-1); $i++) 
 	 { 
 	 	
fputs($out_chanel ,($buf )[$i ]);
	 }
$buf  = array ();
 fclose($out_chanel );
 }



	 function generate_dlt (  $lst, $id ){
   
 	 if (sizeof($lst ) == 0) {
 	 ( $s  = (($s )."")  );
 
 	 }else{

 	 if (sizeof($lst ) >= 1) {
 	for ($i=(strlen($hd )-1); $i >= 0; $i--) 
 	 { 
 	 		if ( ($hd [4] == $id ) ) {
  return ();
 }	else{
 	if ( ($i  == 0) ) {
  ( $s  = (($s ).($hd [$i ]."\n") )  );
 
 }
	else{
  ( $s  = (($s ).($hd [$i ].";") )  );
 
 }

 }

	 }
 array_unshift ( ($buf ) , ($s ) ) ;
  ( $s  = "" );
 generate_dlt($rst ,$id );

 	 }else{

 	 	 }
 
 }



	 function delete (  $id ){
  $id_usr = strval($id );
  $file = "Inscription.csv";
$list_ligne = 	lire_file ($file);
$l = ($list_ligne );
generate_dlt(($l ),$id_usr );
 array_unshift ( ($buf ) , ($sNew ) ) ;
 $out_chanel = fopen($file  , "w" ));
for ($i=(strlen(($buf ))-1); $i >= 0; $i--) 
 	 { 
 	 	
fputs($out_chanel ,($buf )[$i ]);
	 }
$buf  = array ();
 fclose($out_chanel );
 }



	 function generate_search (  $tab, $id, $usr_search ){
   
 	 if (sizeof($tab ) == 0) {
 	()
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



	 function search (  $id_usr ){
  $usr_search = (
 	[ 
	 	 'id' => $id_usr ,
 	 	 'nom' => "",
 	 	 'date_naissance' => "",
 	 	 'mail' => "",
 	 	 'telephone' => ""
  	] ;
$id = strval($id_usr );
  $file = "Inscription.csv";
$list_ligne = 	lire_file ($file);
$l = ($list_ligne );
generate_search(($l ),$id ,$usr_search );
($usr_search )
 }



	 function generate_lst (  $lst ){
   
 	 if (sizeof($lst ) == 0) {
 	()
 	 }else{

 	 if (sizeof($lst ) >= 1) {
 	$tmp = 
 	[ 
	 	 'id' => intval($hd [4]),
 	 	 'nom' => $hd [3],
 	 	 'date_naissance' => $hd [2],
 	 	 'mail' => $hd [1],
 	 	 'telephone' => $hd [0]
  	] ;
 array_unshift ( ($list_users ) , $tmp  ) ;
 	generate_lst ($rst)
 	 }else{

 	 	 }
 
 }



	 function list (  ){
    $file = "Inscription.csv";
$list_ligne = 	lire_file ($file);
$l = ($list_ligne );
	generate_lst (($l));
($list_users )
 }



?>
