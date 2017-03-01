<?php
if (isset($_POST['submit'])){
	$user =$_POST['user'];
	$mail =$_POST['mail'];
	$phone =$_POST['phone'];
$cmd = $user.";".$mail.";".$phone;
$output = shell_exec("ocamlc str.cma read.ml");
echo "<pre>$output</pre>";
// $varr = './a.out te.csv "'.$cmd.'"';
//  shell_exec($varr);
// echo "<center><pre>$varr</pre></center>";
}

if (isset($_POST['submit2'])){

}
    
 if (isset($_POST['submit3'])){
  
}
?>

<html>
   <head>
   	   <LINK rel="stylesheet" type="text/css" href="Style/style.css">
       <title>PHP_OF_OCAML</title>
   </head>
<body>

<div id="logo">
<img src="Image/ocaml-logo.png" width="20%" id="logo_ocaml">
<img src="Image/php-logo.png" width="10%" id="logo_php">
</div>

<div id="search_bar">
<form method="post" action="" class="myForm">

   <label>Store data in csv file </label><br /><br />

    <label>Username</label><input type="text" name="user" class="champs" required="required" /><br />
	<label>Mail</label><input type="text" name="mail" class="champs" required="required"/><br />
	<label>PhoneNumber</label><input type="text" name="phone" class="champs" required="required"/><br />

         
      
<input type="submit" value="valide" name="submit1"/> 

</form>
    
    
    <form method="post" action="" class="myForm">
   <label>Delete data </label><br /><br />

    <label>Username</label><input type="text" name="user" class="champs" required="required" /><br />

         
      
<input type="submit" value="valide" name="submit2"/> 

        
        
</form>
    
    <form method="post" action="" class="myForm">
   <label>Search data from file </label><br /><br />

    <label>Username</label><input type="text" name="user" class="champs" required="required" /><br />

         
      
<input type="submit" value="valide" name="submit3"/> 

</form>


</form>
</div>



</body>

</html>
