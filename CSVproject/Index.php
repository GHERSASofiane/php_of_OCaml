<?php
if (isset($_POST['submit'])){
	$user =$_POST['user'];
	$mail =$_POST['mail'];
	$phone =$_POST['phone'];
$cmd = '"'.$user.",".$mail.",".$phone."'";
$output = shell_exec("./writein te.csv $cmd");
echo "<pre>$output</pre>";
    
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

    <label>Username</label><input type="text" name="delete" class="champs" required="required" /><br />

         
      
<input type="submit" value="delete" name="submit2"/> 

        
        
</form>
    
    <form method="post" action="" class="myForm">
   <label>Display content file </label><br /><br />


         
      
<input type="submit" value="display" name="submit3"/> 

</form>

<?php
if (isset($_POST['submit3'])) {
  $output4 = shell_exec("make disp");
echo "<center><pre>$output4</pre></center>";
}

    if (isset($_POST['submit2'])){
    $delete =$_POST['delete'];
    $todelete = shell_exec("./delete te.csv $delete");
echo "<center><pre>$todelete</pre></center>";
}
    
    ?>


</div>



</body>

</html>
