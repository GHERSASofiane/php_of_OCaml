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


<div id="php_bar"> 
<p id="p_php_bar"> Le résultat est : </p>

<?php
//<div id="ocaml_bar">  </div>
//	si on a rien selectionner 
	if (empty($_POST['file_name'])) {
		header('Location: Index.php');
	}
// si on a selectionner un fichier 
	if (!empty($_POST['file_name'])) {
		
		include_once 'tests/' . $_POST['file_name'];
	}
?>
</div> 

</body>

</html>