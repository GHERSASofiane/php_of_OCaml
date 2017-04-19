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
<form method="post" action="Executer.php">


   <label>Sélectionner un fichier à tester </label><br /><br />

       <select name="file_name" >


<?php
	//**********************  récupère les fichier avec .php et les proposer pour l'exécution
	$dirname = 'tests/';
	$dir = opendir($dirname); 
	// ******** option par défaut
				echo "<option value=''>Sélectionner</option><br>";

		while($file = readdir($dir)) {
			if( strstr($file, '.php')) {
	// ******* pour chaque fichier récupéré on ajouter une option 
				echo "<option value='$file'>$file</option><br>";
		    }
			
		}

	closedir($dir);
 ?>
         
       </select>

<input type="submit" value="Exécuter" /> 
</form>
</div>



</body>

</html>