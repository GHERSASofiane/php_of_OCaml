<html>
   <head>
   	   <LINK rel="stylesheet" type="text/css" href="Style/style.css">
   	   	   	   	<style> 
   	   	   	   	
#fname {
  display: block;
}
input[type=text],input[type=tel],input[type=number],input[type=email] {
    width: 50%;
    padding: 12px 20px;
    margin: 8px 0;
    box-sizing: border-box;
    border: 3px solid #ccc;
    -webkit-transition: 0.5s;
    transition: 0.5s;
    outline: none;
}

input[type=text]:focus {
    border: 3px solid #555;
}
input[type=button], input[type=submit], input[type=reset] {
    background-color: #4CAF50;
    border: none;
    color: white;
    padding: 16px 32px;
    text-decoration: none;
    margin: 4px 2px;
    cursor: pointer;
}

</style>
       <title>Registration</title>
   </head>
<body> 



<ul>
  <li><a href="Index.php">Home</a></li>
  <li><a href="Registration.php">Registration</a></li>
  <li><a href="Listing.php">Listing</a></li>
  <li><a href="Search.php">Search</a></li>
</ul>


 <?php 
  if (!empty($_POST)) {
  	if ($_POST['id']!="") {
  		include "csv.php";   
	  		$user = [ 
		 	 'id' => $_POST['id'],
	 	 	 'nom' => $_POST['nom'],
	 	 	 'date_naissance' => $_POST['dt'],
	 	 	 'mail' => $_POST['mail'],
	 	 	 'telephone' => $_POST['tel']
	  	    ];
	  	write(  $user );
	  	header('Location: Listing.php');
		exit();
  	} 
  }	

?>
<h3>Entrer les informations de l'utilisateur à ajouter.</h3>

<form action="Registration.php" method="post">
  <label for="fname">ID : </label>  <input type="number" id="fname" name="id" ><br>
  <label for="fname">Nom : </label>  <input type="text" id="fname" name="nom" ><br>
  <label for="fname">Date de naissance : </label>  <input type="text" id="fname" name="dt" ><br>
  <label for="fname">Mail : </label>  <input type="email" id="fname" name="mail" ><br>
  <label for="fname">Téléphone : </label>  <input type="tel" id="fname" name="tel" ><br>
  <input type="submit" value="Enregistrer">
</form>

</body>

</html>