<html>
   <head>
   	   <LINK rel="stylesheet" type="text/css" href="Style/style.css">
   	   	<style> 
input[type=text] {
    width: 40%;
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
table {
    border-collapse: collapse;
    width: 100%;
}

th, td {
    text-align: left;
    padding: 8px;
}

tr:nth-child(even){background-color: #f2f2f2}

th {
    background-color: #4CAF50;
    color: white;
}
</style>
       <title>Search</title>
   </head>
<body> 



<ul>
  <li><a href="Index.php">Home</a></li>
  <li><a href="Registration.php">Registration</a></li>
  <li><a href="Listing.php">Listing</a></li>
  <li><a href="Search.php">Search</a></li>
</ul>

<h3>Entrer l'identifiant de l'utilisateur rechercher.</h3>


<form action="Search.php" method="post">
  <label for="fname">ID : </label>
  <input type="text" id="fname" name="id" >
  <input type="submit" value="Recherche">
</form>
 <?php 
  if (!empty($_POST)) {
  	if ($_POST['id']=="") {
  		
  	}else{ include "csv.php";   $result = search  ( $_POST['id'] ); 
  	 if($result["id"]!=-1){ 
?>
 
 	<table>
		  <tr>
		    <th>ID</th>
		    <th>Nom</th>
		    <th>Date De Naissance </th>
		    <th>Mail</th>
		    <th>Téléphone </th>
		  </tr>
		    <tr>
		      <td> <?php echo $result["id"]; ?> </td>
		      <td> <?php echo $result["nom"]; ?> </td>
		      <td> <?php echo $result["date_naissance"]; ?> </td>
		      <td> <?php echo $result["mail"]; ?> </td>
		      <td> <?php echo $result["telephone"]; ?> </td>
		    </tr>
    </table>
  		

<?php
}
  	}
  	
  }else{
  	
  }
  ?>


</body>

</html>