<html>
   <head>
   	<style>
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
.button {
    background-color: #ff0000;
    border: none;
    color: white;
    padding: 15px 32px;
    text-align: center;
    text-decoration: none;
    display: inline-block;
    font-size: 16px;
    margin: 4px 2px;
    cursor: pointer;
}
</style>
   	   <LINK rel="stylesheet" type="text/css" href="Style/style.css">
       <title>Listing</title>
   </head>
<body> 

<?php   include "csv.php";  ?>

<ul>
  <li><a href="Index.php">Home</a></li>
  <li><a href="Registration.php">Registration</a></li>
  <li><a href="Listing.php">Listing</a></li>
  <li><a href="Search.php">Search</a></li>
</ul>

<h2>Liste des utilisateurs</h2>

<table>
  <tr>
    <th>ID</th>
    <th>Nom</th>
    <th>Date De Naissance </th>
    <th>Mail</th>
    <th>Téléphone </th>
    <th> Supprimer </th>
  </tr>
  <?php 

     $result = get_list  (  );
     for ($i=0; $i <= (sizeof($result)-1) ; $i++) { 
  ?>
    <tr>
      <td> <?php echo $result[$i]["id"]; ?> </td>
      <td> <?php echo $result[$i]["nom"]; ?> </td>
      <td> <?php echo $result[$i]["date_naissance"]; ?> </td>
      <td> <?php echo $result[$i]["mail"]; ?> </td>
      <td> <?php echo $result[$i]["telephone"]; ?> </td>
      <td>  <a href="Remove.php?id=<?php echo $result[$i]["id"]; ?>" class="button">Remove</a>     </td>
    </tr>

  <?php  
     }
  ?> 

 
</table>

</body>

</html>