<html>
   <head>
   	   <LINK rel="stylesheet" type="text/css" href="Style/style.css">
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
       <title>Home</title>
   </head>
<body> 



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
  </tr>
  <?php 
      include "csv.php";
     $result = get_list  (  );
     for ($i=0; $i <= (sizeof($result)-1) ; $i++) { 
  ?>
    <tr>
      <td> <?php echo $result[$i]["id"]; ?> </td>
      <td> <?php echo $result[$i]["nom"]; ?> </td>
      <td> <?php echo $result[$i]["date_naissance"]; ?> </td>
      <td> <?php echo $result[$i]["mail"]; ?> </td>
      <td> <?php echo $result[$i]["telephone"]; ?> </td>
    </tr>

  <?php  
     }
  ?> 



<?php
 // include "csv.php";
 // $file = "Inscription.csv";


 // $usr =[ 
	//  	 'id' => 250,
 // 	 	 'nom' => "kasdi",
 // 	 	 'date_naissance' => "01/03/1994",
 // 	 	 'mail' => "m.kasdi.h@gmail.com",
 // 	 	 'telephone' => "012 345 678 999"
 //  	];



                      // Test split
 // $result = split  (  "ghersa;kasdi;hacene;sofiane" , ';' );
 //  print_r  ($result) ;


 //                   //  Test lire_file



 // $result = buffer_file(  $file );
 // echo  ($result) ;

 // $result = lire_file(  $file );
 // print_r  ($result) ;


                     //  Test write
 // write(  $usr );

                     //  Test clean
// clean  ( $file );

// delete  (  20 );


                     //  Test search
// $result = search  (20);
// print_r  ($result) ;


                     //  Test get_list
// $result = get_list  (  );
// echo "Nombre de Enreg :: ".(sizeof($result))."<br>";
// print_r  ($result) ;


?>

</body>

</html>