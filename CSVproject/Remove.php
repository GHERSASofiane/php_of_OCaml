<?php 


include "csv.php";
delete  (  $_GET["id"] );

header('Location: Listing.php');
exit();

?>