(*
<?php
        //Enter your code here, enjoy!
$a = array ('one' => 55, 'two' => "hello", 'three' => "hacene");
    print_r($a);
    $a ['one']= 77;
    $a ['three']= "hacene";
    print_r($a);
    echo $a['one'];
?>
*)


type user = { 
            id: int;
           mutable nom: string; 
           mutable date_naissance: string;
           mutable mail: string;
           mutable telephone: string;
            }


let usr2 = { 
            id = 2 ;
            nom = "kasdi"; 
            date_naissance = "01/03/1994";
            mail = "m.kasdi.h@gmail.com";
            telephone = "111 111 111 111";
            } 
