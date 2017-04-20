open Printf
type user = { 
            id: int;
           mutable nom: string; 
           mutable date_naissance: string;
           mutable mail: string;
           mutable telephone: string;
            }

(************************************************************************************************************)
(**********************************************    pour le test   *******************************************)
(************************************************************************************************************)

let usr1 = { 
            id = 1 ;
            nom = "ghersa"; 
            date_naissance = "01/04/1993";
            mail = "m.ghersa.s@gmail.com";
            telephone = "000  000 000 000";
            }

let usr2 = { 
            id = 2 ;
            nom = "kasdi"; 
            date_naissance = "01/03/1994";
            mail = "m.kasdi.h@gmail.com";
            telephone = "111 111 111 111";
            }

let usr3 = { 
            id = 3 ;
            nom = "toto"; 
            date_naissance = "00/11/2016";
            mail = "toto.tata@gmail.com";
            telephone = "333 333 333 333";
            }

(************************************************************************************************************)
(******************************************    fonction et variable util     ********************************)
(************************************************************************************************************)
let s = ref ""
let sNew = ref ""
let buf = ref []
let list_users = ref []

(* pour remplacer la fonction split *)
let split chaine separateur =
  let result = ref [] in
  begin
        let ch = ref "" in 
        for i = 0 to String.length chaine - 1 do
               begin
                  if chaine.[i] = separateur then 
                  begin
                      result :=  !ch :: !result; ch := ""
                  end
                  else 
                      ch := !ch^(String.make 1 chaine.[i])
               end
        
        done;
        result :=  !ch :: !result;
  end;
 !result

let lire_file file =
  let entree = (open_in file)
  and result = ref [] in 
  begin
        try
            let ligne = ref (split (input_line entree) ';' ) in
            let nb_champs = (List.length !ligne) in
            while true do
                      if (List.length !ligne) != nb_champs then
                          print_endline "Erreur de lecture" 
                      else
                          result := List.append !result [ !ligne ];
                          ligne := (split (input_line entree) ';' ) 
                      done
        with
          | End_of_file -> close_in entree 
          (* fclose($file); *)
  end;
  
  !result



(************************************************************************************************************)
(************************************************************************************************************)
                          (**********   fonction de création d'un user   **********)
(************************************************************************************************************)
(* pour remplacer la fonction split *)
let split chaine separateur =
  let result = ref [] in
  begin
        let ch = ref "" in 
        for i = 0 to String.length chaine - 1 do
               begin
                  if chaine.[i] = separateur then 
                  begin
                      result :=  !ch :: !result; ch := ""
                  end
                  else 
                      ch := !ch^(String.make 1 chaine.[i])
               end
        
        done;
        result :=  !ch :: !result;
  end;
  !result
    

let rec generate_str lst =
  match lst with
  | []  -> () 
  | hd::rst ->begin
                      for i = (List.length hd)-1 downto 0 do
                              if(i = 0) then
                                s:= !s ^ (List.nth hd i)^"\n"
                              else
                                s:= !s ^ (List.nth hd i)^";" 
                      done;                                                    
                      buf:= !s::!buf;
                      s:= ""; 
                      generate_str rst;
              end  

 
let add tab = 
  begin
    for i = 0 to (List.length tab)-1 do

      if(i=(List.length tab)-1)then
                 sNew:= !sNew ^ (List.nth tab i)^"\n"
             else
              sNew:= !sNew ^ (List.nth tab i)^";"
    done;
  end


let write usr =
  let id = (string_of_int (usr.id)) in
  let file = "Inscription.csv" in 
  let list_ligne = lire_file file in
  let l = ref list_ligne in generate_str !l ;
  
  let x = usr.telephone^";"^usr.mail^";"^usr.date_naissance^";"^usr.nom^";"^id in

  let u = (split x ';' )  in add (u);


  buf:= !sNew::!buf ;

  let out_chanel = open_out file in 
        for i = 0 to List.length !buf -1 do
          output_string out_chanel (List.nth !buf i);
        done;
         buf:=[] ;
  close_out out_chanel




(************************************************************************************************************)
(************************************************************************************************************)
                  (**********   fonction de suppression d'un user par id  **********)
(************************************************************************************************************)

let rec generate_dlt lst id =
  match lst with
  | []  ->  s:= !s ^"" 
  | hd::rst ->begin 
                      for i = (List.length hd)-1 downto 0 do
                      begin
                        if (List.nth hd 4) = id  then  ()
                              else 
                              begin
                                
                                  if (i = 0) then
                                      s:= !s ^ (List.nth hd i)^"\n"
                                  else
                                      s:= !s ^ (List.nth hd i)^";"  
                              end 
                      end
                                   
                             
                      done;                                                    
                      buf:= !s::!buf;
                      s:= ""; 
                      generate_dlt rst id;
              end


let delete id = 
  let id_usr = (string_of_int id) in
  let file = "Inscription.csv" in 
  let list_ligne = lire_file file in
  let l = ref list_ligne in generate_dlt !l id_usr;
buf:= !sNew::!buf ;

  let out_chanel = open_out file in 
        for i = List.length !buf -1 downto 0 do
          output_string out_chanel (List.nth !buf i);
        done;
      buf:=[] ; 
  close_out out_chanel



(************************************************************************************************************)
(************************************************************************************************************)
                  (**********   fonction de recherche d'un user par id  **********)
(************************************************************************************************************)

let rec generate_search tab id usr_search =
  match tab with
                | []  ->  () 
                | hd::rst ->begin 
                          if (List.nth hd 4) = id then 
                          begin
                            !usr_search.nom <- (List.nth hd 3);
                            !usr_search.date_naissance <- (List.nth hd 2);
                            !usr_search.mail <- (List.nth hd 1);
                            !usr_search.telephone <- (List.nth hd 0)
                                          
                          end
                          else 
                          begin
                              generate_search rst id usr_search
                          end 

                    end

let search  id_usr = 
     let usr_search = ref { 
                        id = id_usr ;
                        nom = ""; 
                        date_naissance = "";
                        mail = "";
                        telephone = "";
                        } in
    let id = (string_of_int (id_usr)) in
    let file = "Inscription.csv" in 
    let list_ligne = lire_file file in
    let l = ref list_ligne in generate_search !l id usr_search;
    !usr_search
  



(************************************************************************************************************)
(************************************************************************************************************)
                  (**********   fonction de lister tous les users   **********)
(************************************************************************************************************)



 let rec generate_lst lst =
  match lst with
  | []  ->  () 
  | hd::rst ->begin
        let tmp = { 
            id = int_of_string (List.nth hd 4) ;
            nom = (List.nth hd 3); 
            date_naissance = (List.nth hd 2);
            mail = (List.nth hd 1);
            telephone = (List.nth hd 0);
            } in                                             
                      list_users:= tmp::!list_users;
                      generate_lst rst;
              end

let list () = 
  let file = "Inscription.csv" in 
  let list_ligne = lire_file file in
  let l = ref list_ligne in generate_lst !l ;
  !list_users
  
  


(************************************************************************************************************)
(************************************************************************************************************)
                  (**********   jeux de test des différent main    **********)
(************************************************************************************************************)

(* add user 1 *) 
(* let _ =   write usr1;;   *)

(* add user 2 *) 
(* let _ =   write usr2;; *)

(* add user 3 *) 
(* let _ =   write usr3;; *)

(* List users *) 
 (*  let _ = let t = list() in 
 for i = 0 to (List.length t)-1 do
    print_string "Le nom de l'element ";
    print_int i;
    print_string " est : ";
   print_endline (List.nth t i).nom
 done;;  *)


(* let _ = 
  let tab = split "kasdi;hacene;ghersa;sofiane" ';' in 
  print_string " nombre d'element dans le tableau est : "; print_int (List.length tab) ; print_endline ""
 ;; *)

(* delete user 2 *) 
(* let _ =   delete 1 ;;  *)

(* search user " id = 3 " *) 
(* let _ = 
      print_string "Id :"; print_int (search 1).id;print_endline "" ;
      print_string "Nom :"; print_endline (search 1).nom;
      print_string "date_naissance :"; print_endline (search 1).date_naissance;
      print_string "mail :"; print_endline (search 1).mail;
      print_string "telephone :"; print_endline (search 1).telephone ;;  *)