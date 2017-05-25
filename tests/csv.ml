open Printf
type user = { 
           mutable id: int;
           mutable nom: string; 
           mutable date_naissance: string;
           mutable mail: string;
           mutable telephone: string;
            }

(************************************************************************************************************)
(******************************************    fonction utile    *************************************)
(************************************************************************************************************)

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




let buffer_file file =
  let entree = (open_in file)
  and result = ref "" in 
  begin
        try
            let ligne = ref (input_line entree)in
            while true do
                      if !ligne != "" then 
                      begin
                          result := !result ^ !ligne ^ "\n";
                          ligne :=  (input_line entree) 
                      end
                    else raise Exit
                      done
        with
          | End_of_file -> close_in entree 
          | Exit -> print_endline "nnn"
          (* fclose($file); *)
  end;
  !result

(************************************************************************************************************)
(******************************************    fonction pour L'ajout    *************************************)
(************************************************************************************************************)

let write usr =
  let id = (string_of_int (usr.id)) in
  let file = "Inscription.csv" in 
   let x = id^";"^usr.nom^";"^usr.date_naissance^";"^usr.mail^";"^usr.telephone^"\n" in
     let buff = ref (buffer_file file) in 
      let out_chanel = open_out file in 
          buff := !buff^x;
          output_string out_chanel !buff;
          close_out out_chanel
        

 

(************************************************************************************************************)
(******************************************    fonction pour supprimer    ***********************************)
(************************************************************************************************************)


 let clean file =
  let out_chanel = open_out file in 
      output_string out_chanel "";
      close_out out_chanel



  let delete id =
  let file = "Inscription.csv" in 
  let id = string_of_int id in
    let list_ligne = lire_file file in
    let l = ref list_ligne in
    clean file;
      for i = 0 to List.length !l -1 do
      begin 
            let subList = (List.nth !l i) in
            if((List.nth subList 4) <> id) then
                begin
                  let usr_search = ref { 
                                id = int_of_string (List.nth subList 4) ;
                                nom = (List.nth subList 3); 
                                date_naissance = (List.nth subList 2);
                                mail = (List.nth subList 1);
                                telephone = (List.nth subList 0);
                                } in
                 write (!usr_search)
                end
          else ()
           
      end
    done

(************************************************************************************************************)
(******************************************    fonction pour la recherche     *******************************)
(************************************************************************************************************)

let search  id_usr = 
     let usr_search = ref { 
                        id = -1 ;
                        nom = ""; 
                        date_naissance = "";
                        mail = "";
                        telephone = "";
                        } in
    let id = (string_of_int (id_usr)) in
    let file = "Inscription.csv" in 
    let list_ligne = lire_file file in
    let l = ref list_ligne in
      for i = 0 to List.length !l -1 do
      begin
        let subList=(List.nth !l i) in
        if((List.nth subList 4) = id) then
        begin
            !usr_search.id <- id_usr;
            !usr_search.nom <- (List.nth subList 3);
            !usr_search.date_naissance <- (List.nth subList 2);
            !usr_search.mail <- (List.nth subList 1);
            !usr_search.telephone <- (List.nth subList 0)
            
        end
      end
        
      done;
  !usr_search 

(************************************************************************************************************)
(******************************************    fonction pour récupérer l'ensemble des users   ***************)
(************************************************************************************************************)

let get_list () = 
  let list_users = ref [] in 
  let file = "Inscription.csv" in 
  let list_ligne = lire_file file in
  let l = ref list_ligne in 
  for i = 0 to List.length !l -1 do
  begin
    let usr_search = ref { 
                                id = int_of_string (List.nth (List.nth !l i) 4) ;
                                nom = (List.nth (List.nth !l i) 3); 
                                date_naissance = (List.nth (List.nth !l i) 2);
                                mail = (List.nth (List.nth !l i) 1);
                                telephone = (List.nth (List.nth !l i) 0);
                                } in
    list_users := usr_search  :: !list_users ;
  end
  done;
  !list_users

(* let sortie = open_out_gen [Open_append] 755 file in
         output_string sortie x;
         close_out sortie  *)
(* let usr1 = { 
            id = 111 ;
            nom = "ghersa"; 
            date_naissance = "01/04/1993";
            mail = "m.ghersa.s@gmail.com";
            telephone = "000  000 000 000";
            }
  let _ = write usr1 ;; *)

(* 
let _ = delete 1;;
 *)
(* let _ = clean "Inscription.csv" ;;
 *)
(* let _ = let result = search 3  in
        print_endline (result.nom);;
 *)

  (* let _ = let result = get_list ()  in
        print_int (List.length result);;
 *)
(* 
let _ = let   usrr = {
            id = 400;
           nom = "gher"; 
            date_naissance = "11/55/8888";
            mail = "gggggggggg";
            telephone = "221110201";
            }


   in write usrr;;
 *)

(* let _ = let result = split "ghersa;sofiane;kasdi" ';' in
        print_int (List.length result);;
 *)
(* 
let _ = let result = lire_file "Inscription.csv"  in
        print_int (List.length result);;

       *)  