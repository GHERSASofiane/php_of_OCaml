open Printf

exception Fichier_mal_forme 
exception Erreur_List

let list_of_ligne ligne =
  (Str.split (Str.regexp ",") ligne)

let lire_csv f =
  let entree = (open_in f)
  and resultat = ref []
  in begin
  try
    let ligne = ref  (list_of_ligne (input_line entree))
    in
    let nb_champs = (List.length !ligne)
    in
    while true do
      if (List.length !ligne) != nb_champs then
	raise Fichier_mal_forme
      else
	resultat := List.append !resultat [ !ligne ];
      ligne := (list_of_ligne (input_line entree))
    done
  with
    | End_of_file -> close_in entree
  end;
  !resultat

let main () = 
  let file = Sys.argv.(1) in let lst = lire_csv file in
    let l = ref lst in
                while !l <> [] do
                       match !l with
                                  | hd::rst ->
                                    begin
                                     for i = 0 to (List.length hd)-1 do
                                      try 

                                      let x = (List.nth hd i) in
                                      let lin = list_of_ligne x in 
                                      for i = 0 to List.length lin -1 do
                                        Printf.printf "%s \t" (List.nth lin i)
                                      done
                                      with
                                      | _ ->raise Erreur_List
                                      done;
                                      print_newline();
                                      l := (List.tl !l)
                                    end
                                  | _ -> printf "erreur"
                done


let _ = main ();;

	
      
