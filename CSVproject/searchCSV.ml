  open Printf

let s = ref ""
let sBuf = ref ""
let listSearch = ref []

exception Fichier_mal_ecrit 

let list_of_ligne ligne =
  (Str.split (Str.regexp ",") ligne)

  let split_str_func ligne =
  (Str.split (Str.regexp ";") ligne)

let lire_csv f =                                          (* reading a CSV file *)
  let entree = (open_in f)
  and result = ref []
  in begin
  try
    let ligne = ref  (list_of_ligne (input_line entree))  (* split the line and save the strings in a List and return this list at the end of file *)
    in
    let nb_champs = (List.length !ligne)
    in
    while true do
      if (List.length !ligne) != nb_champs then
	raise Fichier_mal_ecrit
      else
	result := List.append !result [ !ligne ];
      ligne := (list_of_ligne (input_line entree))
    done
  with
    | End_of_file -> close_in entree
  end;
  !result

  let print_list_found listSearch =                         (* printing result *)
  if(List.length listSearch = 0 ) then
                Printf.printf "The word not found \n"
              else
  for i = List.length listSearch-1 downto 0 do
    Printf.printf "> : %s \n" (List.nth listSearch i );
  done
  

  let rec searchLine lst wordToSearch =
    match lst with
  | []  -> print_list_found !listSearch                     (* the end of the list we call [print_list_found (LIST)] to print the result  *)
  | hd::rst ->begin
            let sBuf = ref "" in                            (* save the result in this string *)
                for i = 0 to (List.length hd)-1 do
                let refHead = ref hd in
                  let word = (List.nth hd i) in 
                  
                  if(word = wordToSearch) then 
                  begin
                     Printf.printf "The word was found : %s \n" word;
                     for j = 0 to List.length !refHead -1 do
                     sBuf:= !sBuf ^ (List.nth !refHead j) ^ "\t"
                     done;

                      listSearch := !sBuf :: !listSearch;
                  end
                done;
                      searchLine rst wordToSearch
              end  

  

	

let main () =

  let file = Sys.argv.(1) in let lst = lire_csv file in
   let x = Sys.argv.(2) in searchLine lst x

let _ = main ();;