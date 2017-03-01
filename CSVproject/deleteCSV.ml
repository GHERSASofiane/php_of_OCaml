  open Printf

let s = ref ""
let sBuf = ref ""
let listToPrint = ref []

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


let rec delete_record lst word =
  match lst with
  | []  -> s:=""
  | hd::rst ->begin
          if(List.mem word hd) then
          begin
          Printf.printf "you have deleted one record \n";
          delete_record rst word  
          end
          else 
              begin
                 for i = 0 to (List.length hd)-1 do
                      if(i = (List.length hd)-1) then
                      s:= !s ^ (List.nth hd i)^"\n"
                    else
                    s:= !s ^ (List.nth hd i)^","
                      done;                                                     (* make all strings in the buffer *)
                          listToPrint:= !s::!listToPrint;
                          s:= "";
                          delete_record rst word;
              end
          end  
    
  

let main () = 
  let file = Sys.argv.(1) in let lst = lire_csv file in
    let l = ref lst in
         delete_record !l Sys.argv.(2);


          let out_chanel = open_out file in 
        for i = List.length !listToPrint -1 downto 0 do
      output_string out_chanel (List.nth !listToPrint i);
        done;
              close_out out_chanel
        
let _ = main ();;