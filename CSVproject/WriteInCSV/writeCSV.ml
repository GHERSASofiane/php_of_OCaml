
  open Printf


  let s = ref ""
  let sNew = ref ""
  let buf = ref []

exception Fichier_mal_ecrit 

let list_of_ligne ligne =
  (Str.split (Str.regexp ",") ligne)

  let split_str_func ligne =
  (Str.split (Str.regexp ";") ligne)

let lire_csv f =
  let entree = (open_in f)
  and result = ref []
  in begin
  try
    let ligne = ref  (list_of_ligne (input_line entree))
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

let rec generate_str lst =
	match lst with
	| []  -> s:= !s ^"\n"
	| hd::rst ->begin
                      for i = 0 to (List.length hd)-1 do
                      s:= !s ^ (List.nth hd i)^"\t";
                      done;
                      s:= !s ^"\n";
                                                                                      (* make all strings in the buffer *)
                          buf:= !s::!buf;
                      s:= "";
                      generate_str rst;
    			end  
    	                  
let add tab = 
	begin
		for i = 0 to (List.length tab)-1 do
                 sNew:= !sNew ^ (List.nth tab i)^"\t";
    done;
		sNew:= !sNew ^"\n"
	end
	

let main () = 
  let file = Sys.argv.(1) in let lst = lire_csv file in
  (* let buf = ref [] in *)
    let l = ref lst in
    		 generate_str !l ;
let x = Sys.argv.(2) in
	let u = split_str_func x in 
		 add u;

	buf:= !sNew::!buf ;

          let out_chanel = open_out file in 
    		for i = List.length !buf -1 downto 0 do
      output_string out_chanel (List.nth !buf i);
    		done;
              close_out out_chanel
    		
let _ = main ();;

	
      





