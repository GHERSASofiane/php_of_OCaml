let add x y = let z = x + y in z;;


let x = [88] in
match x with
| [] 				-> print_int 7
| a::[] 			-> print_int 88
| a::b::[] 			-> print_int 19
| a::b::c 			-> print_int 39
| a::b::c::d::[] 		-> print_int 77
| _				-> print_int 98


let is_a_vowel c = 
	match c with 
    'a' 	 -> true 
  | 'R' 	 -> true
  | _ 		 -> false ;;



type user = { 
            id: int;
           mutable nom: string; 
           mutable date_naissance: string;
           mutable mail: string;
           mutable telephone: string;
            }



let usr1 = { 
            id = 1 ;
            nom = "ghersa"; 
            date_naissance = "01/04/1993";
            mail = "m.ghersa.s@gmail.com";
            telephone = "000  000 000 000";
            }


let rec generate_search tab id usr_search =
      match tab with
                | []  ->  () 
                | hd::rst ->begin 
                          if (List.nth hd 4) = id then 
                          begin
                            !usr_search.nom <- (List.nth hd 3);
                            !usr_search.date_naissance <- (List.nth hd 2);
                            !usr_search.mail <- (List.nth hd 1);
                            !usr_search.telephone <- (List.nth hd 0);
                                          
                          end
                            else 
                          begin
                              generate_search rst id usr_search
                          end
                   		 end
