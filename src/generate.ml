open Typedtree

exception Not_implemented_yet of string

(* initialize a list to store function names *)
let l = ref []

(* Generate Constant   ******************************************************************************************************* *)

let generate_constant fmt cst =
  let open Asttypes in
  match cst with
  | Const_int i -> Format.fprintf fmt "%d" i 
  | Const_char c -> Format.fprintf fmt "'%c'" c 
  | Const_string (s_01 , s_02) -> Format.fprintf fmt "%c%s%c" '"' s_01 '"' 
  | Const_float f -> Format.fprintf fmt "%s" f 
  | _ -> raise (Not_implemented_yet "generate_constant_error")
  
 (* Generate Function  ******************************************************************************************************* *)
let generation_of_parameter fmt pattern_desc = (* generate parameter *)
  match pattern_desc with
  | Tpat_var (i,loc) ->  Format.fprintf fmt " $%s" loc.txt
  | Tpat_construct (a,b,c) -> Format.fprintf fmt ""

  | Tpat_any -> Format.fprintf fmt "generation_of_parameter_any \n"
  | Tpat_alias (a,b,c) -> Format.fprintf fmt " generation_of_parameter_alias \n"
  | Tpat_constant a -> Format.fprintf fmt " generation_of_parameter_canst \n"
  | Tpat_tuple a -> Format.fprintf fmt " generation_of_parameter_tuple \n"
  | Tpat_variant (a,b,c) -> Format.fprintf fmt " generation_of_parameter_variant \n"
  | Tpat_record (a,b) -> Format.fprintf fmt "  generation_of_parameter_record\n"
  | Tpat_array a -> Format.fprintf fmt " generation_of_parameter_array \n"
  | Tpat_or (a,b,c) -> Format.fprintf fmt " generation_of_parameter_or "
  | Tpat_lazy a -> Format.fprintf fmt " generation_of_parameter_lazy \n"
  |_ ->  Format.fprintf fmt "ERROR_generation_of_parameter"


let rec generate_args fmt case =
   match case with
        |[] ->  Format.fprintf fmt ""
        |arg::[] ->  if arg.c_rhs.exp_loc.loc_ghost then begin
                      generation_of_parameter fmt arg.c_lhs.pat_desc ;
                      Format.fprintf fmt ",";
                      match arg.c_rhs.exp_desc with
                        | Texp_function (label,case,partial) -> Format.fprintf fmt "%a"  generate_args case 
                        | _ -> Format.fprintf fmt "ERROR_generate_args__match"
                       
                      end 
                      else begin
                        generation_of_parameter fmt arg.c_lhs.pat_desc ; 
                        Format.fprintf fmt " ){\n  " ; 
(***************************  le cas ou on mets ou pas le return à l'intérieur d'une fonction  *)
                          match arg.c_rhs.exp_desc with
                          | Texp_apply (exp,l_exp) -> Format.fprintf fmt "\t   return "; generate_expression fmt arg.c_rhs.exp_desc; Format.fprintf fmt " ;\n }\n"; 
                          | _ -> generate_expression fmt arg.c_rhs.exp_desc;Format.fprintf fmt "\n }\n";
                        
                      end
        | _ -> Format.fprintf fmt "ERROR_generate_args"  
 
       
(* Generate Array and tuple  ******************************************************************************************************* *)
and  generate_array_and_tuple fmt tpl =
  let arr_of_tpl = Array.of_list tpl in
        let taille = Array.length arr_of_tpl - 1 in
              for i = 0 to taille  do 
                    if i == taille then begin
                        generate_expression fmt (Array.get arr_of_tpl taille).exp_desc  
                    end  
                    else begin
                       generate_expression fmt (Array.get arr_of_tpl i).exp_desc ; Format.fprintf fmt " , " 
                    end
              done

(* Generate Expression   ******************************************************************************************************* *)

   
and generate_expression fmt exp_desc = 
   let tab_print = ref["print_char";"print_int";"print_float";"print_string";"print_endline";"print_newline"] in
  match exp_desc with
  | Texp_constant cst ->  generate_constant fmt cst 
  | Texp_array ary -> Format.fprintf fmt "array ( " ; generate_array_and_tuple fmt ary ; Format.fprintf fmt ")"
  | Texp_tuple tup -> Format.fprintf fmt "array ( " ; generate_array_and_tuple fmt tup ; Format.fprintf fmt " ) "
  | Texp_function (label,case,partial) ->  Format.fprintf fmt " ( "; generate_args fmt case
  | Texp_ifthenelse (cond,trait,alt) -> begin
                                               Format.fprintf fmt   "\n if( " ;
                                               generat_if_then_else fmt cond ; 
                                               Format.fprintf fmt   " ) {\n " ;
                                        
                                                match trait.exp_desc with

                                                | Texp_constant cst -> Format.fprintf fmt "\t  return ";generat_if_then_else fmt trait; 
                                                                       Format.fprintf fmt " ;\n }"; generate_else fmt alt
                                                | Texp_ident (path,long,typ) -> Format.fprintf fmt "\t  return ";generat_if_then_else fmt trait; 
                                                                       Format.fprintf fmt " ;\n }"; generate_else fmt alt
                                                | Texp_apply (exp,l_exp) -> Format.fprintf fmt "\t  return ";generat_if_then_else fmt trait; 
                                                                       Format.fprintf fmt " ;\n }"; generate_else fmt alt
                                                | _ -> Format.fprintf fmt "\t  "; generat_if_then_else fmt trait; Format.fprintf fmt "\n }";
                                                       generate_else fmt alt
                                        end

  | Texp_apply (exp,l_exp) -> if (List.length l_exp) = 1 then 
                                      begin
                                        match exp.exp_desc with
                                        | Texp_ident (path,long,typ) -> begin
                                          match path with
                                          | Pdot (t,str,i) -> if (List.mem str !tab_print) 
                                                                 then begin (* echo *) 
                                                                    Format.fprintf fmt "echo  ";
                                                                    Format.fprintf fmt " ( ";
                                                                      if str = "print_newline" then 
                                                                            Format.fprintf fmt "\" \\n \""
                                                                      else 
                                                                           generate_param fmt (List.nth l_exp 0);
                                                                           if str = "print_endline" then Format.fprintf fmt ".\" \\n \"" 
                                                                           else Format.fprintf fmt " ";
                                                                      
                                                                    
                                                                    Format.fprintf fmt " ); \n";
                                                                 end  
                                                              else begin  (* operateur unir *)
                                                                    generate_apply_opp fmt exp;
                                                                    Format.fprintf fmt "(";
                                                                    generate_param fmt (List.nth l_exp 0);
                                                                    Format.fprintf fmt ")\n";
                                                              end
                                          | _ -> begin (* appel de fonction récursive *)
                                                    generate_apply_opp fmt exp;
                                                    generate_param fmt (List.nth l_exp 0);
                                                 end
                                        end 
                                        | _ -> Format.fprintf fmt " a_traite_en_generate_expression_Texp_apply_2  ";
                                        
                                      
                                      end
                                      
                                      else generate_Texp_apply fmt exp l_exp;
       
                                
  | Texp_ident (path,long,typ)    -> generate_path fmt path

  | Texp_construct (long_id,constructor_description,exp_list) -> 
                      begin
                           match constructor_description.cstr_res.desc with
                                  Tvar x-> Format.fprintf fmt " Tvar \n"
                                  | Tarrow (lb,typ_exp1,typ_exp2,c) ->Format.fprintf fmt " Tarrow \n"
                                  | Ttuple (typ_exp_lst) ->Format.fprintf fmt " Ttuple \n"
                                  | Tconstr (path,typ_exp_lst,abrv_mem)-> generate_path fmt path (* call the generate_path function *)
                                  | Tfield (str,fld_knd,typ_exp1,typ_exp2)-> Format.fprintf fmt " Tfield \n"
                                  | Tnil -> Format.fprintf fmt " Tnil \n"
                                  |_-> Format.fprintf fmt " generate_Texp_construct \n"
                      end

  | Texp_let (a,b,d) -> Format.fprintf fmt  "generate_expression--generate_let\n"
  | Texp_match (a,b,d,f) -> Format.fprintf fmt   "generate_expression--generate_match"
  | Texp_try (a,b) -> Format.fprintf fmt  "generate_expression--generate_try"
  | Texp_variant (d,f) -> Format.fprintf fmt   "generate_expression--generate_variant"
  | Texp_field (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_field"
  | Texp_setfield (a,b,d,f) -> Format.fprintf fmt  "generate_expression--generate_setfield"
  
  | Texp_sequence (a,b) -> Format.fprintf fmt   "generate_expression--generate_sequence"
  | Texp_while (a,b) -> Format.fprintf fmt   "generate_expression--generate_while"
  | Texp_for (a,b,d,f,m,l) -> Format.fprintf fmt   "generate_expression--generate_for"
  | Texp_send (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_send"
  | Texp_new (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_new"
  | Texp_instvar (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_instvar"
  | Texp_setinstvar (a,b,d,f) -> Format.fprintf fmt   "generate_expression--generate_setinstvar"
  | Texp_letmodule (a,b,d,f) -> Format.fprintf fmt   "generate_expression--generate_letmodule"
  | Texp_assert q -> Format.fprintf fmt   "generate_expression--generate_assert"
  | Texp_lazy q -> Format.fprintf fmt   "generate_expression--generate_lazy"
  | Texp_object (a,b)  -> Format.fprintf fmt  "generate_expression--generate_object"
  | Texp_pack h -> Format.fprintf fmt   "generate_expression--generate_pack"

  | _ -> raise (Not_implemented_yet "generate_expression")  


 (* ************************** generate Texp_apply pour les operateur binaire ************************** *)
and generate_Texp_apply fmt exp l_exp =
 let tab_op= ["+";"+.";"-";"-.";"*";"*.";"/";"/.";"<";"<=";">";">=";"=";"==";"<>";"!="] in
    match exp.exp_desc with
    | Texp_ident (path,long,typ) -> begin
        match path with
        | Pdot (t,str,i) ->begin
              if (List.mem str tab_op)  then 
                begin
                  Format.fprintf fmt " (";generate_param fmt (List.nth l_exp 0);
                  generate_operateur fmt str;
                  generate_param fmt (List.nth l_exp 1);Format.fprintf fmt " )";
                end
                
              else begin
                (*************************** traitement sur les operateur binaire  *)
                match str with
                | "min" -> Format.fprintf fmt "echo min( "; generate_param fmt (List.nth l_exp 0);
                           Format.fprintf fmt " , "; generate_param fmt (List.nth l_exp 1); Format.fprintf fmt " );\n";
                | "max" -> Format.fprintf fmt "echo max( "; generate_param fmt (List.nth l_exp 0);
                           Format.fprintf fmt " , "; generate_param fmt (List.nth l_exp 1); Format.fprintf fmt " );\n";
                | "&&" -> generate_param fmt (List.nth l_exp 0);
                           Format.fprintf fmt " && "; generate_param fmt (List.nth l_exp 1); Format.fprintf fmt "\n";
                | "&" -> generate_param fmt (List.nth l_exp 0);
                           Format.fprintf fmt " && "; generate_param fmt (List.nth l_exp 1); Format.fprintf fmt "\n";
                | "||" -> generate_param fmt (List.nth l_exp 0);
                           Format.fprintf fmt " || "; generate_param fmt (List.nth l_exp 1); Format.fprintf fmt "\n";
                | "mod" -> generate_param fmt (List.nth l_exp 0);Format.fprintf fmt " %c " '%';
                           generate_param fmt (List.nth l_exp 1);Format.fprintf fmt "\n";
                | "**" ->Format.fprintf fmt "pow(";generate_param fmt (List.nth l_exp 0);Format.fprintf fmt " , ";
                        generate_param fmt (List.nth l_exp 1); Format.fprintf fmt ")\n";
                | "^" -> generate_param fmt (List.nth l_exp 0);Format.fprintf fmt " . ";
                           generate_param fmt (List.nth l_exp 1);Format.fprintf fmt "\n";
                

                | _ -> Format.fprintf fmt " NON_traiter_dans_generate_Texp_apply ";
              end    
                           end 
        | Pident ident_t -> begin
           let taille_l_exp = (List.length l_exp) in
                              Format.fprintf fmt " %s (" ident_t.name;
                               
                              if taille_l_exp > 1 then 
                                  begin
                                      for i = 0 to taille_l_exp - 2 do
                                      generate_param fmt (List.nth l_exp i);
                                      Format.fprintf fmt " ,";
                                      done;
                                      generate_param fmt (List.nth l_exp (taille_l_exp - 1));
                                  end
                              else if taille_l_exp = 1 then
                                      generate_param fmt (List.nth l_exp 0)
                              else 
                                 Format.fprintf fmt "";
 
                                Format.fprintf fmt " )";
                            end
        
        | _ -> Format.fprintf fmt " Error_generate_Texp_apply_1 ";
                                    end
    | _ ->  Format.fprintf fmt " Error_generate_Texp_apply_2 "; 

 (* ************************** generate Path.t ************************** *)
 and generate_path fmt path =
              
                       match path with
                      | Pident ident_t ->
                                          if(List.mem ident_t.name !l) 
                              then 
                                begin
                                  Format.fprintf fmt " %s " ident_t.name;
                                end
                                  else
                                    begin
                                      Format.fprintf fmt " $%s  " ident_t.name ;
                                    end

                      | Pdot (t,str,i) -> generate_operateur fmt str;

                      | Papply (t_1,t_2) -> Format.fprintf fmt " == Papply (regarde dans /typing/path.ml)"
                      | _-> Format.fprintf fmt "error_generate___Texp_ident \n"

  (*************************  génération d'opérateur et les operand  *)                    
 and generate_operateur fmt str=             
                               if (String.length str) > 1 && str.[1]='.'  then 
                                                         Format.fprintf fmt " %c " str.[0] 
                                                         else if (String.length str) > 1 && str.[1]='-' then
                                                         Format.fprintf fmt " %c " str.[1]
                                                         else if (String.length str) = 1 && str.[0]='='
                                                         then Format.fprintf fmt " =%s " str
                                                         else if str = "not" then Format.fprintf fmt "! "
                                                         else if str = "succ" then Format.fprintf fmt "(1) + "
                                                         else if str = "pred" then Format.fprintf fmt "--"
                                                         else if str = "abs" then Format.fprintf fmt "echo abs  "
                                                         else Format.fprintf fmt " %s " str

(* Pour appler la fonction generate_expression pour éviter la récursivité *)
and generat_body_else fmt trait = 
    match trait.exp_desc with
          | Texp_constant cst -> Format.fprintf fmt "\t  return "; generate_expression fmt trait.exp_desc;
                                 Format.fprintf fmt " ;";
          | Texp_ident (path,long,typ) -> Format.fprintf fmt "\t  return "; generate_expression fmt trait.exp_desc;
                                          Format.fprintf fmt " ;";
          | Texp_apply (exp,l_exp) -> Format.fprintf fmt "\t  return "; generate_expression fmt trait.exp_desc;
                                      Format.fprintf fmt " ;";
          | _ -> Format.fprintf fmt "\t  "; generate_expression fmt trait.exp_desc

and generat_if_then_else fmt trait = 
              generate_expression fmt trait.exp_desc

and generate_else fmt alt=
              match alt with 
              | Some z-> Format.fprintf fmt "else{\n "; generat_body_else fmt z;
                         Format.fprintf fmt   "\n }\n"
              | None -> Format.fprintf fmt   "\n";


(* généré l'operation avec ces parametre  *)
 and generate_apply_opp fmt exp1= 
           generate_expression fmt exp1.exp_desc 

and generate_param fmt param_op =
  let (lab,exp,op) = param_op in
      match exp with
      | Some b -> generate_expression fmt b.exp_desc
      | None -> Format.fprintf fmt ""
   
 
  
  
(* Generate Value Binding    *************************************************************************************************** *)
 
let generate_value_binding fmt value_binding =
  let {vb_pat; vb_expr; vb_attributes; vb_loc} = value_binding in
  
    match vb_pat.pat_desc with
  | Tpat_var (ident, loc) ->  begin
                                match vb_expr.exp_desc with
                                | Texp_function (label,case,partial) -> begin 
                                (* add the Function name to the list *)
                                      l:=loc.txt::!l;
                                      Format.fprintf fmt "\n function %s%a\n"  loc.txt  generate_expression vb_expr.exp_desc ;

                              end 
                                | _ ->  Format.fprintf fmt "\n $%s = %a;\n"  loc.txt  generate_expression vb_expr.exp_desc
                             end
  | Tpat_any -> Format.fprintf fmt "generate_value_binding_any \n"
  | Tpat_alias (a,b,c) -> Format.fprintf fmt " generate_value_binding_alias \n"
  | Tpat_constant a -> Format.fprintf fmt " generate_value_binding_canst \n"
  | Tpat_tuple a -> Format.fprintf fmt " generate_value_binding_tuple \n"
  | Tpat_construct (a,b,c) -> Format.fprintf fmt " generate_value_binding_construct \n"
  | Tpat_variant (a,b,c) -> Format.fprintf fmt " generate_value_binding_variant \n"
  | Tpat_record (a,b) -> Format.fprintf fmt "  generate_value_binding_record\n"
  | Tpat_array a -> Format.fprintf fmt " generate_value_binding_array \n"
  | Tpat_or (a,b,c) -> Format.fprintf fmt " generate_value_binding_or "
  | Tpat_lazy a -> Format.fprintf fmt " generate_value_binding_lazy \n"
  | _ ->  Format.fprintf fmt " generate_value_binding \n"
      
(* Generate Structure Item   *************************************************************************************************** *)


let generate_structure_item fmt item =
  let { str_desc; _ } = item in
  match str_desc with
  | Tstr_value (rec_flag, val_binds) ->begin 
                                           match rec_flag with
                                           | Nonrecursive -> List.iter (generate_value_binding fmt) val_binds
                                           | Recursive -> 
                                           begin
                                            (* in order to update the list  *)
                                            l:= [];
                                           List.iter (generate_value_binding fmt) val_binds ;
                                           end (* Format.fprintf fmt "rec here \n" *)
                                        end 
  | Tstr_eval (exp,att) ->  generate_expression fmt exp.exp_desc
  | Tstr_primitive a -> Format.fprintf fmt " generate_structure_item_primitiv  \n"
  | Tstr_type e -> Format.fprintf fmt " generate_structure_item_type  \n"
  | Tstr_typext a -> Format.fprintf fmt " generate_structure_item_typex  \n"
  | Tstr_exception a -> Format.fprintf fmt " generate_structure_item_exeption  \n"
  | Tstr_module a -> Format.fprintf fmt " generate_structure_item_module  \n"
  | Tstr_recmodule a-> Format.fprintf fmt " generate_structure_item_recmodule  \n"
  | Tstr_modtype a -> Format.fprintf fmt " generate_structure_item_modtype  \n"
  | Tstr_open a-> Format.fprintf fmt " generate_structure_item_open  \n"
  | Tstr_class a -> Format.fprintf fmt " generate_structure_item_class  \n"
  | Tstr_class_type e -> Format.fprintf fmt " generate_structure_item_class_type  \n"
  | Tstr_include a -> Format.fprintf fmt " generate_structure_item_include  \n"
  | Tstr_attribute a -> Format.fprintf fmt " generate_structure_item_attribut  \n" 
  | _ -> raise (Not_implemented_yet "generate_structure_item")
      
(* Generate From Structure  *************************************************************************************************** *)
     
let generate_from_structure fmt structure =
  let {str_items; str_type; str_final_env} = structure in

  (* Php header *)
  Format.fprintf fmt "<?php\n\n";
  
  List.iter (generate_structure_item fmt) str_items;

  (* Flushing the output *)
  Format.fprintf fmt "\n?>\n%!";
  ()
