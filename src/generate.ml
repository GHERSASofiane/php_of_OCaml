open Typedtree

exception Not_implemented_yet of string

(* Generate Constant   ******************************************************************************************************* *)

let generate_constant fmt cst =
  let open Asttypes in
  match cst with
  | Const_int i -> Format.fprintf fmt "%d" i 
  | Const_char c -> Format.fprintf fmt "'%c'" c 
  | Const_string (s_01 , s_02) -> Format.fprintf fmt "%c%s%c" '"' s_01 '"' 
  | Const_float f -> Format.fprintf fmt "%s" f 
  | _ -> raise (Not_implemented_yet "generate_constant_error")
  
(* Generate Array   ******************************************************************************************************* *)
let generate_arg fmt pattern_desc =
  match pattern_desc with
  | Tpat_var (i,loc) ->  Format.fprintf fmt " %s" loc.txt
  | Tpat_constant cst ->  Format.fprintf fmt " Constant"
  |_ ->  Format.fprintf fmt " k "

 let rec generate_a fmt arr =
      match arr with
        |[] ->  Format.fprintf fmt ""
        |x::[] -> generate_arg fmt x.c_lhs.pat_desc
        |x::rest -> generate_arg fmt x.c_lhs.pat_desc ; Format.fprintf fmt "," ; generate_a fmt rest
       
let rec generate_array fmt arr =
      match arr with
        |[] -> Format.fprintf fmt " "
        |x::[] -> generate_expression fmt x.exp_desc 
        |x::rest -> generate_expression fmt x.exp_desc  ; Format.fprintf fmt " , " ; generate_array fmt rest
   
(* Generate Expression   ******************************************************************************************************* *)
 
and generate_expression fmt exp_desc =
  match exp_desc with
  | Texp_constant cst ->  generate_constant fmt cst 
  | Texp_array ary -> Format.fprintf fmt "array ( " ; generate_array fmt ary ; Format.fprintf fmt ")"
  | Texp_tuple tup -> Format.fprintf fmt "array ( " ; generate_array fmt tup ; Format.fprintf fmt " ) "
  | Texp_ident (a,b,d)    -> Format.fprintf fmt  "generate_expression--generate_identt"
  | Texp_let (a,b,d) -> Format.fprintf fmt  "generate_expression--generate_let"
  
  | Texp_function (a,b,c) ->  Format.fprintf fmt "(%a ){body}"  generate_a b 
  
  | Texp_apply (a,b) -> Format.fprintf fmt  "generate_expression--generate_apply"
  | Texp_match (a,b,d,f) -> Format.fprintf fmt   "generate_expression--generate_match"
  | Texp_try (a,b) -> Format.fprintf fmt  "generate_expression--generate_try"
  | Texp_construct (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_construnct"
  | Texp_variant (d,f) -> Format.fprintf fmt   "generate_expression--generate_variant"
  | Texp_field (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_field"
  | Texp_setfield (a,b,d,f) -> Format.fprintf fmt  "generate_expression--generate_setfield"
  | Texp_ifthenelse (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_if then else"
  | Texp_sequence (a,b) -> Format.fprintf fmt   "generate_expression--generate_seq"
  | Texp_while (a,b) -> Format.fprintf fmt   "generate_expression--generate_whille"
  | Texp_for (a,b,d,f,m,l) -> Format.fprintf fmt   "generate_expression--generate_fouuuurrr"
  | Texp_send (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_send"
  | Texp_new (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_new"
  | Texp_instvar (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_instvar"
  | Texp_setinstvar (a,b,d,f) -> Format.fprintf fmt   "generate_expression--generate_set inv"
  | Texp_letmodule (a,b,d,f) -> Format.fprintf fmt   "generate_expression--generate_letmod"
  | Texp_assert q -> Format.fprintf fmt   "generate_expression--generate_assert"
  | Texp_lazy q -> Format.fprintf fmt   "generate_expression--generate_laz"
  | Texp_object (a,b)  -> Format.fprintf fmt  "generate_expression--generate_object"
  | Texp_pack h -> Format.fprintf fmt   "generate_expression--generate_pack"
  | _ -> raise (Not_implemented_yet "generate_expression")

(* Generate Dolar  ******************************************************************************************************* *)

  and generate_dolar fmt exp_desc =
  match exp_desc with
  | Texp_constant cst ->  Format.fprintf fmt "$" 
  | Texp_array ary -> Format.fprintf fmt "$" 
  | Texp_tuple tup -> Format.fprintf fmt "$" 
  | Texp_ident (a,b,d)    -> Format.fprintf fmt "" 
  | Texp_let (a,b,d) -> Format.fprintf fmt ""
  
  | Texp_function (a,b,d) -> Format.fprintf fmt "function "
  
  | Texp_apply (a,b) -> Format.fprintf fmt ""
  | Texp_match (a,b,d,f) -> Format.fprintf fmt ""
  | Texp_try (a,b) -> Format.fprintf fmt ""
  | Texp_construct (a,b,d) -> Format.fprintf fmt ""
  | Texp_variant (d,f) -> Format.fprintf fmt ""
  | Texp_field (a,b,d) -> Format.fprintf fmt ""

  (* | Texp_setfield (a,b,d,f) -> Format.fprintf fmt  "generate_expression--generate_setfield"
  | Texp_ifthenelse (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_if then else"
  | Texp_sequence (a,b) -> Format.fprintf fmt   "generate_expression--generate_seq"
  | Texp_while (a,b) -> Format.fprintf fmt   "generate_expression--generate_whille"
  | Texp_for (a,b,d,f,m,l) -> Format.fprintf fmt   "generate_expression--generate_fouuuurrr"
  | Texp_send (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_send"
  | Texp_new (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_new"
  | Texp_instvar (a,b,d) -> Format.fprintf fmt   "generate_expression--generate_instvar"
  | Texp_setinstvar (a,b,d,f) -> Format.fprintf fmt   "generate_expression--generate_set inv"
  | Texp_letmodule (a,b,d,f) -> Format.fprintf fmt   "generate_expression--generate_letmod"
  | Texp_assert q -> Format.fprintf fmt   "generate_expression--generate_assert"
  | Texp_lazy q -> Format.fprintf fmt   "generate_expression--generate_laz"
  | Texp_object (a,b)  -> Format.fprintf fmt  "generate_expression--generate_object"
  | Texp_pack h -> Format.fprintf fmt   "generate_expression--generate_pack"
   *)
  | _ -> raise (Not_implemented_yet "generate_expression")
     
(* Generate Value Binding    *************************************************************************************************** *)
 
let generate_value_binding fmt value_binding =
  let {vb_pat; vb_expr; vb_attributes; vb_loc} = value_binding in
  
    match vb_pat.pat_desc with
  | Tpat_var (ident, loc) -> 
      Format.fprintf fmt "%a%s = %a;\n" generate_dolar vb_expr.exp_desc loc.txt  generate_expression vb_expr.exp_desc                                                   
  | Tpat_any -> Format.fprintf fmt " any \n"
  | Tpat_alias (a,b,c) -> Format.fprintf fmt " alias \n"
  | Tpat_constant a -> Format.fprintf fmt " canst \n"
  | Tpat_tuple a -> Format.fprintf fmt " tuple \n"
  | Tpat_construct (a,b,c) -> Format.fprintf fmt " construct \n"
  | Tpat_variant (a,b,c) -> Format.fprintf fmt " variant \n"
  | Tpat_record (a,b) -> Format.fprintf fmt "  record\n"
  | Tpat_array a -> Format.fprintf fmt " array \n"
  | Tpat_or (a,b,c) -> Format.fprintf fmt " or "
  | Tpat_lazy a -> Format.fprintf fmt " lazy \n"
  | _ ->  Format.fprintf fmt " errroooorr \n"
      
(* Generate Structure Item   *************************************************************************************************** *)
      
let generate_structure_item fmt item =
  let { str_desc; _ } = item in
  match str_desc with
  | Tstr_value (rec_flag, val_binds) -> List.iter (generate_value_binding fmt) val_binds
  | Tstr_eval (a,b) ->  Format.fprintf fmt " eval  \n"
  | Tstr_primitive a -> Format.fprintf fmt " primitiv  \n"
  | Tstr_type e -> Format.fprintf fmt " type  \n"
  | Tstr_typext a -> Format.fprintf fmt " typex  \n"
  | Tstr_exception a -> Format.fprintf fmt " exeption  \n"
  | Tstr_module a -> Format.fprintf fmt " module  \n"
  | Tstr_recmodule a-> Format.fprintf fmt " recmodule  \n"
  | Tstr_modtype a -> Format.fprintf fmt " modtype  \n"
  | Tstr_open a-> Format.fprintf fmt " open  \n"
  | Tstr_class a -> Format.fprintf fmt " class  \n"
  | Tstr_class_type e -> Format.fprintf fmt " class_type  \n"
  | Tstr_include a -> Format.fprintf fmt " include  \n"
  | Tstr_attribute a -> Format.fprintf fmt " attribut  \n" 
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
