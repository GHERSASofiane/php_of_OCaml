open Typedtree

exception Not_implemented_yet of string

let generate_constant fmt cst =
  let open Asttypes in
  match cst with
   | Const_int i -> Format.fprintf fmt "%d  %s" i ";// On a Traduit un entier "
  | Const_char c -> Format.fprintf fmt "'%c'  %s" c ";// On a Traduit un caractère"
  | Const_string (s_01 , s_02) -> Format.fprintf fmt "%c%s%c  %s" '"' s_01 '"' ";// On a Traduit une chaîne de caractère "
  | Const_float f -> Format.fprintf fmt "%s %s" f ";// On a Traduit un réel"
  | _ -> raise (Not_implemented_yet "generate_constant_error")
           
let generate_expression fmt exp_desc =
  match exp_desc with
  | Texp_constant cst ->
     generate_constant fmt cst
  | _ -> raise (Not_implemented_yet "generate_expression")
            
let generate_value_binding fmt value_binding =
  let {vb_pat; vb_expr; vb_attributes; vb_loc} = value_binding in
  
  let ident =
    match vb_pat.pat_desc with
    | Tpat_var (ident, loc) ->
       ident
    | _ -> raise (Not_implemented_yet "generate_value_binding")
  in
  Format.fprintf fmt "$%s = %a ;\n"
                 ident.Ident.name
                 generate_expression vb_expr.exp_desc
       
let generate_structure_item fmt item =
  let { str_desc; _ } = item in
  match str_desc with
  | Tstr_value (rec_flag, val_binds) ->
     List.iter (generate_value_binding fmt) val_binds
     
  | _ -> raise (Not_implemented_yet "generate_structure_item")
    
       
let generate_from_structure fmt structure =
  let {str_items; str_type; str_final_env} = structure in

  (* Php header *)
  Format.fprintf fmt "<?php\n\n";
  
  List.iter (generate_structure_item fmt) str_items;

  (* Flushing the output *)
  Format.fprintf fmt "\n?>\n%!";
  ()
