open Typedtree

exception Not_implemented_yet of string

let generate_constant fmt cst =
  let open Asttypes in
  match cst with
  | Const_int i -> Format.fprintf fmt "%d " i
  | Const_char i -> Format.fprintf fmt "'%c' " i
  | Const_string (s_01 , s_02) -> Format.fprintf fmt "%c%s%c " '"' s_01 '"' 
  | Const_float f -> Format.fprintf fmt "%s" f 
  | _ -> raise (Not_implemented_yet "generate_constant")

(* Array ******************************************************************************************************************)

let rec generate_array fmt arr =
      match arr with
        |[] -> Format.fprintf fmt " "
        |x::[] -> generate_expression fmt x.exp_desc
        |x::rest -> generate_expression fmt x.exp_desc ; Format.fprintf fmt " , " ; generate_array fmt rest

and generate_expression fmt exp_desc =
  match exp_desc with
  | Texp_constant cst -> generate_constant fmt cst
  | Texp_array arr -> generate_array fmt arr
  | _ -> raise (Not_implemented_yet "generate_expression")
            
let generate_value_binding fmt value_binding =
  let {vb_pat; vb_expr; vb_attributes; vb_loc} = value_binding in
  
  let ident =
    match vb_pat.pat_desc with
    | Tpat_var (ident, loc) ->
       ident
    | _ -> raise (Not_implemented_yet "generate_value_binding")
  in
  Format.fprintf fmt "$%s = array( %a );\n"
                 ident.Ident.name
                 generate_expression vb_expr.exp_desc
       
let generate_structure_item fmt item =
  let { str_desc; _ } = item in
  match str_desc with
  | Tstr_value (rec_flag, val_binds) ->
     List.iter (generate_value_binding fmt) val_binds
  (*add records *)
  
  | _ -> raise (Not_implemented_yet "generate_structure_item")
    
       
let generate_from_structure fmt structure =
  let {str_items; str_type; str_final_env} = structure in

  (* Php header *)
  Format.fprintf fmt "<?php\n\n";
  
  List.iter (generate_structure_item fmt) str_items;

  (* Flushing the output *)
  Format.fprintf fmt "\n?>\n%!";
()