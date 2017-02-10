open Cmt_format

let cmt_file = ref ""
let output_file = ref "a.php"

let use = "Usage: php_of_ocaml <cmt_file>"
                   
let () =
  let open Arg in
  if Array.length Sys.argv < 2 &&
       Filename.check_suffix Sys.argv.(1) ".cmt"; then
    begin
      print_endline use;
      exit 1
    end
  else
    Arg.parse
      [ "-o", Set_string output_file, "Specify the output file" ]
      (fun s -> cmt_file := s)
      use

let run () =
  let (_, cmt_opt) = read !cmt_file in
  let ({cmt_annots; _} as cmt) =
    match cmt_opt with Some cmt -> cmt | None -> assert false in
  let structure = match cmt_annots with
    | Implementation str -> str
    | _ -> failwith "Expected a structure ast"
  in
  let out_fmt = Format.formatter_of_out_channel (open_out !output_file) in
  Generate.generate_from_structure out_fmt structure
                                   
let () = run ()
