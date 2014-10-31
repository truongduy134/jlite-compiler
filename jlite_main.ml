
(* ===================================================== *)
(* ============== CS41212 Compiler Design ============== *)
(* ===================================================== *)

open Printf 

open Jlite_annotatedtyping

open Ir3_structs
open Jlite_toir3


let source_files = ref []

let usage_msg = Sys.argv.(1) ^ " <source files>"

let set_source_file arg = source_files := arg :: !source_files

let parse_file file_name = 
  let org_in_chnl = open_in file_name in
  let lexbuf = Lexing.from_channel org_in_chnl in
    try
  		print_string "Parsing...\n" ;
		print_string file_name ;
		print_string "\n" ;
		let prog =  Jlite_parser.input (Jlite_lexer.token file_name) lexbuf in
		  close_in org_in_chnl;
        prog 
    with
		End_of_file -> exit 0	  

let process file_name prog  = 
	begin
		print_string (Jlite_structs.string_of_jlite_program prog);
		let typedprog= (Jlite_annotatedtyping.type_check_jlite_program prog) in
		print_string (Jlite_structs.string_of_jlite_program typedprog);
		let ir3prog = Jlite_toir3.jlite_program_to_IR3 typedprog in
		print_string (Ir3_structs.string_of_ir3_program ir3prog);
	end
	
let _ = 
 begin
	Arg.parse [] set_source_file usage_msg ;
    match !source_files with
    | [] -> print_string "No file provided \n"
    | x::_-> 
    	let prog = parse_file x in
    	process x prog
 end
 
 