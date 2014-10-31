(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(* 			  	 Lexing of Jlite programs 			 	 *)
(* ===================================================== *)

{
  open Jlite_parser (* Assumes the parser file is "parser.mly". *)
  let incr_linenum file_name lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- 
	{ pos with
	    Lexing.pos_fname = file_name;
	    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	    Lexing.pos_bol = pos.Lexing.pos_cnum;
	}
}

let digit = ['0'-'9']
let varid = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let classid = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let charhex = ['0'-'9' 'A'-'F' 'a'-'f']
let whitespace = [' ' '\t']
let newline = ('\n' | '\r' | "\r\n")
let charprintable = ['\032' - '\126']
let stringliteral = ([^ '"' '\\' '\n' '\r'] | '\\' (['\\' '"' '\'' 'n' 'r' 't' 'b']
					|['0'-'9'] ['0'-'9'] ['0'-'9'] |'x' charhex charhex))*
			
rule token file_name = parse
  | '+'		{ PLUS }
  | '-'		{ MINUS }
  | '*'		{ MULTIPLY }
  | '/'		{ DIVIDE }
  | "=="    { EQ }
  | '=' 	{ ASSIGN }
  | "!=" 	{ NEQ }
  | '<' 	{ LE }
  | '>'		{ GRE }
  | ">=" 	{ GEQ }
  | "<="	{ LEQ }
  | "&&" 	{ AND }
  | "||" 	{ OR }
  | '!' 	{ NEG }
  | '{'		{ OBRACE }
  | '}'		{ CBRACE }
  | '('		{ OPAREN }
  | ')'		{ CPAREN }
  | "Int"	{ INT_KWORD}
  | "Bool"	{ BOOL_KWORD}
  | "String" { STRING_KWORD}
  | "true"	{ TRUE_KWORD }
  | "false"	{ FALSE_KWORD }
  | "class" { CLASS_KWORD }
  | "void"  { VOID_KWORD }
  | "while"	{ WHILE_KWORD }
  | "if" 	{ IF_KWORD }
  | "else"	{ ELSE_KWORD }
  | "return" { RETURN_KWORD }
  | "this"	{ THIS_KWORD }
  | "NULL" { NULL_KWORD }
  | "new"	{ NEW_KWORD }
  | "main"  { MAIN_KWORD }
  | "readln" { READ_KWORD }
  | "println" { PRINT_KWORD }
  | ';'		{ SEMICOLON }
  | '.'		{ DOT }
  | ','		{ COMMA }
  | "//"    { single_comment file_name lexbuf }
  | "/*" 	{ multi_comment file_name lexbuf }
  | "*/" 	{ COMMENT_CLOSE }
  | classid as str		{ CLASS_IDENTIFIER str}
  | varid as str1		{ VAR_IDENTIFIER str1 }
  | digit+ as num
		{ INTEGER_LITERAL (int_of_string num) }
  |  "\""( stringliteral
          as s) 
	"\"" { STRING_LITERAL s }
  | whitespace { token file_name lexbuf }
  | '\n' { incr_linenum file_name lexbuf; token file_name lexbuf }
  | '\r' { incr_linenum file_name lexbuf; token file_name lexbuf }
  | "\r\n" { incr_linenum file_name lexbuf; token file_name lexbuf }
  | eof		{ EOF }
  
  and multi_comment file_name = parse
  | "*/" 	{ token file_name lexbuf }
  | _ 		{ multi_comment file_name lexbuf}
  
 and single_comment file_name = parse
  | newline { token file_name lexbuf }
  | _ 		{ single_comment file_name lexbuf}	