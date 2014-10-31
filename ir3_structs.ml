
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(* 		  Structures for IR3 of JLite language 			 *)
(* ===================================================== *)

open Jlite_structs

(* IR3 types, expressions, and statements are represented by Ocaml Variant Types *)
(* Ocaml Variant Types can contain more than one kind of value *)
(* Instantianting such a type is done through the corresponding type constructor *)
(* Ex : BinaryExp3 (ComparisonOp "<=",  IntLiteral3 2, Var3 x) *)
(* Accesing such a type is done through the "match with" construct 
	as can be seen in the code below *)
	
type ir3_op = Jlite_structs.jlite_op

type id3 = string
type cname3 = string
type label3 = int

type ir3_type = Jlite_structs.jlite_type
	
type idc3 = 
  | IntLiteral3 of int
  | BoolLiteral3 of bool
  | StringLiteral3 of string
  | Var3 of id3

type ir3_exp =
  | BinaryExp3 of ir3_op * idc3 * idc3
  | UnaryExp3 of ir3_op * idc3
  | FieldAccess3 of id3 * id3
  | Idc3Expr of idc3
  | MdCall3 of id3 * (idc3 list) 
  | ObjectCreate3 of string

type ir3_stmt = 
	| Label3 of label3
	| IfStmt3 of ir3_exp * label3 
	| GoTo3 of label3 
	| ReadStmt3 of id3
	| PrintStmt3 of idc3
	| AssignStmt3 of id3 * ir3_exp
	| AssignDeclStmt3 of ir3_type * id3 * ir3_exp
	| AssignFieldStmt3 of ir3_exp * ir3_exp
	| MdCallStmt3 of ir3_exp
	| ReturnStmt3 of id3
	| ReturnVoidStmt3

(* Ocaml Tuple Type representing an IR3 variable declaration *)
type var_decl3 = ir3_type * id3

(* Ocaml Record Type representing an IR3 method declaration *)
(* MdDecl -> <Type> <id> ( <FmlList> ) { <VarDecl>* <Stmt>* *)	
type md_decl3 =
	{ 
	  id3: id3;	
	  rettype3: ir3_type;
	  params3:(var_decl3 list);
	  localvars3:(var_decl3 list);
	  ir3stmts:(ir3_stmt list) 
	 }
  
(* Ocaml Tuple Type representing an IR3 class declaration *)
(* ClassDecl -> class <cname> {<VarDecl>* <MdDecl>} *)	  
type cdata3 = cname3 * (var_decl3 list)

(* Ocaml Tuple Type representing an IR3 program *)
type ir3_program = (cdata3 list) * (md_decl3 ) * (md_decl3 list)
	
(* ===================================================== *)
(*  Functions for printing the IR3 *)
(* ===================================================== *)

(* Mutable value for pretty printing and indentation *)
(* Denotes the number of indentation tabs *)
let indent = ref 0

let indent_inc() : string = 
	 indent:= (!indent + 1);""
	
let indent_dec() : string = 
	indent := (!indent - 1); ""	
	
(* display a a number of indentation tabs *)
let rec print_tab(): string = 
	let rec helper_func n: string = 
		if (n <= 0) then "" else "  "^ (helper_func (n -1))
	in helper_func (abs !indent)
	
(* The following function traverses a list, 
	applies a function to each element and concatenates the results *)
let string_of_list lst func delim  = 
	String.concat delim (List.map func lst)
	
(* The following function pretty prints a block of statements *)
let string_of_indented_stmt_list s f xs = 
	let stmtBegin = print_tab() ^ "{\n" in
	let indentI = indent_inc() in
	let stmtList = String.concat s (List.map f xs)in 
	let indentD = indent_dec() in 
	let stmtEnd = "\n" ^ print_tab() ^ "} \n" in 
		stmtBegin ^ indentI 
		^ stmtList ^ indentD ^ stmtEnd
		
(* display an IR3 operator *)
let string_of_ir3_op (e:jlite_op):string =
  match e with
    | BooleanOp s | AritmeticOp s
	| RelationalOp s | UnaryOp s -> s
	
(* display an IR3 type *)
let rec string_of_ir3_type e:string =
  match e with
    | BoolT -> "Bool"
    | IntT -> "Int"
    | StringT -> "String"
	| VoidT -> "void"
	| ObjectT c -> c
	| Unknown -> ""

(* display an id or constant: IRC3 *)
let rec string_of_idc3 e:string =
  match e with
    | BoolLiteral3 v -> (string_of_bool v)
    | IntLiteral3 v -> (string_of_int v)
	| StringLiteral3 v -> "\"" ^ v ^ "\"" 
    | Var3 v -> v
	
(* display an IR3 expr in prefix form *)
let string_of_ir3_exp e:string =
  match e with
    | UnaryExp3 (op,arg) -> 
		"(" ^ string_of_ir3_op op ^ ")[" ^(string_of_idc3 arg)^"]"
    | BinaryExp3 (op,arg1,arg2) -> 
		"[" ^(string_of_idc3 arg1)^","
		^(string_of_idc3 arg2)^"](" 
		^ string_of_ir3_op op ^ ")"
	| FieldAccess3 (id1,id2) -> id1^"."^id2
    | ObjectCreate3 c -> "new " ^ c ^ "()"
    | MdCall3 (id,args) -> 
		"["^ id
		^"("^(string_of_list args string_of_idc3 ",")^ ")]"
	| Idc3Expr e -> (string_of_idc3 e)
  
  
(* display an IR3 statement *)
let string_of_ir3_stmt (s:ir3_stmt):string =
  match s with
	| Label3 l -> " Label " ^ (string_of_int l) ^ ":"
    | IfStmt3 (e, l) -> 
		print_tab() ^ "If(" ^ (string_of_ir3_exp e) 
		^") goto " ^ (string_of_int l) ^";"
	| GoTo3  (l) -> 
		print_tab() ^ "goto " ^ (string_of_int l) ^ ";"
	| ReadStmt3 idc -> print_tab() ^ "readln(" ^ idc ^");"
	| PrintStmt3 idc -> print_tab() 
		^ "println(" ^ (string_of_idc3 idc) ^");"
    | AssignStmt3 (id,e) ->  
		print_tab() ^ id^"="^(string_of_ir3_exp e)^";"
	| AssignFieldStmt3 (id,e) ->  
		print_tab() ^ (string_of_ir3_exp id) ^"="
		^(string_of_ir3_exp e)^";"
	| AssignDeclStmt3 (t, id, e) -> 
		print_tab() ^ (string_of_ir3_type t) ^ id ^ "="
		^(string_of_ir3_exp e)^";"
	| MdCallStmt3 (e) ->  
		print_tab() ^ (string_of_ir3_exp e)^";"
    | ReturnStmt3 id ->  
		print_tab() ^ "Return "^ id ^";"
	| ReturnVoidStmt3 ->  
		print_tab() ^ "Return;"
  
(* display an IR3 variable declaration *)
let string_of_var_decl3 ((t,id):var_decl3) : string = 
	print_tab() ^ (string_of_ir3_type t) ^ " " ^ id

(* display an IR3 method argument declaration *)  
let string_of_arg_decl3 ((t,id):var_decl3) : string = 
	(string_of_ir3_type t) ^ " " ^ id
		
(* display an IR3 method declaration *)
let string_of_meth_decl3 (m:md_decl3) : string = 
	let methodHeader = 
		print_tab() ^ (string_of_ir3_type m.rettype3) 
		^ " " ^ m.id3 ^ 
		"(" ^ (string_of_list m.params3 string_of_arg_decl3 "," ) ^ ")" 
		^"{\n" in
	let indentI = indent_inc() in
	let methodVariables = 
		(string_of_list m.localvars3 string_of_var_decl3 ";\n") 
		^ (if ((List.length m.localvars3) >  0) then ";\n" else "") in
	let methodStmts = 
		(string_of_list m.ir3stmts string_of_ir3_stmt "\n")
		^ (if ((List.length m.ir3stmts) >  0) then "\n" else "") in
	let indentD = indent_dec() in
	let methodEnd = print_tab() ^ "}\n" in
		methodHeader ^ indentI 
		^ methodVariables ^ methodStmts 
		^ indentD ^ methodEnd

(* display an IR3 Class declaration *)
let string_of_cdata3 
	((c,var_list):cdata3) : string = 
	let classHeader = 
		"class " ^ c ^ "{\n" ^ indent_inc() in
	let classBody = 
		(string_of_list var_list string_of_var_decl3 ";\n")  
		^ (if ((List.length var_list) >  0) then ";\n" else "") 
	in let classEnd = indent_dec()^ "}" in
		classHeader ^ classBody  ^ classEnd
						
(* display an IR3 program *)
let string_of_ir3_program 
	((cdata3lst, mainmd3, md3lst):ir3_program) : string = 
	"======= IR3 Program =======\n\n" 
	^ "======= CData3 ======= \n\n" ^ 
		(string_of_list cdata3lst string_of_cdata3 "\n\n" )
	^ "\n\n" ^ "=======  CMtd3 ======= \n\n"
		^ (string_of_meth_decl3 mainmd3) ^ "\n"
		^ (string_of_list md3lst string_of_meth_decl3 "\n" )
	^ "\n======= End of IR3 Program =======\n\n"