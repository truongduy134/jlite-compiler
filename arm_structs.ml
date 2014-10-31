type reg = string

(* empty for default (word), B (byte), 
   SH (signed halfword), H (unsigned halfword), 
   D (doubleword) 
*)
type word_type = string

type label = string

type address_type = 
	| LabelAddr of string
	| Reg of reg
	| RegPreIndexed of reg * int * bool
	| RegPostIndexed of reg * int

type cond = string

(* Operand2 has two possible forms *)
(* #immed_8r (8-bit numeric constant) or Rm (register) *)
type operand2_type = 
	| ImmedOp of string
	| RegOp of reg

(* Memory Access Instructions: LDR or STR
 op{cond}type Rd, [Rn]
 op{cond}type Rd, [Rn, Offset]{!}
 op{cond}type Rd, label
 op{cond}type Rd, [Rn], Offset
*)
type mem_instr_type = 
	cond * word_type * reg * address_type 

(* General Data Processing instruction type *)
(* op{cond}{S} Rd, Rn, Operand2 *)
(* cond is an optional condition code *)
(* S is an optional suffix to denote 
	the updating of condition flags on the result *)
type data_instr_type = 
	cond * bool * reg * reg * operand2_type
	
type multiply_instr_type = 
	cond * bool * reg * reg * reg
(* 
   MOV{cond}{S} Rd, Operand2
   MVN{cond}{S} Rd, Operand2 
*)
type mov_instr_type = 
	cond * bool * reg * operand2_type

(* 
  CMP{cond} Rn, Operand2
  CMN{cond} Rn, Operand2
  TST{cond} Rn, Operand2
  TEQ{cond} Rn, Operand2
*)
type cmp_instr_type = cond * reg * operand2_type

type arm_instr = 
	| PseudoInstr of string
	| Label of string
	| LDR of mem_instr_type 
	| STR of  mem_instr_type
	| LDMFD of (reg list)
	| STMFD of (reg list)
	| ADD of data_instr_type
	| SUB of data_instr_type
	| RSB of data_instr_type
	| AND of data_instr_type
	| ORR of data_instr_type
	| EOR of data_instr_type
	| MOV of mov_instr_type
	| MVN of mov_instr_type
	| CMP of cmp_instr_type
	| CMN of cmp_instr_type
	| TST of cmp_instr_type
	| TEQ of cmp_instr_type
	| B of cond * label
	| BL of cond * label
	| BX of cond * reg
	| MUL of multiply_instr_type
	
type arm_program = arm_instr list
			 				
(*					 *)
(* Display functions *)
(*					 *)

let string_of_address_type s =
	match s with
	| LabelAddr l -> l
	| Reg r -> r
	| RegPreIndexed (r, off, write) -> 
		"["^ r ^ ",#"^ (string_of_int off) 
		^ (if write then "!" else "") ^ "]"
	| RegPostIndexed (r,off) -> 
		"[" ^ r ^ "]" ^ ", #" ^ (string_of_int off)

let string_of_mem_instr_type m = 
	match m with 
	| (c, wordtype, rd, addr) -> 
		c ^ wordtype ^ " " ^ rd ^ "," ^ string_of_address_type addr
	
let string_of_operand2_type op2 =
	match op2 with
	| ImmedOp s-> s
	| RegOp r -> r
	
let string_of_data_instr_type d = 
	match d with 
	| (c, flags, rd, rn, op2) -> 
		c ^ (if flags then "S" else "") 
		^ " " ^ rd ^  "," ^ rn 
		^ "," ^ (string_of_operand2_type op2)
				
let string_of_mov_instr_type m = 
	match m with 
	| (c, flags, rd, op2) ->  
		c ^ (if flags then "S" else "") 
		^ " " ^ rd
		^ "," ^ (string_of_operand2_type op2)
		
let string_of_cmp_instr_type m = 
	match m with 
	| (c, rd, op2) ->  
		c ^ " " ^ rd 
		^ "," ^ (string_of_operand2_type op2)
	
let string_of_multiply_instr_type d = 
	match d with 
	| (c, flags, rd, rn, rs) -> 
		c ^ (if flags then "S" else "") 
		^ " " ^ rd ^  "," ^ rn 
		^ "," ^ rs
		
(* The following function traverses a list, 
	applies a function to each element and concatenates the results *)
let string_of_list lst func delim  = 
	String.concat delim (List.map func lst)
	
let string_of_arm_instr instr = 	
	match instr with
	| PseudoInstr s -> s
	| Label l -> "\n"^l^":"
	| LDR m -> "ldr" ^ string_of_mem_instr_type m
	| STR m -> "str" ^ string_of_mem_instr_type m
	| LDMFD rlist -> 
		"ldmfd sp!,{" 
		^ (string_of_list rlist (fun x -> x) ",") ^ "}"
	| STMFD rlist -> 
		"stmfd sp!,{" 
		^ (string_of_list rlist (fun x -> x) ",") ^ "}"
	| ADD  d -> "add" ^ string_of_data_instr_type d
	| SUB  d -> "sub" ^ string_of_data_instr_type d
	| RSB  d -> "rsb" ^ string_of_data_instr_type d
	| AND  d -> "and" ^ string_of_data_instr_type d
	| ORR  d -> "orr" ^ string_of_data_instr_type d
	| EOR  d -> "eor" ^ string_of_data_instr_type d
	| MOV  m -> "mov" ^ string_of_mov_instr_type m
	| MVN  m -> "mvn" ^ string_of_mov_instr_type m
	| CMP  c -> "cmp" ^ string_of_cmp_instr_type c
	| CMN  c -> "cmn" ^ string_of_cmp_instr_type c
	| TST  c -> "tst" ^ string_of_cmp_instr_type c
	| TEQ  c -> "teq" ^ string_of_cmp_instr_type c
	| B  (c,l) -> "b" ^ c ^ " " ^ l
	| BL (c,l) -> "bl" ^ c ^ " " ^ l
	| BX (c,r) -> "bx" ^ c ^ " " ^ r
	| MUL m -> "mul" ^ string_of_multiply_instr_type m
	
let string_of_arm_prog p = 
	string_of_list p string_of_arm_instr "\n\t"
