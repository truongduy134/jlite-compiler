open Jlite_structs
open Ir3_structs
open Arm_structs

let debug = false
let debug_print s = if debug then print_string s else ()

let flat_map f lst = List.flatten (List.map f lst)

let rec list_take lst n =
  if n <= 0 then []
  else match lst with
  | x::rest -> x::(list_take rest (n - 1))
  | [] -> []

let list_remove el lst = List.filter (fun x -> x != el) lst

let identity x = x



class label_gen prefix =
  object
    val mutable counter = 0
    method fresh =
      counter <- counter + 1;
      prefix ^ string_of_int counter
  end

(* find a label for str, store binding in str_labels *)
let str_label_gen = new label_gen "STR"
let str_labels = Hashtbl.create 100

let add_string str =
  try Hashtbl.find str_labels str
  with Not_found ->
    let l = str_label_gen#fresh in
    Hashtbl.add str_labels str l; l

(*  collect string literals in the program,  generate inst *)
let collect_strings () =
  Hashtbl.fold
  (fun str l inst -> [Label l; PseudoInstr (".asciz \"" ^ str ^ "\"")]@inst)
  str_labels []


let word_size = 4
let return_reg = "a1"
let fp_base = 6 * word_size

let int_of_bool b = if b then 1 else 0
let printf_id = "printf(PLT)"

let var_registers = ["v1"; "v2"; "v3"; "v4"; "v5"; "a4"; "a3"; "a2"]
let spill_registers = ["a1"]

let param_registers = ["a1"; "a2"; "a3"; "a4"]
let num_param_registers = List.length param_registers

type reg_id = string
type mem_id = int

let get_param_reg i =
  if i < num_param_registers then
    ("a" ^ (string_of_int (i + 1)))
  else failwith "invalid param index for register"

let get_param_mem i =
  if i >= num_param_registers then
    - word_size * (i - num_param_registers + 1)
  else failwith ("invalid param index for mem: " ^ (string_of_int i))

let get_mem_addr offset =
  RegPreIndexed ("fp", -offset, false)

let get_param_mem_addr i =
  get_mem_addr (get_param_mem i)

let find_cdata3 id (lst, _, _) = List.find (fun (name, _) -> name = id) lst

let get_cls_size cls p =
  let (_, vars) = find_cdata3 cls p in word_size * (List.length vars)

let find_local_var id md =
  List.find (fun (_, name) -> name = id) (md.localvars3@md.params3)

let index_of_var id vars =
  let rec helper lst count =
    match lst with
    | (_, name)::rest ->
        if id = name then Some count
        else helper rest (count + 1)
    | _ -> None
  in helper vars 0

(* ------ ARM helpers *)

let translate_rel_op op =
  match op with
  | ">" -> "gt"
  | ">=" -> "ge"
  | "<" -> "lt"
  | "<=" -> "le"
  | "==" -> "eq"
  | "!=" -> "ne"
  | _ -> failwith ("unsupported relational op: " ^ op)
let negate_cond cond =
  match cond with
  | "gt" -> "le"
  | "lt" -> "ge"
  | "ge" -> "lt"
  | "le" -> "gt"
  | "eq" -> "ne"
  | "ne" -> "eq"
  | _ -> failwith ("unsupported cond: " ^ cond)

let store_inst reg addr =
  STR ("", "", reg, addr)

let load_inst reg addr =
  LDR ("", "", reg, addr)

let get_int_immed i = ImmedOp ("#" ^ (string_of_int i))

let get_op_str o =
  match o with
  | BooleanOp op -> op
  | RelationalOp op -> op
  | AritmeticOp op -> op
  | UnaryOp op -> op

let int_to_ir3_expr i = Idc3Expr (IntLiteral3 i)
let bool_to_ir3_expr b = Idc3Expr (BoolLiteral3 b)
