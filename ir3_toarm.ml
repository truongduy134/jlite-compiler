(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*   Transformation from IR3 to ARM assembly code   *)
(* ===================================================== *)

open Jlite_structs
open Arm_structs
open Ir3_structs
open Common_helper

let reg_descriptor = Hashtbl.create 15
let var_descriptor = Hashtbl.create 5000
let arm_value_regs = ["v1"; "v2"; "v3"; "v4"; "v5"]
(* Globle var for storing Pseudo Instruction *)
let pseudoInstrList = []
let labelcount = ref 0
let next_label () = (labelcount:=!labelcount+1; "L"^(string_of_int !labelcount))

(* Convert a hash table to a list of pairs (key, value) *)
let hashtbl_to_list hashtbl =
  let helper k v mlist = (k, v) :: mlist
  in
    Hashtbl.fold helper hashtbl []

(* Check if a register is in the register list *)
let rec is_in_reg_list (target_reg : reg) (reg_list : reg list) : bool =
  match reg_list with
  | [] -> false
  | head_reg :: tail_list ->
    if (String.compare target_reg head_reg) == 0
    then true
    else is_in_reg_list target_reg reg_list

(* Return the register holding value of the input variable if such exists *)
let find_reg_contain_var (var : idc3): bool * reg =
  let rec helper (reg_list : (reg * idc3) list) (var : idc3): bool * reg =
    match reg_list with
    | [] -> (false, "")
    | (cur_reg, cur_var) :: tail_lst ->
      if cur_var = var
      then (true, cur_reg)
      else helper tail_lst var
  in
    helper (hashtbl_to_list reg_descriptor) var

(* Return an empty ARM value register if such exists *)
let find_empty_value_reg (): bool * reg =
  let rec helper (reg_list : reg list): bool * reg =
    match reg_list with
    | [] -> (false, "")
    | cur_reg :: tail_lst ->
      if Hashtbl.mem reg_descriptor cur_reg
      then helper tail_lst
      else (true, cur_reg)
  in helper arm_value_regs

(* Return true if the address type is for variable in memory *)
let is_var_memory_addr (addr_type : address_type): bool =
  match addr_type with
  | RegPreIndexed _ -> true
  | RegPostIndexed _ -> true
  | _ -> false

(* Return true if the value of the input variable is stored in memory *)
let has_value_in_memory (var : idc3): bool =
  if not (Hashtbl.mem var_descriptor var)
  then false
  else
    let rec helper (store_locations : address_type list): bool =
      match store_locations with
      | [] -> false
      | head_location :: tail_lst ->
        if (is_var_memory_addr head_location)
        then true
        else helper tail_lst
    in
    let store_locations = Hashtbl.find var_descriptor var
    in helper store_locations

(* Add binding register to the variable it is holding variable *)
let rec add_reg_bindings (reg_var_lst : reg * idc3 list): unit =
  match reg_var_lst with
  | [] -> ()
  | (head_reg, head_var) :: tail_lst ->
    begin
      Hashtbl.replace reg_descriptor head_reg head_var;
      add_reg_bindings tail_lst
    end

(* Remove register binding *)
let rec remove_reg_bindings_for_vars (var_lst : idc3 list): unit =
  match var_lst with
  | [] -> ()
  | head_var :: tail_lst ->
    begin
      let (found, head_reg) = find_reg_contain_var head_var in
      let _ =
        if found
        then Hashtbl.remove head_reg
        else ()
      in remove_reg_bindings_for_vars tail_lst
    end

(* Return a register that can be used to hold variable value. A list of exluded
   registers can be specified, if so, the returned register will not be in
   the excluded list. The return register comes with an integer value:
   0: target variable's value is already in the returned register
   1: target variable's value is not in the returned register, but there
      is no spilling
   2: target variable's value is not in the returned register, but there
      is spilling
   Assume we can always find such register *)
let get_reg_single
  (target_var : idc3)
  (excluded_reg_lst : reg list option): int * reg =
  match (find_reg_contain_var target_var) with
  | (true, result_reg) -> (0, result_reg)
  | (false, _) ->
    begin
    match (find_empty_value_reg ()) with
    | (true, result_reg) -> (1, result_reg)
    | (false, _) ->
      begin
        let excluded_regs =
          match excluded_reg_lst with
          | None -> []
          | Some lst -> lst
        in
        let rec helper
            (reg_lst : reg list)
            (excluded_lst : reg list): (bool * reg) list =
          match reg_lst with
          | [] -> []
          | head_reg :: tail_lst ->
            if (is_in_reg_list head_reg excluded_lst)
            then helper tail_lst excluded_lst
            else
              let reg_var = Hashtbl.find reg_descriptor head_reg in
              let need_spill = not (has_value_in_memory reg_var) in
              (need_spill, head_reg) :: (helper tail_lst excluded_lst)
        in
        let rec choose_non_spill
            (reg_status_lst : (bool * reg) list): bool * (bool * reg) =
          match reg_status_lst with
          | [] -> (false, (false, ""))
          | (need_spill, head_reg) :: tail_lst ->
            if not need_spill
            then (true, (need_spill, head_reg))
            else choose_non_spill tail_lst
        in
          let reg_status_lst = helper arm_value_regs excluded_regs
          in
            begin
              match (choose_non_spill reg_status_lst) with
              | (true, (stat, m_reg)) -> (1, m_reg)
              | (false, _) ->
                let lst_length = List.length reg_status_lst in
                let (stat, m_reg) =
                  List.nth reg_status_lst (Random.int lst_length)
                in (2, m_reg)
            end
      end
    end

(* Return registers that can be used to hold values for two input variables.
   Each return register comes with an integer value:
   0: target variable's value is already in the returned register
   1: target variable's value is not in the returned register, but there
      is no spilling
   2: target variable's value is not in the returned register, but there
      is spilling
   This function is suitable for finding registers for copy instructions or
   instructions with right-hand side involving only 1 variable operand
   Assume we can always find such registers *)
let get_reg_two
  (left_var : idc3)
  (right_var : idc3): (int * reg) * (int * reg) =
  let (right_flg, right_reg) = get_reg_single right_var None in
  if left_var = right_var
  then ((0, right_reg), (right_flg, right_reg))
  else
  let (left_flg, left_reg) = get_reg_single left_var (Some [right_reg])
  in ((left_flg, left_reg), (right_flg, right_reg))

(* Return registers that can be used to hold values for three input variables.
   Each return register comes with an integer value:
   0: target variable's value is already in the returned register
   1: target variable's value is not in the returned register, but there
      is no spilling
   2: target variable's value is not in the returned register, but there
      is spilling
   This function is suitable for finding registers for instructions with
   right-hand side involving only 2 variable operands
   Assume we can always find such registers *)
let get_reg_three
  (left_var : idc3)
  (right_var_1 : idc3)
  (right_var_2 : idc3): (int * reg) * (int * reg) * (int * reg) =
  let hashtbl = Hashtbl.create 2 in
  let (right_flg_1, right_reg_1) = get_reg_single right_var_1 None in
  let _ = Hashtbl.add hashtbl right_var_1 (right_flg_1, right_reg_1) in
  let (right_flg_2, right_reg_2) =
    if right_var_1 = right_var_2
    then (0, right_reg_1)
    else get_reg_single right_var_2 (Some [right_reg_1])
  in
  let _ = Hashtbl.replace hashtbl right_var_2 (right_flg_2, right_reg_2)
  in
  let (left_flg, left_reg) =
    if Hashtbl.mem hashtbl left_var
    then Hashtbl.find hashtbl left_var
    else get_reg_single right_var_2 (Some [right_reg_1; right_reg_2])
  in ((left_flg, left_reg), (right_flg_1, right_reg_1),
      (right_flg_2, right_reg_2))


let rec spill_reg lst = 
  match lst with
  | [] -> []
  | (need_to_spill, r)::lst -> 
    if need_to_spill 
    then let var_to_spill = Hashtbl.find reg_descriptor r in
      let addresses = Hashtbl.find var_descriptor var_to_spill in
      if (List.length addresses) != 1 then failwith ("descriptor for variable not found: " ^ (string_of_idc3 var_to_spill))
      else let address = List.nth addresses 0 in 
      let str_instr = STR ("", "", r, address) in
      str_instr::(spill_reg lst)
    else spill_reg lst




let ir3_stmt_to_arm (struct_list:cdata3 list) (md_decl:md_decl3) (stmt:ir3_stmt) (exit_label:label) = 
  let to_armlabel (label:label3) = let armlabel = "."^label in (Label armlabel) in
  match stmt with
  | Label3 label3 -> let armlabel = "."^(string_of_int label3) in [(Label armlabel)]
  | IfStmt3 (ir3_exp, label3) -> 
                                begin 
                                match ir3_exp with
                                | BinaryExp3 (op, b1, b2) -> []
                                | _ -> failwith ""
                                end
  | GoTo3 label3 -> let armlabel = "."^(string_of_int label3) in [(B ("", armlabel))]
  | ReadStmt3 id3  -> []
  | PrintStmt3 idc3 -> []
  | AssignStmt3 (leftid, ir3_exp) -> 
    begin
      match ir3_exp with
      | BinaryExp3 (ir3_op, idc3a, idc3b) ->
        let ((spilled1, rleft), (spilled2, rright1), (spilled3, rright2)) = get_reg_three (Var3 leftid) idc3a idc3b in 
        let spill_instrs = spill_reg [(spilled1, rleft); (spilled2, rright1); (spilled3, rright2)] in 
        let assgn_instr = 
        begin
          match ir3_op with
            | BooleanOp op -> 
              if (op = "&&") then [AND ("", false, rleft, rright1, RegOp rright2)]
              else if (op = "||") then [ORR ("", false, rleft, rright1, RegOp rright2)]
              else failwith ("unrecognised booleanop "^op)
            | RelationalOp op -> 
              let cmp_instr = CMP ("", rright1, RegOp rright2) in 
              let true_cond = 
                if (op = "<") then "lt"
                else if (op = ">") then "gt"
                else if (op = "<=") then "le"
                else if (op = ">=") then "ge"
                else if (op = "==") then "eq"
                else if (op = "!=") then "ne"
                else failwith ("unrecognised RelationalOp "^op) in 
              let false_cond = 
                if (op = "<") then "ge"
                else if (op = ">") then "le"
                else if (op = "<=") then "gt"
                else if (op = ">=") then "lt"
                else if (op = "==") then "ne"
                else if (op = "!=") then "eq"
                else failwith ("unrecognised RelationalOp "^op) in 
              let mov_instr1 = MOV (true_cond, false, rleft, ImmedOp "#1") in 
              let mov_instr2 = MOV (false_cond, false, rleft, ImmedOp "#0") in 
              [cmp_instr; mov_instr1; mov_instr2]
            | AritmeticOp op -> 
              if (op = "+") then [ADD ("", false, rleft, rright1, RegOp rright2)]
              else if (op = "-") then [SUB ("", false, rleft, rright1, RegOp rright2)] 
              else if (op = "*") then [MUL ("", false, rleft, rright1, rright2)]  
              else failwith ("unrecognised AritmeticOp "^op)             
        end in spill_instrs@assgn_instr
      | UnaryExp3 (ir3_op, idc3) -> 
        let ((spilled1, rleft),(spilled2, rright)) = get_reg_two (Var3 leftid) idc3 in
        let _ = spill_reg [(spilled1, rleft); (spilled2, rright)] in
        begin
          match ir3_op with
          | UnaryOp op -> 
            if (op = "-") then [RSB ("", false, rleft, rright, ImmedOp "#0")]
            else if (op = "!") then [EOR ("", false, rleft, rright, ImmedOp "#1")]
          | _ -> failwith "unrecognized op"
        end
      | FieldAccess3 (id3a, id3b) ->
        let 
      | Idc3Expr idc3 -> 
        let ((spilled1, rleft),(spilled2, rright)) = get_reg_two (Var3 leftid) idc3 in
        let _ = spill_reg [(spilled1, rleft); (spilled2, rright)] in
        [MOV ("", false, rleft, RegOp rright)]
      | MdCall3 (id3, idc3s) -> []
      | ObjectCreate3 c -> []
    end
  | AssignDeclStmt3 (ir3_type, id3, ir3_exp) -> []
  | AssignFieldStmt3 (ir3_exp1, ir3_exp2) ->
    begin
      match ir3_exp1 with
      | FieldAccess3 (id1, id2) -> 
        begin
        match ir3_exp2 with
        | BinaryExp3 (ir3_op, idc3a, idc3b) ->
          let ((spilled1, rleft), (spilled2, rright1), (spilled3, rright2)) = get_reg_three (Var3 "_____temp") idc3a idc3b in 
          let spill_instrs = spill_reg [(spilled1, rleft); (spilled2, rright1); (spilled3, rright2)] in 
          let assgn_instr = 
          begin
            match ir3_op with
              | BooleanOp op -> 
                if (op = "&&") then [AND ("", false, rleft, rright1, RegOp rright2)]
                else if (op = "||") then [ORR ("", false, rleft, rright1, RegOp rright2)]
                else failwith ("unrecognised booleanop "^op)
              | RelationalOp op -> 
                let cmp_instr = CMP ("", rright1, RegOp rright2) in 
                let true_cond = 
                  if (op = "<") then "lt"
                  else if (op = ">") then "gt"
                  else if (op = "<=") then "le"
                  else if (op = ">=") then "ge"
                  else if (op = "==") then "eq"
                  else if (op = "!=") then "ne"
                  else failwith ("unrecognised RelationalOp "^op) in 
                let false_cond = 
                  if (op = "<") then "ge"
                  else if (op = ">") then "le"
                  else if (op = "<=") then "gt"
                  else if (op = ">=") then "lt"
                  else if (op = "==") then "ne"
                  else if (op = "!=") then "eq"
                  else failwith ("unrecognised RelationalOp "^op) in 
                let mov_instr1 = MOV (true_cond, false, rleft, ImmedOp "#1") in 
                let mov_instr2 = MOV (false_cond, false, rleft, ImmedOp "#0") in 
                [cmp_instr; mov_instr1; mov_instr2]
              | AritmeticOp op -> 
                if (op = "+") then [ADD ("", false, rleft, rright1, RegOp rright2)]
                else if (op = "-") then [SUB ("", false, rleft, rright1, RegOp rright2)] 
                else if (op = "*") then [MUL ("", false, rleft, rright1, rright2)]  
                else failwith ("unrecognised AritmeticOp "^op)             
          end in spill_instrs@assgn_instr
        | UnaryExp3 (ir3_op, idc3) -> 
          let ((spilled1, rleft),(spilled2, rright)) = get_reg_two (Var3 leftid) idc3 in
          let _ = spill_reg [(spilled1, rleft); (spilled2, rright)] in
          begin
            match ir3_op with
            | UnaryOp op -> 
              if (op = "-") then [RSB ("", false, rleft, rright, ImmedOp "#0")]
              else if (op = "!") then [EOR ("", false, rleft, rright, ImmedOp "#1")]
            | _ -> failwith "unrecognized op"
          end
        | FieldAccess3 (id3a, id3b) ->
          let 
        | Idc3Expr idc3 -> 
          let ((spilled1, rleft),(spilled2, rright)) = get_reg_two (Var3 leftid) idc3 in
          let _ = spill_reg [(spilled1, rleft); (spilled2, rright)] in
          [MOV ("", false, rleft, RegOp rright)]
        | MdCall3 (id3, idc3s) -> []
        | ObjectCreate3 c -> []
        end
      | _ -> failwith "invalid field assign stmt"
    end
  | MdCallStmt3 ir3_exp -> []
  | ReturnStmt3 id3 -> 
    (* need to check if a1 needs to be spilled *)[MOV ("",false,"a1", RegOp id3); B ("",exit_label)] (*need get reg for id3*)
  | ReturnVoidStmt3 -> 
    [B ("",exit_label)]

let exit_label_gen = new label_gen ".L"

let rec get_obj_size (struct_list:cdata3 list) (obj:class_name) = 
  let rec helper lst cname = 
    match lst with
    | [] -> failwith "class name not found"
    | (c, vars)::lst -> 
      if (c = obj) then get_num_of_data struct_list vars 
      else helper lst cname in 
  helper struct_list obj

and get_num_of_data (struct_list:cdata3 list) (vars:var_decl3 list) = 
  match vars with
  | [] -> 0
  | (t, id)::vars -> 
    begin
      match t with
      | ObjectT obj -> (get_obj_size struct_list obj) + (get_num_of_data (struct_list:cdata3 list) vars)
      | _ -> 1 + (get_num_of_data (struct_list:cdata3 list) vars)
    end


let ir3_md_to_arm (struct_list:cdata3 list) (md_decl:md_decl3) =
  let md_start_label = Label md_decl.id3 in 
  let exitLabel = (exit_label_gen#fresh^"exit:") in 
  let stmfd_instr = STMFD (["fp"; "lr"; "v1"; "v2"; "v3"; "v4"; "v5"]) in 
  let set_fp_instr = ADD ("", false, "fp", "sp", ImmedOp "#24") in 
  let offset = (List.length md_decl.params3) + (get_num_of_data struct_list md_decl.localvars3) - 1 in 
  let offset_str = "#"^(string_of_int (offset*4 + 24)) in 
  let set_sp_instr = SUB ("", false, "sp", "fp", ImmedOp offset_str) in 
  let body_arm = List.concat (List.map (ir3_stmt_to_arm struct_list md_decl exitLabel) md_decl.ir3stmts) in 
  let exit_instr_label = Label exitLabel in 
  let reset_sp_instr = SUB ("", false, "sp", "fp", ImmedOp "#24") in 
  let ldmfd_instr = LDMFD (["fp"; "pc"; "v1"; "v2"; "v3"; "v4"; "v5"]) in
  (md_start_label::stmfd_instr::set_fp_instr::set_sp_instr::body_arm)@([exit_instr_label; reset_sp_instr; ldmfd_instr])


(* Transform an IR3 program to IR3 *)
let ir3_program_to_arm
  ((struct_list, main_md, md_list): ir3_program): arm_program = 
  let mds_in_arms = List.map (ir3_md_to_arm struct_list) md_list in 
  let main_in_arms = ir3_md_to_arm struct_list main_md in 
  List.concat (mds_in_arms@[main_in_arms])
