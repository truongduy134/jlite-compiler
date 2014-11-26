open Jlite_structs
open Ir3_structs
open Common_helper
open Ir3_to_cfg


let blocks_to_stmts = flat_map (fun b -> b.stmts)

let update_expr expr_f stmt =
  match stmt with
	| IfStmt3 (e, l) -> IfStmt3 (expr_f e, l)
	| AssignStmt3 (var, e) -> AssignStmt3 (var, expr_f e)
	| AssignFieldStmt3 (FieldAccess3 (var, f), e2) ->
    AssignFieldStmt3 (FieldAccess3 (var, f), expr_f e2)
	| MdCallStmt3 e -> MdCallStmt3 (expr_f e)
  | _ -> stmt

(* dead node(def) elimination
 * "a = a;" statements are eliminated too *)
let eliminate_dead_node v =
  let helper stmt node =
    match stmt with
    | AssignStmt3 (id, e) ->
      if e <> Idc3Expr (Var3 id) && Id3Set.mem id node.out_var then
        [stmt]
      else begin
        match e with
        (* md call may have side-effects, keep the call *)
        | MdCall3 _ -> [MdCallStmt3 e]
         (* ObjectCreate3 have no side-effects
          * eliminate. *)
        | _ -> []
      end
    | _ -> [stmt]
  in
  let block = !(v.block) in
  let stmt_list = List.map2 helper block.stmts v.subnodes in
  List.flatten stmt_list

(* constant folding *)
let fold_constant v =
  let fold_equality e op p1 p2 =
    match (p1, p2) with
    | (Var3 a, Var3 b) ->
      if a <> b then e
      else bool_to_ir3_expr (if op = "==" then true else false)
    | (Var3 _, _) | (_, Var3 _) -> e
    | _ ->
      bool_to_ir3_expr (if op = "==" then p1 = p2 else p1 <> p2)
  in let fold_expr e =
    match e with
    | BinaryExp3 (RelationalOp "==", p1, p2) ->
      fold_equality e "==" p1 p2
    | BinaryExp3 (RelationalOp "!=", p1, p2) ->
      fold_equality e "!=" p1 p2

    | BinaryExp3 (op, IntLiteral3 i1, IntLiteral3 i2) ->
      begin
        match op with
        | AritmeticOp "+" -> int_to_ir3_expr (i1 + i2)
        | AritmeticOp "-" -> int_to_ir3_expr (i1 - i2)
        | AritmeticOp "*" -> int_to_ir3_expr (i1 * i2)
        | RelationalOp ">" -> bool_to_ir3_expr (i1 > i2)
        | RelationalOp "<" -> bool_to_ir3_expr (i1 < i2)
        | RelationalOp ">=" -> bool_to_ir3_expr (i1 >= i2)
        | RelationalOp "<=" -> bool_to_ir3_expr (i1 <= i2)
        | _ -> e
      end
    | BinaryExp3 (BooleanOp op, v, BoolLiteral3 b)
    | BinaryExp3 (BooleanOp op, BoolLiteral3 b, v) ->
      begin
        match (op, b) with
        | ("&&", false) -> bool_to_ir3_expr false
        | ("||", true) -> bool_to_ir3_expr true
        | ("&&", true) | ("||", false) -> Idc3Expr v
        | _ -> e
      end
    | UnaryExp3 (UnaryOp "-", IntLiteral3 i) -> int_to_ir3_expr (-i)
    | UnaryExp3 (UnaryOp "!", BoolLiteral3 b) -> bool_to_ir3_expr (not b)
    | _ -> e
  in let helper stmt = update_expr fold_expr stmt in
  let block = !(v.block) in
  List.map helper block.stmts

type binding_term =
  | VarTerm of id3
  | FieldTerm of id3 * id3
  | LiteralTerm of idc3
type binding_expr = string * (binding_term list)

(* means binding_expr can be replaced by id3 *)
type binding = binding_expr * binding_term

let string_of_binding_term t =
  match t with
  | VarTerm v -> v
  | LiteralTerm v -> string_of_idc3 v
  | FieldTerm (v, f) -> v ^ "." ^ f
let string_of_binding_expr (op, lst) =
  "[" ^ op ^ "(" ^ (string_of_list lst string_of_binding_term ", ") ^ ")]"
let string_of_binding (e, t) =
  (string_of_binding_expr e) ^ " -> " ^ (string_of_binding_term t)
let string_of_binding_map m =
  string_of_list m string_of_binding ", "

let filter_binding_map f map =
  List.filter (fun ((_, terms), value) ->
    List.for_all f (value::terms)
  ) map

let expire_binding updated binding_map =
  (* expire binding with any invalidated term *)
  filter_binding_map (fun term ->
    match (updated, term) with
    | (VarTerm a, VarTerm b) | (VarTerm a, FieldTerm (b, _)) ->
      (* if a var is updated, expire the var and its fields *)
      a <> b
    | (FieldTerm (_, f1), FieldTerm (_, f2)) ->
      (* if a field is updated, expire all fields with the same name *)
      f1 <> f2
    | _ -> true
  ) binding_map

(* called when encounter a method call *)
let expire_all_fields binding_map =
  filter_binding_map (fun term ->
    match term with
    | FieldTerm _ -> false
    | _ -> true
  ) binding_map

let find_bound_value (op, terms) binding_map =
  let helper e =
    List.filter (fun (e2, _) -> e = e2) binding_map
  in let values =
    match op with
    | "+" | "*" | "&&" | "||" | "==" | "!=" ->
      (* comutative *)
      (helper (op, terms))@(helper (op, List.rev terms))
    | _ -> helper (op, terms)
  in match values with
  | [] -> None
  | (_, v)::_ -> Some v

(* check if a binding is in the map *)
let is_bound (e, t) binding_map =
  (find_bound_value e binding_map) = Some t

let idc3_to_binding_term idc3 =
  match idc3 with
  | Var3 id -> VarTerm id
  | v -> LiteralTerm v

let ir3_expr_to_term_expr ir3_e =
  match ir3_e with
  | Idc3Expr (Var3 var) -> (* not going to replace literals *)
    ("=", [VarTerm var])
  | FieldAccess3 (var, f) ->
    ("=", [FieldTerm (var, f)])
  | UnaryExp3 (o, p) ->
    ((get_op_str o), [idc3_to_binding_term p])
  | BinaryExp3 (o, p1, p2) ->
    let t1 = idc3_to_binding_term p1 in
    let t2 = idc3_to_binding_term p2 in
    ((get_op_str o), [t1; t2])
  | _ -> ("", [])

let update_binding stmt binding_map =
  let update_copy def_term value_expr map =
    (* replace all def_term by value_expr - copy/constant propagation *)
    let t =
      match value_expr with
      | Idc3Expr v -> idc3_to_binding_term v
      | FieldAccess3 (var, f) -> FieldTerm (var ,f)
      | _ -> failwith "invalid value_expr"
    in
    if t = def_term then map
    else (("=", [def_term]), t)::(expire_binding def_term map)
  in let update_subexpr t ir3_e map =
    (* replace all ir3_e by t - common subexpr *)
    let new_map =
      match ir3_e with
      | MdCall3 _ -> expire_all_fields map
      | _ -> map
    in match (ir3_expr_to_term_expr ir3_e, t) with
    | (("", _), _) | (("=", [VarTerm _]), FieldTerm _) ->
      (* never replace a var by a field access *)
      (expire_binding t new_map)
    | (e, _) ->
      if e = ("=", [t]) then new_map (* no change *)
      else (e, t)::(expire_binding t new_map)
  in match stmt with
  | AssignStmt3 (id, Idc3Expr idc3) ->
    update_copy (VarTerm id) (Idc3Expr idc3) binding_map
  | AssignFieldStmt3 (FieldAccess3 (var, f), Idc3Expr idc3) ->
    (* replace all var.f by idc3 - copy/constant propagation *)
    let ft = FieldTerm (var, f) in
    update_copy ft (Idc3Expr idc3) binding_map
  | AssignFieldStmt3 (FieldAccess3 (var1, f1), FieldAccess3 (var2, f2)) ->
    let ft = FieldTerm (var1, f1) in
    update_copy ft (FieldAccess3 (var2, f2)) binding_map

  | AssignStmt3 (id, e) ->
    update_subexpr (VarTerm id) e binding_map
  | AssignFieldStmt3 (FieldAccess3 (var, f), e) ->
    update_subexpr (FieldTerm (var, f)) e binding_map

  | MdCallStmt3 _ ->
    expire_all_fields binding_map
  | _ -> binding_map

let replace_bindings stmt binding_map =
  let replace_var x =
    let be = ("=", [VarTerm x]) in
    match find_bound_value be binding_map with
    | Some (VarTerm var) -> var
    | _ -> x
  in let replace_idc3 idc3 =
    (* NOTE: copy/constant propogation *)
    let be = ("=", [idc3_to_binding_term idc3]) in
    match find_bound_value be binding_map with
    | Some (LiteralTerm v) -> v (* constant prop *)
    | Some (VarTerm var) -> Var3 var (* copy prop *)
    | _ -> idc3
  in let replace_expr e =
    (* NOTE: common subexpr/constant propagation *)
    let be = ir3_expr_to_term_expr e in
    match find_bound_value be binding_map with
    | Some (LiteralTerm v) -> Idc3Expr v (* constant prop *)
    | Some (VarTerm v) -> Idc3Expr (Var3 v) (* common subexpr *)
    | Some (FieldTerm (v, f)) -> FieldAccess3 (v, f) (* common subexpr *)
    | _ ->
      match e with
      | MdCall3 (md, args) ->
        MdCall3 (md, List.map replace_idc3 args)
      | BinaryExp3 (op, p1, p2) ->
        BinaryExp3 (op, replace_idc3 p1, replace_idc3 p2)
      | UnaryExp3 (op, p) -> UnaryExp3 (op, replace_idc3 p)
      | FieldAccess3 (var, field) -> FieldAccess3 (replace_var var, field)
      | Idc3Expr idc3 -> Idc3Expr (replace_idc3 idc3)
      | ObjectCreate3 _ -> e
  in match stmt with
	| IfStmt3 (e, l) -> IfStmt3 (replace_expr e, l)
	| PrintStmt3 idc3 -> PrintStmt3 (replace_idc3 idc3)
	| AssignStmt3 (var, e) -> AssignStmt3 (var, replace_expr e)
	| AssignFieldStmt3 (FieldAccess3 (var, f), e2) ->
    AssignFieldStmt3 (FieldAccess3 (replace_var var, f), replace_expr e2)
	| MdCallStmt3 e -> MdCallStmt3 (replace_expr e)
	| ReturnStmt3 var -> ReturnStmt3 (replace_var var)
  | _ -> stmt

(* copy propagation and common subexpr elimination *)
let propogate_copy stmts =
  let blocks = ir3_stmts_to_cfg stmts in
  let binding_maps = Hashtbl.create (List.length blocks) in
  let get_binding_map blk_beg =
    try Hashtbl.find binding_maps blk_beg
    with _ -> []
  in let get_pred_binding_map blk =
    let binding_maps = List.map (fun b -> get_binding_map !b.beg) blk.pred in
    match binding_maps with
    | [] -> []
    | binding_map::rest ->
      List.filter (fun x -> List.for_all (is_bound x) rest) binding_map
  in let propagate_stmt (binding_map, stmts) stmt =
    (* apply propogation, need to be done before updating map to avoid cycles *)
    let new_stmt = replace_bindings stmt binding_map in
    let new_binding_map = update_binding new_stmt binding_map in
    (new_binding_map, stmts@[new_stmt])
  in let propagate_block change blk =
    let binding_map = get_pred_binding_map blk in
    let (new_binding_map, new_stmts) =
      List.fold_left propagate_stmt (binding_map, []) blk.stmts in
    let new_change = change || blk.stmts <> new_stmts in
    begin
      blk.stmts <- new_stmts;
      (* update binding_map of blk *)
      Hashtbl.replace binding_maps blk.beg new_binding_map;
      new_change
    end
  in let rec helper () =
    let change = List.fold_left propagate_block false blocks in
    if change then helper ()
    else ()
  in let _ = helper () in
  blocks_to_stmts blocks


(* dead code elimination *)
let eliminate_dead_code stmts =
  let removed_dead_branch = flat_map (fun s ->
    match s with
    | IfStmt3 (Idc3Expr (BoolLiteral3 false), _) -> []
    | IfStmt3 (Idc3Expr (BoolLiteral3 true), l) -> [GoTo3 l]
    | _ -> [s]
  ) stmts in
  let blocks = ir3_stmts_to_cfg removed_dead_branch in
  match blocks with
  | [] -> []
  | entry::rest ->
    (* remove blocks that have no entry *)
    let live_blocks = List.filter (fun b -> (List.length b.pred) > 0) rest in
    blocks_to_stmts (entry::live_blocks)

(* combine blocks *)
let combine_blocks stmts =
  let blocks = ir3_stmts_to_cfg stmts in
  let end_blocks = List.filter (fun b -> (List.length b.succ) <> 1) blocks
  in let remove_pre_label stmts =
    match stmts with
    | (Label3 _)::rest -> rest
    | _ -> stmts
  in let remove_post_goto stmts =
    match List.rev stmts with
    | (GoTo3 _)::rest -> List.rev rest
    | _ -> stmts
  in let rec compress b =
    if List.length b.pred = 1 then
      let pred = !(List.hd b.pred) in
      if List.length pred.succ == 1 then
        begin
          pred.stmts <- (remove_post_goto pred.stmts)@(remove_pre_label b.stmts);
          b.stmts <- [];
          compress pred
        end
      else ()
    else ()
  in let _ = List.iter compress end_blocks in
  blocks_to_stmts blocks

let local_opt stmts =
  let opt = [eliminate_dead_node; fold_constant] in
  let blocks = ir3_stmts_to_cfg stmts in
  let live_fg = analyze_live_var blocks in
  (* use one cfg as local optimizations don't change control flow *)
  let _ = List.iter (fun f ->
    List.iter (fun node ->
      let stmts = f node in
      !(node.block).stmts <- stmts
    ) live_fg
  ) opt
  in blocks_to_stmts blocks

let global_opt stmts =
  let opt = [propogate_copy; eliminate_dead_code; combine_blocks] in
  List.fold_left (fun s f -> f s) stmts opt

let iter_optimize_ir3 stmts =
  global_opt (local_opt stmts)

let optimize_ir3_md md =
  let rec helper stmts =
    (* repeat optimization until closed *)
    let new_stmts = iter_optimize_ir3 stmts in
    if new_stmts = stmts then stmts
    else helper new_stmts
  in let s = helper md.ir3stmts in
  {
    id3 = md.id3;
    rettype3 = md.rettype3;
    params3 = md.params3;
    localvars3 = md.localvars3;
    ir3stmts = s;
  }

let optimize_ir3_program (cls_list, main, md_list) =
  (cls_list, (optimize_ir3_md main), (List.map optimize_ir3_md md_list))
