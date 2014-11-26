open Jlite_structs
open Ir3_structs
open Common_helper

(* -------- control flow graph --------- *)

type ir3_block = {
  beg : int;
  mutable stmts : ir3_stmt list;
  mutable pred : (ir3_block ref) list;
  mutable succ : (ir3_block ref) list;
}

let print_blocks md_id blocks =
  debug_print ("CFG for " ^ md_id ^ ": \n");
  List.iter (fun b ->
    let ed = b.beg + (List.length b.stmts) - 1 in
    debug_print ("\t" ^ (string_of_int b.beg) ^ "-" ^ (string_of_int ed));
    debug_print " pred(";
    debug_print
      (string_of_list b.pred (fun bp -> (string_of_int !bp.beg)) ",");
    debug_print ") succ(";
    debug_print
      (string_of_list b.succ (fun bp -> (string_of_int !bp.beg)) ",");
    debug_print ")\n";
  ) blocks

let ir3_stmts_to_cfg stmts =
  let rec helper stmts cur num =
    let create_block s e =
      if s = [] then []
      else [{
        beg = e - (List.length s) + 1;
        stmts = s;
        pred = [];
        succ = [];
      }]
    in match stmts with
    | stmt::rest ->
        begin
          match stmt with
          | Label3 l ->
              (* start of a block *)
              let follow = helper rest [stmt] (num + 1) in
              (create_block cur (num - 1))@follow
          | IfStmt3 _ | GoTo3 _ | ReturnStmt3 _ | ReturnVoidStmt3 ->
              (* end of a block *)
              (create_block (cur@[stmt]) num)@(helper rest [] (num + 1))
          | PrintStmt3 _ | ReadStmt3 _ | AssignStmt3 _ | AssignDeclStmt3 _
          | AssignFieldStmt3 _ | MdCallStmt3 _ ->
              helper rest (cur@[stmt]) (num + 1)
        end
    | [] -> create_block cur (num - 1)
  in let blocks = helper stmts [] 1
  in let find_block l = List.find (fun b -> (List.hd b.stmts) = Label3 l) blocks
  in let add_edge out_block in_block =
    begin
      out_block.succ <- (ref in_block)::out_block.succ;
      in_block.pred <- (ref out_block)::in_block.pred
    end
  in let add_next_edge i =
    if List.length blocks > i + 1 then
      add_edge (List.nth blocks i) (List.nth blocks (i + 1))
    else ()
  in let _ = (* add graph edges *)
    List.iteri (fun i b ->
      match List.hd (List.rev b.stmts) with
      | GoTo3 l -> add_edge b (find_block l)
      | IfStmt3 (_, l) ->
        begin
          add_edge b (find_block l);
          add_next_edge i
        end
      | ReturnStmt3 _ | ReturnVoidStmt3 -> ()
      | _ -> add_next_edge i
    ) blocks
  in blocks


(* -------- live variables analysis --------- *)

module Id3Set = Set.Make(String)

type live_var_subnode = {
  number : int;
  use : Id3Set.t;
  def : Id3Set.t;
  in_var : Id3Set.t;
  out_var : Id3Set.t;
}

type live_var_node = {
  block : ir3_block ref;
  subnodes: live_var_subnode list;
}

let print_live_var_graph g stmts =
  let string_of_set s =
    "(" ^ (String.concat "," (Id3Set.elements s)) ^ ")"
  in
  debug_print "Live variable analysis: \n";
  List.iter (fun n ->
    List.iteri (fun i v ->
      debug_print (string_of_int v.number);
      debug_print ": ";
      debug_print ((string_of_ir3_stmt (List.nth stmts (v.number - 1))) ^ "\n");
      debug_print ("\tuse: " ^ (string_of_set v.use) ^ "\n");
      debug_print ("\tdef: " ^ (string_of_set v.def) ^ "\n");
      debug_print ("\tin: " ^ (string_of_set v.in_var) ^ "\n");
      debug_print ("\tout: " ^ (string_of_set v.out_var) ^ "\n");
    ) n.subnodes;
  ) g

let collect_use stmt =
  let helper_idc3 v =
    match v with
    | Var3 id -> Id3Set.singleton id
    | _ -> Id3Set.empty
  in let helper expr =
    match expr with
    | BinaryExp3 (_, p1, p2) -> Id3Set.union (helper_idc3 p1) (helper_idc3 p2)
    | UnaryExp3 (_, p) -> helper_idc3 p
    | FieldAccess3 (id, _) -> Id3Set.singleton id
    | MdCall3 (_, args) ->
      (List.fold_left Id3Set.union Id3Set.empty (List.map helper_idc3 args))
    | ObjectCreate3 _ -> Id3Set.empty
    | Idc3Expr v -> helper_idc3 v
  in match stmt with
  | PrintStmt3 e -> helper_idc3 e
  | IfStmt3 (e, _) -> helper e
  | MdCallStmt3 e -> helper e
  | ReturnStmt3 id -> Id3Set.singleton id
  | AssignStmt3 (_, expr) -> helper expr
  | AssignFieldStmt3 (e1, e2) -> Id3Set.union (helper e1) (helper e2)
  | Label3 _ | GoTo3 _ | ReturnVoidStmt3 -> Id3Set.empty
  (* | AssignDeclStmt3 _ | ReadStmt3 _ *)
  | _ -> failwith ("Unsupported statement: " ^ (string_of_ir3_stmt stmt))

let collect_def stmt =
  match stmt with
  | AssignStmt3 (id, _) -> Id3Set.singleton id
  | _ -> Id3Set.empty

let iter_live_var nodes =
  let find_node_by_block block_ref =
    List.find (fun n -> !(n.block) == !block_ref) nodes
  in let iter_subnode i v node =
    let succ =
      if i == (List.length node.subnodes) - 1 then
        (* succ come from succ blocks *)
        (List.map (fun n -> List.hd n.subnodes)
          (List.map find_node_by_block !(node.block).succ))
      else [List.nth node.subnodes (i + 1)]
    in {
      number = v.number;
      use = v.use;
      def = v.def;
      in_var = Id3Set.union v.use (Id3Set.diff v.out_var v.def);
      out_var =
        List.fold_left Id3Set.union Id3Set.empty
          (List.map (fun u -> u.in_var) succ)
    }
  in List.map (fun node ->
    {
      block = node.block;
      subnodes = List.mapi (fun i v -> iter_subnode i v node) node.subnodes;
    }
  ) nodes

let equal_nodes x y =
  !(x.block) == !(y.block) && x.subnodes = y.subnodes

(* live_var_node list in code order *)
let analyze_live_var blocks =
  let init block =
    {
      block = ref block;
      subnodes = List.mapi (fun i s -> {
        number = i + block.beg;
        use = collect_use s;
        def = collect_def s;
        in_var = Id3Set.empty;
        out_var = Id3Set.empty;
      }) block.stmts;
    }
  in let rec helper nodes =
    let new_nodes = iter_live_var nodes in
    if List.exists2 (fun a b -> not (equal_nodes a b)) new_nodes nodes then
      helper new_nodes
    else nodes
  in helper (List.map init blocks)
