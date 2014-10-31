
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(* 			  TypeChecking of Jlite programs 			 *)
(* ===================================================== *)

open Jlite_structs

(* Compare two types *) 	
let compare_jlite_types 
	(t1:jlite_type) (t2:jlite_type) = 
	match t1,t2 with 
	| (ObjectT name1), (ObjectT "null") -> true
	| (ObjectT name1), (ObjectT name2) -> 
		((String.compare name1 name2) == 0) 
	| t1, t2 -> t1 == t2
	
(* Compare two variable ids *) 	
let compare_var_ids v1 v2 =
	match v1,v2 with
	| SimpleVarId id1, SimpleVarId id2 -> 
		((String.compare id1 id2) == 0)
	| SimpleVarId id1, TypedVarId (id2,t,s) -> 
		((String.compare id1 id2) == 0)	
	| TypedVarId (id1,t,s), SimpleVarId id2 -> 
		((String.compare id1 id2) == 0)		
	| TypedVarId (id1,t1,s1), TypedVarId (id2,t2 ,s2) ->
		((String.compare id1 id2) == 0) && (s1 == s2)
		
(* Find the declared type of a variable *) 		
let rec find_var_decl_type 
	(vlst: var_decl list) (vid:var_id) =
  match vlst with
    | [] -> (Unknown, SimpleVarId "") 
    | (t,v)::tail_lst -> 
		if (compare_var_ids v vid) 
		then (t,v) 
		else (find_var_decl_type tail_lst vid)

(* Check if a variable id exists *) 		
let exists_var_id 
	(vlst: var_decl list) (vid: var_id) : bool =
	let is_eq_id ((t,v): var_decl):bool =
		(compare_var_ids v vid) 
	in (List.exists is_eq_id vlst) 	

(* Check if a list of variable declarations has duplicates *) 	
let rec find_duplicate_var_decl 
	(vlst: var_decl list) =
  match vlst with
    | [] -> None
    | (typ,vid)::tail_lst -> 
		if exists_var_id tail_lst vid
		then Some vid
		else (find_duplicate_var_decl tail_lst)		

(* Check if the declaration of a class exists *) 			  
let exists_class_decl 
	((cm,clst): jlite_program) (cid:class_name) =
	let rec helper clst =
		match clst with
		| [] -> false 
		| (cname,cvars,cmthd)::tail_lst -> 
			if ((String.compare cname cid) = 0) 
				then true
				else ( helper tail_lst)
	in ( helper clst) 

(* Find the declaration of a class *) 
let find_class_decl 
	((cm,clst): jlite_program) (cid:class_name) =
	let rec helper clst =
		match clst with
		| [] -> None
		| (cname,cvars,cmthd)::tail_lst -> 
			if ((String.compare cname cid) = 0) 
				then (Some (cname,cvars,cmthd)) 
				else ( helper tail_lst)
	in ( helper clst) 

(* Compare a declared variable type against a given type *)
let compare_param_decl_type 
	((vt,vid):var_decl) (t:jlite_type) =
	(compare_jlite_types vt t)

(* Check if a method exists *) 
let find_method_decl_type 
	(p: jlite_program) 
	(calleecls:class_name) 
	(calleeid:var_id) 
	(calleetypes: jlite_type list) =
	match (find_class_decl p calleecls) with
	| None -> failwith "Cannot Find method"
	| Some (cname,cvars,cmthd) -> 
		let rec helper mthdlst =
		match mthdlst with
		| [] -> failwith 
			("Cannot find method:" 
			^ string_of_var_id calleeid 
			^ " in class:" ^ calleecls ) 
		| mthd::tail_lst -> 
			if (compare_var_ids mthd.jliteid calleeid) 
			then if ((List.length mthd.params) 
					  == (List.length calleetypes)) 
				then if (List.for_all2 
						compare_param_decl_type 
						mthd.params calleetypes
						) 
					then (mthd.ir3id,mthd.rettype)
					else ( helper tail_lst)
				else ( helper tail_lst)
			else ( helper tail_lst)
		in ( helper cmthd)
	
(* Check if a field exist *)	
let find_field 
	(p: jlite_program) 
	(cls:class_name) 
	(fieldid:var_id) =
	match (find_class_decl p cls) with
	| None -> Unknown
	| Some (cname,cvars,cmthd) -> 
		let rec  helper flst :jlite_type =
			match flst with
			| [] -> Unknown 
			| (ftype,fid)::tail_lst -> 
				if (compare_var_ids fid fieldid) 
				then ftype
				else ( helper tail_lst)
		in helper cvars

(* Annotate a list of variable declarations with their scope *)	
let rec create_scoped_var_decls
	(vlst: var_decl list) (scope:int) =
	let helper ((vt,vid):var_decl) =
		match vid with
		| SimpleVarId id -> 
			(vt, TypedVarId (id, vt, scope))
		| TypedVarId (id,t,s) -> 
			(vt,TypedVarId (id, vt, scope))
	in (List.map helper vlst)

  
(* Type check a list of variable declarations 
  1) Determine if all object types exist
  2) Find and return duplicate variable names	
*)  
let rec type_check_var_decl_list
	(p: jlite_program) 
	(vlst: var_decl list) =
	let rec helper 
		(vlst: var_decl list) :jlite_type list =
		match vlst with
		| [] -> []
		| (typ,vid)::tail_lst -> 
			match typ with
			| ObjectT cname -> 
				if (exists_class_decl p cname) 
					then ( helper tail_lst) 
					else typ::( helper tail_lst) 
			| _ -> ( helper tail_lst) 
	in match ( helper vlst) with
		| [] -> 
			begin
			match (find_duplicate_var_decl vlst) with
			| Some a -> 
				(false,("Duplicate variable name:" 
						^ string_of_var_id a))
			| None -> (true,"")
			end
		| lst -> (false, ("Undefined types: " 
				^ (string_of_list lst string_of_jlite_type ",")))

 
(* Compare the parameter type signatures of two methods *)
let equal_md_param_signatures 
	(md1lst:var_decl list) (md2lst:var_decl list) =
	if ((List.length md1lst) == (List.length md2lst)) 
	then (List.for_all2 
			(fun (t1,v1) (t2,v2) -> compare_jlite_types t1 t2) 
			md1lst md2lst
		 )
	else false
	
(* Type check a list of method declarations 
  1) Determine if there is illegal overloading
  2) Find and return overloaded method names	
*)  
let rec type_check_md_decl_list
	(classid: class_name)
	(mdlst: md_decl list) =
	let rec helper 
		(mlst: md_decl list) 
		(count: int) =
		match mlst with
		| [] -> []
		| md::tail_lst -> 
			md.ir3id <- SimpleVarId (classid ^ "_" ^ string_of_int count);
			if (List.for_all
				(fun md1 -> 
					not ((compare_var_ids md.jliteid md1.jliteid) && 
						 (equal_md_param_signatures 
							md.params md1.params)
						)
				) tail_lst) 
				then helper tail_lst (count +1)
				else md.jliteid::helper tail_lst (count +1)
	in match ( helper mdlst 0) with
		| [] -> (true,"")
		| lst -> (false, (" Overloaded method names: " 
				^ (string_of_list lst string_of_var_id ",")))
				
(* Type check an expression *)
(* Return the type of the Expression and a new TypedExpession *)  
let rec type_check_expr 
	(p: jlite_program)(env: var_decl list) 
	(classid: class_name) (exp:jlite_exp) = 
	let rec helper e 
	: (jlite_type * jlite_exp) =
		match e with
		| BoolLiteral v -> (BoolT, e)
		| IntLiteral v -> (IntT, e)
		| StringLiteral v -> (StringT, e)
		| ThisWord -> 
			((ObjectT classid), TypedExp (e,(ObjectT classid)))
		| NullWord -> ((ObjectT "null") , TypedExp (e,(ObjectT "null")))
		| Var v -> 
			let (vtyp,vid) =(find_var_decl_type env v) in
			(vtyp, TypedExp (Var vid,vtyp)) 
		| UnaryExp (op,arg) -> 
			let (argtype,argTypedExp) = helper arg in
			begin
			match op,argtype with
			| UnaryOp "-",IntT 
			  -> (IntT, 
				  TypedExp(UnaryExp (op, argTypedExp), IntT))
			| UnaryOp "!",BoolT 
			  -> (BoolT, 
				  TypedExp(UnaryExp (op, argTypedExp), BoolT))
			| _, _ -> (Unknown,e)
			end
		| BinaryExp (op,arg1,arg2) -> 
			let (arg1type, arg1TypedExp)  = helper arg1 in
			let (arg2type, arg2TypedExp) = helper arg2 in
			begin
			match op,arg1type,arg2type with 
			| AritmeticOp opid,IntT,IntT 
			  -> (IntT, TypedExp(
					BinaryExp (op, arg1TypedExp, arg2TypedExp), IntT))
			| RelationalOp opid,IntT,IntT 
			  -> (BoolT, TypedExp(
				  BinaryExp (op, arg1TypedExp, arg2TypedExp), BoolT))
			| RelationalOp "==",ObjectT t1,ObjectT "null" 
			| RelationalOp "==",ObjectT "null", ObjectT t1
			| RelationalOp "!=",ObjectT t1,ObjectT "null" 
			| RelationalOp "!=",ObjectT "null", ObjectT t1
			  -> (BoolT, TypedExp(
				  BinaryExp (op, arg1TypedExp, arg2TypedExp), BoolT))
			| BooleanOp opid,BoolT,BoolT 
			  -> (BoolT, TypedExp(
				  BinaryExp (op, arg1TypedExp, arg2TypedExp), BoolT))
			| _,_,_-> (Unknown, TypedExp(
					BinaryExp (op, arg1TypedExp, arg2TypedExp), Unknown))
			end
		| FieldAccess (e,id) -> 
			let (objtype, objTypedExp) = helper e in
			begin
			match objtype with
			| ObjectT cname -> 
				let typ = (find_field p cname id) in 
				(typ, TypedExp 
					(FieldAccess(objTypedExp,id), typ)) 
			| _ -> (Unknown, TypedExp(
					FieldAccess (objTypedExp,id), Unknown))
			end
		| ObjectCreate c -> 
			if (exists_class_decl p c) 
			then ((ObjectT c), TypedExp(e,(ObjectT c)))
			else (Unknown, e)
		| MdCall (e,args) -> 
			(type_check_method_call p env classid (e,args)) 
		| _ -> (Unknown, e)
	  in  helper exp

(* Type check a method call expression *)	  
and type_check_method_call 
	(p: jlite_program)(env: var_decl list) 
	(classid: class_name) 
	((calleeexp,calleeparams)
	:jlite_exp * (jlite_exp list)) = 
	let (calleeid, calleecls, caleeTypedExp) =
    	match calleeexp with
		| Var v -> (v, classid, calleeexp)
		| FieldAccess (e,id) -> 
			let (typ, objTypedExp) = 
				(type_check_expr p env classid e) in
			begin
			match typ with
			| ObjectT c-> 
				(id, c, (FieldAccess(objTypedExp,id)))
			| _ -> failwith 
				("Type-check error for method call. Not an object type:" 
				^ (string_of_jlite_expr e))
			end
		| _ -> failwith "Type-check error for method call"
		in
	let rec helper explst =
		match explst with
		| [] -> []
		| exp::tail_lst -> 
			(type_check_expr p env classid exp) :: helper tail_lst in
	let (argtypes,argExpr) = 
		List.split( helper calleeparams) in 
	let (ir3id,rettype) =
		(find_method_decl_type 
			p calleecls calleeid argtypes
		) in
	let caleeRenamedExp = match caleeTypedExp with
		| Var v -> Var ir3id
		| FieldAccess (e,id) -> FieldAccess (e,ir3id)	
		| _  -> failwith "Error in method call"		
	in(rettype, TypedExp(
			MdCall(caleeRenamedExp,argExpr),rettype))
			
(* Type check a list of statements and determine the return type.
   Exceptions are thrown when a statement does not type check 
   and when dead code is found
*)  
let rec type_check_stmts 
	(p: jlite_program)(env: var_decl list) 
	(classid: class_name) 
	(mthd: md_decl) 
	(stmtlst:jlite_stmt list)
	(rettype: jlite_type option) 
	: (jlite_type option *(jlite_stmt list))  =
	match stmtlst with
	| [] -> (rettype,[])
	| s::tail_lst -> 
		let rec helper s 
		: (jlite_stmt * jlite_type option) =
		match s with
		| IfStmt (e, stmts1, stmts2) -> 
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			let (then_branch,thenlist) = 
			 (type_check_stmts p env classid mthd stmts1 rettype) in 
			let (else_branch,elselist) = 
			 (type_check_stmts p env classid mthd stmts2 rettype) in
			begin
			match expr_type with
			| BoolT -> 
				begin
				match then_branch, else_branch with
				| Some t1, Some t2 -> 
					if (t1!= t2) 
					then failwith 
						("\nType-check error in " 
						^ classid ^ "." ^ string_of_var_id mthd.jliteid 
						^ ". If statement returns different types on branches:\n" 
						^ string_of_jlite_stmt s ^ "\n")
					else (IfStmt (exprnew, thenlist,elselist),Some t1)
				| _, _ -> (IfStmt (exprnew, thenlist,elselist), None)
				end
			| _ -> failwith 
					("\nType-check error in " 
					^ classid ^ "." ^ string_of_var_id mthd.jliteid 
					^ ". If expression is not of type boolean:\n" 
					^ string_of_jlite_expr e ^ "\n")
			end
		| WhileStmt (e, stmts) -> 
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			let (rettype, stmtsnew) = 
			 (type_check_stmts p env classid mthd stmts rettype) in 
			begin
			match expr_type with
			| BoolT -> (WhileStmt (exprnew, stmtsnew), rettype)
			| _ -> failwith 
					("\nType-check error in " 
					^ classid ^ "." ^ string_of_var_id mthd.jliteid 
					^ ". While expression is not of type boolean:\n" 
					^ string_of_jlite_expr e ^ "\n")
			end
		| ReturnStmt e ->  
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			begin
			match expr_type with
			| Unknown -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Return expression fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  (ReturnStmt exprnew, Some expr_type)
			end
		| ReturnVoidStmt ->  
			(ReturnVoidStmt, Some VoidT)
		| AssignStmt (id,e) ->  
			let (exprtype,exprnew) = 
			 (type_check_expr p env classid e) in
			let (idtype,scopedid)= (find_var_decl_type env id) in 
			if (compare_jlite_types idtype exprtype)
				then (AssignStmt(scopedid,exprnew),None)
				else failwith 
					("\nType-check error in " 
					^ classid ^ "." ^ string_of_var_id mthd.jliteid 
					^ ". Assignment statement failskkk:\n" 
					^ string_of_jlite_stmt s ^ string_of_jlite_type exprtype ^ "\n")
		| ReadStmt id -> 
			let (idtype,scopedid) = (find_var_decl_type env id) in
			begin
			match idtype with
			| ObjectT _ | Unknown  -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Read statement fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  (ReadStmt scopedid,None)
			end
		| PrintStmt e -> 
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			begin
			match expr_type with
			| Unknown | ObjectT _ -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Statement fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  (PrintStmt exprnew, None)
			end
		|  MdCallStmt (e) -> 
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			begin
			match expr_type with
			| Unknown -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Statement fails:\n" 
				^ string_of_jlite_expr exprnew ^ "\n")
			| _ ->  (MdCallStmt exprnew, None)
			end
		| AssignFieldStmt (id,e) ->  
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			let (id_type,idnew) = 
			 (type_check_expr p env classid id) in 
			if (expr_type == id_type)
				then (AssignFieldStmt(idnew,exprnew),None)
				else failwith 
					("\nType-check error in " ^ classid ^ "." 
					^ string_of_var_id mthd.jliteid 
					^ ". Field assignment statement fails:\n" 
					^ string_of_jlite_stmt s ^ "\n")					
	  in let (newstmt,newrettype) = ( helper s) in
	  match newrettype,tail_lst with
		| Some t, head::tail -> 
			failwith 
			("\nType-check error in " ^ classid ^ "." 
			 ^ string_of_var_id mthd.jliteid 
			 ^ ". Dead Code:\n" 
			 ^ (string_of_list tail_lst string_of_jlite_stmt "\n" ^ "\n")) 
		| _,_ ->  
			let (rettype,stmts) = 
				(type_check_stmts p env classid mthd tail_lst newrettype) in
				(rettype,(newstmt::stmts))
  
(* TypeCheck a JLite Method Declaration *)
let type_check_mthd_decl p env cname m : md_decl = 
	let mthdenv = 
		List.append m.params m.localvars in 
	let (retval, errmsg) = 
		(type_check_var_decl_list p mthdenv)
	in if (retval == false) 
		then failwith 
		 ("\nType-check error in " ^ cname ^ "." 
		  ^ string_of_var_id m.jliteid 
		  ^ " parameter or local variables declarations.\n"
		  ^ errmsg ^ "\n")
		else
		let scopedEnv = List.append 
				(create_scoped_var_decls mthdenv 2) env in 
		(* TypeCheck the body of the method *)
			let (rettyp,newstmts) = 
				(type_check_stmts p scopedEnv cname m m.stmts None) in
		(* TypeCheck the return type of the method *)
			let _ = match rettyp,m.rettype with
			| None, VoidT -> true
			| Some VoidT, VoidT -> true
			| None, t -> 
				failwith 
				("\nType-check error in " ^ cname ^ "." 
				^ string_of_var_id m.jliteid 
				^ ". This method must return a result of type "
				^ string_of_jlite_type m.rettype ^ ". \n")
			| Some (ObjectT "null"), (ObjectT t2) -> 
				true
			| Some (ObjectT t1), (ObjectT t2) -> 
				if ((String.compare t1 t2) != 0) 
				then failwith 
					("\nType-check error in " ^ cname ^ "." 
					^ string_of_var_id m.jliteid 
					^ ". Type mismatch. Return type of method " 
					^ "is different from declared type "
					^ string_of_jlite_type m.rettype ^ t1 ^ ". \n")
				else true
			| Some t1, t2 -> 
				if (t1!= t2) 
				then failwith 
					("\nType-check error in " ^ cname ^ "." 
					^ string_of_var_id m.jliteid 
					^ ". Type mismatch. Return type of method "
					^ "is different from declared type "
					^ string_of_jlite_type m.rettype 
					^ string_of_jlite_type t1 ^ ". \n")
				else true
			in { m with stmts=newstmts;
				}

(* TypeCheck a JLite Program. 
   Return a new JLite Program where expressions are annotated with types
*)
let type_check_jlite_program  
	(p:jlite_program) : jlite_program=
	let type_check_class_main 
		((cname,mmthd):class_main ) =
		(cname,(type_check_mthd_decl p [] cname mmthd )) in
	let rec type_check_class_decl 
		((cname,cvars,cmthds):class_decl) =
		(* TypeCheck field declarations *)
		let (retval, errmsg) = 
			(type_check_var_decl_list p cvars) in
		if (retval==false) then 
			failwith 
			("\nType-check error in " ^ cname 
			^ " field declarations." ^ errmsg ^ "\n")
			(* TypeCheck method declarations *)
			else let env = (create_scoped_var_decls cvars 1) in
				let rec helper mthdlst =
					match mthdlst with 
					| [] -> []
					| m::tail_rest -> 
						(type_check_mthd_decl p env cname m)
							::( helper tail_rest)
				in (cname,cvars,( helper cmthds))
	in 
	begin
		let (mainclass, classes) = p in 
		(* TypeCheck methods overloading *)
		List.iter (fun (cname,cvars,cmthds) -> 
		let (retval, errmsg) = 
			(type_check_md_decl_list cname cmthds) in
			if (retval==false) then 
				failwith 
				("\nType-check error in " ^ cname 
				^ " method declarations." ^ errmsg ^ "\n")
			else ()
		) classes;
		let newmain =(type_check_class_main mainclass) in
		let newclasses=(List.map type_check_class_decl classes) in
		(newmain, newclasses)
	end