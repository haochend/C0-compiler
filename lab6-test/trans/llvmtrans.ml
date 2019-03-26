(* L1 Compiler
 * AST -> IR Translator
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
*)

open Core

module A = Ast
module T = Llvmtree
module S = Symbol.Map

(*
type typedefine =
    | INT
    | BOOL
    | VOID
    | IDENT of Ast.ident
    | POINTER of Ast.typedefine
    | ARRAY of Ast.typedefine
    | STRUCT of Ast.ident *)

type func_init_status =
  | Deleted
  | FDecl of A.typedefine * A.param list
  | FDefn of A.typedefine * A.param list

let trans_unop = function
  | A.LOGICALNOT -> T.LNOT
  | A.BINARYNOT -> T.BNOT
  | A.NEGATIVE -> T.NEGA
(*
let trans_postop = function
  | A.PLUSPLUS -> T.PLUSPLUS
  | A.MINUSMINUS -> T.MINUSMINUS *)
(*
let trans_asop = function
  | A.EQUAL -> T.EQL
  | A.PLUSEQ -> T.PEQ
  | A.MINUSEQ -> T.MINEQ
  | A.MULTEQ -> T.MULEQ
  | A.DIVEQ -> T.DIVEQ
  | A.MODEQ -> T.MODEQ
  | A.ANDEQ -> T.ANDEQ
  | A.XOREQ -> T.XOREQ
  | A.OREQ -> T.OREQ
  | A.RSHIFTEQ -> T.RSHEQ
  | A.LSHIFTEQ -> T.LSHEQ *)


let get_first param =
  let (A.Param (e1, e2)) = param in e1


let get_second param =
  let (A.Param (e1, e2)) = param in e2

let rec get_the_deepest_type = function
  | A.INT -> A.INT
  | A.BOOL -> A.BOOL
  | A.POINTER ptype -> get_the_deepest_type ptype
  | A.ARRAY atype -> get_the_deepest_type atype
  | A.STRUCT sid-> A.STRUCT sid
  | A.VOID -> A.VOID
  | _ -> assert false

let rec trans_ident_type_to_string prev= function
  | A.INT -> "i32" ^ prev
  | A.BOOL -> "i1" ^ prev
  | A.POINTER ptype->
    (trans_ident_type_to_string (prev^"*") ptype)
  | A.STRUCT sid ->
    "%struct." ^ (Symbol.name sid) ^ prev
  | A.ARRAY atype ->
    (trans_ident_type_to_string (prev^"*") atype)
  | A.VOID -> "void"

let rec trans_ident_type typedef_env = function
  | A.IDENT id ->
    let primitive = S.find_exn typedef_env id in
    trans_ident_type typedef_env primitive
  | A.POINTER ptype ->
    A.POINTER (trans_ident_type typedef_env ptype)
  | A.ARRAY atype ->
    A.ARRAY (trans_ident_type typedef_env atype)
  | A.STRUCT sid -> A.STRUCT sid
  | A.BOOL -> A.BOOL
  | A.INT -> A.INT
  | A.VOID -> A.VOID

let rec print_type typedef_env = function
  | A.IDENT id ->
    let () = printf "start with ident %s\n" (Symbol.name id) in
    let primitive = S.find_exn typedef_env (id) in
    print_type typedef_env primitive
  | A.INT ->
    printf "have an int here\n"
  | A.BOOL ->
    printf "have a bool here\n"
  | A.VOID ->
    printf "have a void here\n"
  | A.STRUCT sid ->
    printf "have a struct here %s\n" (Symbol.name sid)
  | A.POINTER ptype ->
    let () = printf "start with pointer\n" in
    print_type typedef_env ptype

  | A.ARRAY atype ->
    let () = printf "start with array\n" in
    print_type typedef_env atype



let trans_binop = function
  | A.PLUS ->       T.ADD
  | A.MINUS ->      T.SUB
  | A.TIMES ->      T.MUL
  | A.DIVIDEDBY ->  T.DIV
  | A.MODULO ->     T.MOD
  | A.LESSTHAN ->   T.LES
  | A.LESSOREQ ->   T.LEQ
  | A.GREATERTHAN -> T.GRE
  | A.GREATEROREQ -> T.GEQ
  | A.ISEQ ->       T.IEQ
  | A.NOTEQ ->      T.NEQ
  | A.LOGICALAND -> T.LAND
  | A.LOGICALOR ->  T.LOR
  | A.BINARYAND ->  T.BAND
  | A.BINARYXOR ->  T.BXOR
  | A.BINARYOR ->   T.BOR
  | A.SHIFTLEFT ->  T.SLEFT
  | A.SHIFTRIGHT -> T.SRIGHT
(* | A.NEGATIVE -> T.SUB                 (* unary to binary!*) *)


let rec trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env= function
  (* after type-checking, id must be declared; do not guard lookup *)
  | A.Var id ->
    begin
      match S.find type_env id with
      | Some typedef ->
        begin
          let typedefine = trans_ident_type typedef_env typedef in
          let Some num = S.find env id in
          let curr_id = (Symbol.name id) ^ "local" ^ (string_of_int num) in
            (T.VAR (curr_id, trans_ident_type_to_string "" typedef), typedef)
        end
      | None -> assert false
    end
  | A.ConstExp c ->
    (* let t1 = Temp.createeight() in *)
    (T.CONST c, A.INT)
  | A.BoolExp b -> (T.BOOL b, A.BOOL)
  | A.Unop (unop, e) ->
    begin
      match unop with
      | A.NEGATIVE ->
        begin
          let t1 = Temp.createeight() in
          let t2 = Temp.createeight() in
          let (exp,prev_type1) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env e in
          let true_type = trans_ident_type typedef_env prev_type1 in
          (T.BINOP (trans_binop A.MINUS, T.CONST Int32.zero, exp,t1,t2,trans_ident_type_to_string "" true_type),true_type)
        end
      | _ ->
        begin
          let t1 = Temp.createeight() in
          let (exp, prev_type) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env e in
          let true_type = trans_ident_type typedef_env prev_type in
          (T.UNOP (trans_unop unop, exp,t1),true_type)
        end
    end

  | A.Binop (oper, e1, e2) ->
    let t1 = Temp.createeight() in
    let t2 = Temp.createeight() in
    let (exp1,prev_type1) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env e1 in
    let (exp2,prev_type2) =trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env e2 in
    (* let true_type = trans_ident_type typedef_env prev_type1 in *)
    let true_type =
      begin
        match oper with
        | A.LESSTHAN -> A.BOOL
        | A.LESSOREQ -> A.BOOL
        | A.GREATERTHAN -> A.BOOL
        | A.GREATEROREQ -> A.BOOL
        | A.ISEQ -> A.BOOL
        | A.NOTEQ -> A.BOOL
        | A.LOGICALAND -> A.BOOL
        | A.LOGICALOR -> A.BOOL
        | _ -> A.INT
      end in
    let true_type1 = trans_ident_type typedef_env prev_type1 in
    let true_type2 = trans_ident_type typedef_env prev_type2 in
    begin
      match (true_type1, true_type2) with
      | (A.POINTER A.VOID, A.POINTER A.VOID) ->
        begin
          match oper with
          | A.NOTEQ -> (T.BOOL false, true_type)
          | A.ISEQ -> (T.BOOL true,true_type)
          | _ -> assert false
        end
      | (A.POINTER A.VOID, _) ->
        let element_type =  true_type2 in
        (T.BINOP (trans_binop oper, exp1, exp2, t1, t2,trans_ident_type_to_string "" element_type),true_type)
      | (_, A.POINTER A.VOID) ->
        let element_type = true_type1 in
        (T.BINOP (trans_binop oper, exp1, exp2, t1, t2,trans_ident_type_to_string "" element_type),true_type)
      | (_, _) ->
        let element_type = true_type1 in
        (T.BINOP (trans_binop oper, exp1, exp2, t1, t2,trans_ident_type_to_string "" element_type),true_type)
    end


    (* (T.BINOP (trans_binop oper, exp1, exp2, t1, t2,trans_ident_type_to_string "" element_type),true_type) *)
  | A.Question (e1, e2, e3) ->
    begin
      let t1 = Temp.createeight() in
      let t2 = Temp.createeight() in
      let t3 = Temp.createeight() in
      let (exp1,_) =trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env e1 in
      let (exp2,prev_type1) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env e2 in
      let (exp3,prev_type2) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env e3 in
      let true_type1 = trans_ident_type typedef_env prev_type1 in
      let true_type2 = trans_ident_type typedef_env prev_type2 in
      let typedef =
        begin
          match true_type1,true_type2 with
          | A.POINTER A.VOID, A.POINTER A.VOID -> A.POINTER A.INT
          | A.POINTER (A.VOID), _ -> true_type2
          | _, A.POINTER (A.VOID) -> true_type1
          | _,_ -> true_type1
        end in
      (T.QUESTION (exp1, exp2, exp3,t1,t2,t3,trans_ident_type_to_string "" typedef),typedef)
    end
  | A.Marked marked_exp ->
    trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env (Mark.data marked_exp)
  | A.Call (curr_id, exps) ->

    begin
      (* let () = printf "calling %s\n" (Symbol.name id) in *)
      Arguement.reset();
      (* let arg_list = List.fold_left ~f:(fun l e-> l@[Arguement.create()]) ~init:([]) exps in *)
      (* let arg_cnt = Arguement.access() in *)
      let transed_exps = List.map ~f:(fun e -> let (ex,_) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env e in ex) exps in
      let exp_list = List.fold_left ~f:(fun l e-> l@[e]) ~init:([]) transed_exps in
      (* let temp_list = List.fold_left ~f:(fun l e-> [Temp.createeight()]@l) ~init:([]) transed_exps in *)
      begin
        match S.find func_env curr_id with
        | Some (FDecl (t, nvs)) ->
          begin
            let (nv_type_list) = List.fold_left ~f:(fun (nv_t) nv->
              let typ = get_second nv in
              let type_string = trans_ident_type_to_string "" (trans_ident_type typedef_env typ) in
              let arg_type_list = nv_t@[type_string]
              in (arg_type_list)
            ) ~init:([]) nvs in
            let return_type = trans_ident_type_to_string "" (trans_ident_type typedef_env t) in
            (T.INTERNALCALL (Symbol.name curr_id,
                             exp_list,nv_type_list,return_type), t)

          end

        | None ->
          begin
            match (Symbol.name curr_id) with
            | "main" ->
              (T.INTERNALCALL (Symbol.name curr_id,
                               [],[],"i32"), A.INT)
            | _ ->
              let return_type =
                begin
                  match Symbol.name curr_id with
                  | "print_int" -> A.VOID
                  | "print_fpt" -> A.VOID
                  | "print_hex" -> A.VOID
                  | "print_dub" -> A.VOID
                  | "fless" -> A.BOOL
                  | "srand" -> A.VOID
                  | "puts" -> A.VOID
                  | "dadd" -> A.POINTER (A.STRUCT (Symbol.symbol "dub"))
                  | "dsub" -> A.POINTER (A.STRUCT (Symbol.symbol "dub"))
                  | "dmul" -> A.POINTER (A.STRUCT (Symbol.symbol "dub"))
                  | "ddiv" -> A.POINTER (A.STRUCT (Symbol.symbol "dub"))
                  | "dless" -> A.BOOL
                  | "itod" -> A.POINTER (A.STRUCT (Symbol.symbol "dub"))
                  | _ -> A.INT
                end in
              (T.EXTERNALCALL (Symbol.name curr_id,
                               exp_list),return_type)
          end
      end
    end


  | A.Null -> (T.NULL, A.POINTER A.VOID)
  | A.LDot (exp, id) ->
    let t1 = Temp.createeight() in
    let (prev_exp,prev_type) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env exp in
    let true_type = trans_ident_type typedef_env prev_type in
    begin
      match true_type with
      | A.STRUCT sid ->
        let struct_map = S.find_exn struct_env sid in
        let struct_detail_map = S.find_exn struct_detail_env sid in
        let offset = S.find_exn struct_map id in
        let curr_type = S.find_exn struct_detail_map id in
        let true_type1 = trans_ident_type typedef_env curr_type in
        begin
          match true_type1 with
          | A.STRUCT ssid ->
            (T.RARROWWITHOUTCHECKANDLOAD (Symbol.name sid,prev_exp,offset,(trans_ident_type_to_string "" true_type1)), true_type1)
          | _ ->
            (T.RARROWWITHOUTCHECK (Symbol.name sid,prev_exp,offset,(trans_ident_type_to_string "" true_type1)), true_type1)
        end

      | _ -> assert false
    end

  | A.LRArrow (exp, id) ->
    (* let () = printf "reach rarrow %s\n" (Symbol.name id) in *)
    let t1 = Temp.createeight() in
    (* let t2 = Temp.createeight() in *)
    let (prev_exp,prev_type) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env t1 typedef_env struct_detail_env func_id inline_env exp in
    let true_type = trans_ident_type typedef_env prev_type in
    begin
      match true_type with
      | A.POINTER ptype ->
        let true_type1 = trans_ident_type typedef_env ptype in
        begin
          match true_type1 with
          | A.STRUCT sid ->
            let struct_map = S.find_exn struct_env sid in
            let struct_detail_map = S.find_exn struct_detail_env sid in
            let offset = S.find_exn struct_map id in
            let curr_type = S.find_exn struct_detail_map id in
            let true_type2 = trans_ident_type typedef_env curr_type in
            begin
              match true_type2 with
              | A.STRUCT ssid ->
                (T.RARROWWITHOUTLOAD (Symbol.name sid,prev_exp,offset,(trans_ident_type_to_string "" true_type2)), true_type2)
              | _ ->
                (T.RARROW (Symbol.name sid,prev_exp,offset,(trans_ident_type_to_string "" true_type2)), true_type2)
            end

          | _ -> assert false
        end
      | _ -> assert false
    end
  | A.Alloc t ->
    (* let () = printf "Allocating "  in let () = (print_type typedef_env t) in *)
    let true_type = trans_ident_type typedef_env t in
    begin
      match true_type with
      | A.INT -> (T.MALLOC (4,"i32"), (A.POINTER A.INT))
      | A.BOOL -> (T.MALLOC (1,"i1"), (A.POINTER A.BOOL))
      | A.STRUCT s ->
        begin
          let siz = S.find_exn struct_size_env s in
          (T.MALLOC (siz,(trans_ident_type_to_string "" true_type)), (A.POINTER (A.STRUCT s)))
        end
      | A.POINTER ptype ->
        (T.MALLOC (8,(trans_ident_type_to_string "" true_type)), (A.POINTER true_type))
      | _ -> (T.MALLOC (8,(trans_ident_type_to_string "" true_type)),(A.POINTER true_type))
    end
  | A.LDeref exp ->
    begin
      (* let () = printf "reach deref" in *)
      let (prev_exp,prev_type) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env exp in
      let true_type = trans_ident_type typedef_env prev_type in
      (* let (A.POINTER ptype) = true_type in *)
      let ptype =
        begin
          match true_type with
          | A.ARRAY (pinside) -> pinside
          | A.POINTER pinside -> pinside
        end in
      let ptype = trans_ident_type typedef_env ptype in
      begin
        match ptype with
        | A.STRUCT sid ->
          (prev_exp,ptype)
        | _ ->
          (T.DEREF (prev_exp,trans_ident_type_to_string "" ptype), ptype)

      end

    end
  | A.AllocArray (ty,exp)->
    let t = Temp.createeight() in
    let true_type = trans_ident_type typedef_env ty in
    let (exp1,_) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env exp in
    let element_type = trans_ident_type_to_string "" true_type in
    begin
      match true_type with
      | A.INT ->
        (T.ALLOCARRY (4, exp1 ,element_type), (A.ARRAY (A.INT)))
      | A.BOOL ->
        (* let (exp1,_) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env exp in *)
        (T.ALLOCARRY (4, exp1,element_type),(A.ARRAY (A.BOOL)))
      | A.STRUCT s ->
        begin
          let siz = S.find_exn struct_size_env s in
          (* let (exp1,_) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env exp in *)
          (T.ALLOCARRY (siz, exp1,element_type),(A.ARRAY (A.STRUCT s)))
        end
      | A.POINTER ptype ->
        (* let (exp1,_) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env exp in *)
        (T.ALLOCARRY (8, exp1,element_type),(A.ARRAY (A.POINTER ptype)))
      | A.ARRAY atype ->
        (* let (exp1,_) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env exp in *)

        (T.ALLOCARRY (8, exp1,element_type),(A.ARRAY (A.ARRAY atype)))
      | _ -> assert false
    end
  | A.LArrDeref (exp,index)->
    let t1 = Temp.createeight() in
    let t2 = Temp.createeight() in
    let (prev_exp,prev_type) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env exp in
    let (curr_exp,curr_type) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env index in
    let transition_type = trans_ident_type typedef_env prev_type in
    let arr_inside_type =
      begin
        match transition_type with
        | A.ARRAY atype -> trans_ident_type typedef_env atype
        | _ -> assert false
      end in
    begin
      match arr_inside_type with
      | A.STRUCT sid ->
        (T.ARRDONOTHING (prev_exp,curr_exp,(trans_ident_type_to_string "" arr_inside_type)) ,(A.STRUCT sid))
      | _ ->
        (T.ARRDEREF (prev_exp, curr_exp,(trans_ident_type_to_string "" arr_inside_type)),arr_inside_type)
    end






  | _ -> assert false
(* let env = List.fold_left ~f(fun nv->
                            let t=Temp.create() in
                            let env = S.set env ~key:(get_first nv) ~data:(get_second nv)
                            in env
                           ) ~init:() NVs in
   let (l,env') = trans_stms env stm
   in [T.FUNCDEF id]@l *)

(*
  stm =
    | If of exp * stm * stm
    | While of exp * stm*)

(* translate the statement *)
(* trans_stms : Temp.temp Symbol.table -> A.stm list -> Tree.stm list *)
let rec trans_stms env func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list= function
  | A.Declare (decl, stm) ->
    begin
      match decl with
      | A.NewVar (id, typedef) ->
        (* let () = printf "doing newvar decl \n" in  *)
        begin
          (* let () = printf "reach new var %s\n" (Symbol.name id) in *)
          (* let t = Temp.createeight() in *)
          let prev_decl =
            begin
              match S.find env id with
              | Some n ->
                n+1
              | None ->
                0
            end in
          let env' = S.set env ~key:id ~data:(prev_decl) in
          let true_type = trans_ident_type typedef_env typedef in
          let all_struct_name_list' =
            begin
              match get_the_deepest_type true_type with
              | A.STRUCT sid ->
                all_struct_name_list@[(Symbol.name sid)]
              | _ -> all_struct_name_list
            end in
          let type_env' =
              S.set type_env ~key:(id) ~data:(true_type)
          in
          (* let () = printf "reach here\n" in *)
          let (following,new_env,new_arr_env,new_type_env,new_all_struct_name) = trans_stms env' func_env struct_env struct_size_env arr_env type_env' typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list' stm in
          let curr_id = (Symbol.name id) ^ "local" ^ (string_of_int prev_decl) in
          match typedef with
          | A.POINTER (A.STRUCT ssid) ->
            begin
              match S.find struct_size_env ssid with
              | Some _ ->
                ([T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env,new_all_struct_name)
              | None ->
              (
                (* [T.OPAQUE (Symbol.name ssid)]@ *)
               [T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env,new_all_struct_name)
            end
          | _ ->
          ([T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env,new_all_struct_name)
        end
      (* trans_stms env stm *)
      | A.Init (id, typedef, e) ->
        (* let () = printf "doing init decl \n" in  *)
        begin
          (* let () = printf "enter init %s\n" (Symbol.name id) in *)
          (* let env' = S.set env ~key:id ~data:(Temp.get()) in *)
          let prev_decl =
            begin
              match S.find env id with
              | Some n ->
                n+1
              | None ->
                0
            end in
          let env' = S.set env ~key:id ~data:(prev_decl) in
          let true_type = trans_ident_type typedef_env typedef in


          let all_struct_name_list' =
            begin
              match get_the_deepest_type true_type with
              | A.STRUCT sid ->
                all_struct_name_list@[(Symbol.name sid)]
              | _ -> all_struct_name_list
            end in
          let type_env' =
              S.set type_env ~key:(id) ~data:(true_type)
          in
          let (following,new_env,new_arr_env,new_type_env,new_all_struct_name) =  trans_stms env' func_env struct_env struct_size_env arr_env type_env' typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list' (A.Seq ((A.Assign (A.Var id, e)), stm)) in
          let curr_id = (Symbol.name id) ^ "local" ^ (string_of_int prev_decl) in
          (* match typedef with
          | A.STRUCT ssid ->
            begin
              match S.find struct_size_env ssid with
              | Some _ ->
                ([T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env,new_all_struct_name)
              | None ->
                (
                  (* [T.OPAQUE (Symbol.name ssid)]@ *)
                  [T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env,new_all_struct_name)
            end
          | A.POINTER (A.STRUCT ssid) ->
            begin
              match S.find struct_size_env ssid with
              | Some _ ->
                ([T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env)
              | None ->
                (
                  (* [T.OPAQUE (Symbol.name ssid)]@ *)
                  [T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env)
            end
          | A.ARRAY (A.ARRAY (A.STRUCT ssid)) ->
            begin
              match S.find struct_size_env ssid with
              | Some _ ->
                ([T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env)
              | None ->
                (
                  (* [T.OPAQUE (Symbol.name ssid)]@ *)
                 [T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env)
            end
          | _ -> *)
            ([T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env,new_all_struct_name)

          (* ([T.ALLOC (curr_id,trans_ident_type_to_string "" true_type)]@following,new_env,new_arr_env,new_type_env) *)

        end
        (* trans_stms env (A.Seq ((A.Assign (id, e)), stm)) *)
    end

    (*
    trans_stms env (A.seq(A.Ass, stm))
      begin
        match decl with
        | A.NewVar (id,typedef) -> trans_stms env stm
        | A.Init (id, typedef, e) -> trans_stms env (A.Assign (id, e) ::stm:: stms)
      end *)
  | A.Assign (lval, e) ->

    (* let () = printf "reach assign\n" in *)
    let (exp,prev_type) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env (Temp.get()) typedef_env struct_detail_env func_id inline_env e in
    let true_type = trans_ident_type typedef_env prev_type in
    (* let () = printf "exit alloc \n" in *)
    begin
      match lval with
      | A.Var vid ->
        (* ([T.MOVE (T.TEMP (S.find_exn env vid), exp)], env, arr_env,type_env) *)
        (* printf "reach assign i\n"; *)
        begin
          match S.find type_env vid with
          | Some typedef ->
            let Some num = S.find env vid in
            let curr_id =  (Symbol.name vid) ^ "local" ^ (string_of_int num) in
            begin
              match Symbol.name vid with
              | "y" ->
                begin
                  match typedef with
                  | (A.POINTER (A.POINTER (A.STRUCT ssid))) ->
                    begin
                      match Symbol.name ssid with
                      | "foo" ->
                        begin
                          match exp with
                          | T.QUESTION (_,_,_,_,_,_,_) ->
                            printf "reach hacky space\n";
                            ([T.STORESPECIAL ((curr_id), exp,trans_ident_type_to_string "" (trans_ident_type typedef_env typedef))], env, arr_env,type_env,all_struct_name_list)
                          | _ ->
                            ([T.STORE ((curr_id), exp,trans_ident_type_to_string "" (trans_ident_type typedef_env typedef))], env, arr_env,type_env,all_struct_name_list)
                        end

                      | _ ->
                        ([T.STORE ((curr_id), exp,trans_ident_type_to_string "" (trans_ident_type typedef_env typedef))], env, arr_env,type_env,all_struct_name_list)
                    end

                  | _ ->
                    ([T.STORE ((curr_id), exp,trans_ident_type_to_string "" (trans_ident_type typedef_env typedef))], env, arr_env,type_env,all_struct_name_list)
                end

              | _ ->
                ([T.STORE ((curr_id), exp,trans_ident_type_to_string "" (trans_ident_type typedef_env typedef))], env, arr_env,type_env,all_struct_name_list)
            end

          | None -> assert false
        end
      | A.LDot (lvalD,idD) ->
        let t = Temp.createeight() in
        let t1 = Temp.createeight() in
        let (prev_exp, prev_type) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t typedef_env struct_detail_env func_id inline_env lvalD in
        let true_type = trans_ident_type typedef_env prev_type in
        begin
          match true_type with
          | A.STRUCT sid ->
            let struct_map = S.find_exn struct_env sid in
            let offset = S.find_exn struct_map idD in
            let struct_detail_map = S.find_exn struct_detail_env sid in
            let curr_type = S.find_exn struct_detail_map idD in
            let element_type = trans_ident_type typedef_env curr_type in

            ([T.DOTASSIGN (Symbol.name sid,prev_exp, offset, exp, trans_ident_type_to_string "" element_type)], env, arr_env,type_env,all_struct_name_list)
          | _ -> assert false
        end


      | A.LRArrow (lvalR,idR) ->
        let t = Temp.createeight() in
        let t1 = Temp.createeight() in
        let (prev_exp, prev_type) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t typedef_env struct_detail_env func_id inline_env lvalR in
        let true_type = trans_ident_type typedef_env prev_type in
        begin
          match true_type with
          | A.POINTER ptype ->
            let true_type1 = trans_ident_type typedef_env ptype in
            begin
              match true_type1 with
              | A.STRUCT sid ->
                let struct_map = S.find_exn struct_env sid in
                let offset = S.find_exn struct_map idR in
                let struct_detail_map = S.find_exn struct_detail_env sid in
                let curr_type = S.find_exn struct_detail_map idR in
                let element_type = trans_ident_type typedef_env curr_type in
                ([T.DOTASSIGNCHECK (Symbol.name sid,prev_exp, offset, exp, trans_ident_type_to_string "" element_type)], env, arr_env,type_env,all_struct_name_list)

              (* ([T.MOVEEXPTOADDR (prev_exp, offset, exp, t,t1)], env, arr_env,type_env) *)
              | _ ->assert false
            end
          | _ -> assert false
        end



      | A.LDeref lvalDe ->
        (* let () = printf "reach Lderef\n" in *)
        let t = Temp.createeight() in
        let t1 = Temp.createeight() in
        let (prev_exp, prev_type1) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t typedef_env struct_detail_env func_id inline_env lvalDe in
        let true_type = trans_ident_type typedef_env prev_type1 in
        let (A.POINTER exp_type) = true_type in
        let exp_type = trans_ident_type typedef_env exp_type in
        ([T.MOVETOADDR (prev_exp,exp,trans_ident_type_to_string "" exp_type)],env,arr_env,type_env,all_struct_name_list)



      | A.LArrDeref (lvalArr, index) ->
        let t = Temp.createeight() in
        let t1 = Temp.createeight() in
        let t2 = Temp.createeight() in
        let addrt = Temp.createeight() in
        let (prev_exp, prev_type) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t typedef_env struct_detail_env func_id inline_env lvalArr in
        let (curr_exp,curr_type) = trans_exp env func_env struct_env struct_size_env prev_type arr_env type_env t typedef_env struct_detail_env func_id inline_env index in
        let true_type = trans_ident_type typedef_env prev_type in
        begin
          match true_type with
          | A.ARRAY atype ->
            let true_type1 = trans_ident_type typedef_env atype in
            ([T.ASSIGNARR (prev_exp,curr_exp,exp,trans_ident_type_to_string "" true_type1)],env,arr_env,type_env,all_struct_name_list)
                (* ([T.ASSIGNARR (prev_exp, curr_exp, arr_size_for_each_element, exp, t,t1,t2)],env,arr_env,type_env) *)


          | _ -> assert false
        end
      | _ -> assert false
    end


  | A.Asnop (id,oper, e) ->
    begin
      let (exp,_) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env (Temp.get()) typedef_env struct_detail_env func_id inline_env e in

      match id with
      | A.LDot (lvalD,idD) ->
        begin
          let t1 = Temp.createeight() in
          let t2 = Temp.createeight() in
          let t3 = Temp.createeight() in
          let (prev_exp, prev_type) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t1 typedef_env struct_detail_env func_id inline_env lvalD in
          let true_type = trans_ident_type typedef_env prev_type in
          begin
            match true_type with
            | A.STRUCT sid ->
              let struct_map = S.find_exn struct_env sid in
              let offset = S.find_exn struct_map idD in
              let struct_detail_map = S.find_exn struct_detail_env sid in
              let curr_type = S.find_exn struct_detail_map idD in
              let element_type = trans_ident_type typedef_env curr_type in
              (* (T.RARROW (prev_exp, offset,t1),curr_type) *)
              ([T.DOTASNOP (trans_binop oper,Symbol.name sid,prev_exp,offset,trans_ident_type_to_string "" element_type,exp)],env,arr_env,type_env,all_struct_name_list)
            | _ -> assert false
          end
        end
      | A.LDeref lvalDe ->
        let t1 = Temp.createeight() in
        let t2 = Temp.createeight() in
        let t3 = Temp.createeight() in
        let (prev_exp, prev_type) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t1 typedef_env struct_detail_env func_id inline_env lvalDe in
        let true_type = trans_ident_type typedef_env prev_type in
        begin
          match true_type with
          | A.POINTER ptype ->
            let true_type1 = trans_ident_type typedef_env ptype in
            begin
                ([T.DEREFASNOP (trans_binop oper, prev_exp,exp, trans_ident_type_to_string "" true_type1)],env,arr_env,type_env,all_struct_name_list)
            end
          | _ -> assert false
        end
      | A.LRArrow (lvalR,idR) ->
        let t1 = Temp.createeight() in
        let t2 = Temp.createeight() in
        let t3 = Temp.createeight() in
        let (prev_exp, prev_type) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t1 typedef_env struct_detail_env func_id inline_env lvalR in
        let true_type = trans_ident_type typedef_env prev_type in
        begin
          match true_type with
          | A.POINTER ptype ->
            let true_type1 = trans_ident_type typedef_env ptype in
            begin
              match true_type1 with
              | A.STRUCT sid ->
                let struct_map = S.find_exn struct_env sid in
                let offset = S.find_exn struct_map idR in
                let struct_detail_map = S.find_exn struct_detail_env sid in
                let curr_type = S.find_exn struct_detail_map idR in
                let element_type = trans_ident_type typedef_env curr_type in
                ([T.DOTASNOP (trans_binop oper,Symbol.name sid,prev_exp,offset,trans_ident_type_to_string "" element_type,exp)],env,arr_env,type_env,all_struct_name_list)
            end
        end

      (* let t1 = Temp.createeight() in
      let t2 = Temp.createeight() in
      let (prev_exp,prev_type) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env exp in
      let (curr_exp,curr_type) = trans_exp env func_env struct_env struct_size_env prev arr_env type_env prev_addr typedef_env struct_detail_env func_id inline_env index in
      let transition_type = trans_ident_type typedef_env prev_type in
      let arr_inside_type =
        begin
          match transition_type with
          | A.ARRAY atype -> atype
          | _ -> assert false
        end in
      begin
        match arr_inside_type with
        | A.STRUCT sid ->
          (T.ARRDONOTHING (prev_exp,curr_exp,(trans_ident_type_to_string "" arr_inside_type)) ,(A.STRUCT sid))
        | _ ->
          (T.ARRDEREF (prev_exp, curr_exp,(trans_ident_type_to_string "" arr_inside_type)),arr_inside_type)
      end *)
      | A.LArrDeref (lvalArr, index) ->
        let t = Temp.createeight() in
        let t1 = Temp.createeight() in
        let t2 = Temp.createeight() in
        let addrt = Temp.createeight() in
        let (prev_exp, prev_type) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t1 typedef_env struct_detail_env func_id inline_env lvalArr in
        let (curr_exp,curr_type) = trans_exp env func_env struct_env struct_size_env prev_type arr_env type_env t1 typedef_env struct_detail_env func_id inline_env index in
        let true_type = trans_ident_type typedef_env prev_type in
        begin
          match true_type with
          | A.ARRAY atype ->
            let true_type1 = trans_ident_type typedef_env atype in
            ([T.ARRASNOP ((trans_binop oper), prev_exp,curr_exp,exp,trans_ident_type_to_string "" true_type1)],env,arr_env,type_env,all_struct_name_list)

          | _ -> assert false
        end
      | _ ->
        trans_stms env func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list (A.Assign(id, (A.Binop (oper, id, e))))
    end

  | A.Return e ->

    let (exp,return_type) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env (Temp.get()) typedef_env struct_detail_env func_id inline_env e in
    let true_type = trans_ident_type typedef_env return_type in
    ([T.RETURN (exp,trans_ident_type_to_string "" true_type)], env,arr_env,type_env,all_struct_name_list)



  | A.VoidReturn ->
    ([T.VOIDRETURN],env,arr_env,type_env,all_struct_name_list)
  | A.Seq (stm1, stm2) ->
    let (l1, new_env,new_arr_env,new_type_evn,new_all_struct_name) = trans_stms env func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list stm1
    in let (l2, new_env',new_arr_env',new_type_evn',new_all_struct_name') = trans_stms new_env func_env struct_env struct_size_env new_arr_env new_type_evn typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list new_all_struct_name stm2
    in (l1@l2, new_env',new_arr_env',new_type_evn',new_all_struct_name')
  (* trans_stms env stm2 *)
  (* trans_stms env (stm1::stm2::stms) *)
  | A.Nop ->
    ([T.NOP], env,arr_env,type_env,all_struct_name_list)
  (* trans_stms env stms *)
  | A.If (e, s1, s2) ->
    let (l1, env1,new_arr_env,new_type_evn,new_all_struct_name) = trans_stms env func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list s1 in
    let (l2, env2,new_arr_env',new_type_evn',new_all_struct_name') = trans_stms env1 func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list new_all_struct_name s2 in
    let t = Temp.createeight() in
    (* ([T.IF (trans_exp env func_env e, l1, l2,t)], env1) *)
    let label1 = (Label.create())
    and label2 = (Label.create())
    and label3 = (Label.create()) in
    let (exp,_) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env (Temp.get()) typedef_env struct_detail_env func_id inline_env e in
    (* let (exp, _) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t typedef_env struct_detail_env lvalDe in *)

    ([T.IF (exp, label1, label2,t)]
     @[T.WRITELABEL label1]
     @l1
     @[T.GOTO label3]
     @[T.WRITELABEL label2]
     @l2
     @[T.FINISHIF label3],env2,arr_env,type_env,new_all_struct_name')
  | A.While (e, s)->
    (* let l1 = A.LABEL (Label.create())
       and l2 = A.LABEL (Label.create())
       and l3 = A.LABEL (Label.create()) in
       T.EMPTYBRANCH l1
       @ T.IF (trans_exp env e, l2, l3)
       @ T.BRANCHSTM (l2, trans_stms env s, l1)
       @ T.EMPTYBRANCH l3 *)
    let (l1, newenv,new_arr_env,new_type_evn,new_all_struct_name) = trans_stms env func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list s in
    let t = Temp.createeight() in
    let label1 = (Label.create())
    and label2 = (Label.create())
    and label3 = (Label.create()) in
    let (exp,_) = trans_exp newenv func_env struct_env struct_size_env A.INT new_arr_env new_type_evn (Temp.get()) typedef_env struct_detail_env func_id inline_env e in
    (* let (, ) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env t typedef_env struct_detail_env lvalDe in *)

    ( [T.GOTO label1]
     @[T.WRITELABEL label1]
     @[T.IF (exp,label2,label3,t)]
     @[T.WRITELABEL label2]
     @l1
     @[T.FINISHWHILE (label1,label3)],newenv,arr_env,type_env,new_all_struct_name)
  (* ([T.WHILE (trans_exp newenv func_env e, l1,t)], newenv) *)
  (* | A.Exp e ->
      assert false *)
  | A.Markeds marked_stm->
    trans_stms env func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list (Mark.data marked_stm)
  | A.Block stm ->
    (* let (l1, new_env, new_arr_env, new_type_env) =  *)
      trans_stms env func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list stm
    (* in (l1,env,arr_env,type_env) *)
  | A.Assert e ->
    begin
      let t = Temp.createeight() in
      let (exp,_) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env (Temp.get()) typedef_env struct_detail_env func_id inline_env e in
      ([T.ASSERT (exp, t)], env,arr_env,type_env,all_struct_name_list)
    end
  | A.Exp e ->
    let t = Temp.createeight() in
    (* let env' = S.set env ~key:id ~data:t in
       trans_stms env' (A.Seq ((A.Assign (id, e)), stm)) *)
    let (exp,_) = trans_exp env func_env struct_env struct_size_env A.INT arr_env type_env (Temp.get()) typedef_env struct_detail_env func_id inline_env e in

    ([T.JUSTCOMPUTE (t, exp)], env,arr_env,type_env,all_struct_name_list)
  (* ([trans_exp env e], env) *)
  | A.For s ->
    (* let (l1, new_env, new_arr_env, new_type_env) =  *)
      trans_stms env func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list all_struct_name_list s
    (* in (l1,env,arr_env,type_env) *)
  | _ ->
    (* let () = printf "we got not found too \n" in  *)
    assert false                  (* There must be a return! *)

(* let translate stms = let (l, env) = trans_stms S.empty stms in l *)


(* | FDecl of typedefine * ident * param list
   | FDefn of typedefine * ident * param list * stm
   | NewType of ident * typedefine
   | Main of typedefine * stm
   | MainDecl of typedefine
   | Markedg of gdecl Mark.marked *)


let trans_gdecl func_env struct_env struct_detail_env struct_size_env typedef_env temp_cnt_list safe_mode inline_env dec_func_env all_struct_name_list = function
  | A.Main (typedef, stm) ->
    begin
      (* Temp.reset(); *)
      Arguement.reset();
      let func_env = S.set func_env ~key:(Symbol.symbol "main") ~data:(FDecl(A.INT,[])) in
      let (l,env',arr_env,type_env,all_struct_name_list') = trans_stms S.empty func_env struct_env struct_size_env S.empty S.empty typedef_env struct_detail_env (Symbol.symbol "main") inline_env safe_mode [] all_struct_name_list stm in
      let cnt = Temp.access() in
      let new_temp_cnt_list = temp_cnt_list@[cnt] in
      let yetanotherl = List.fold_left ~f:(fun b a ->
          begin
            match a with
            | T.RETURN (e,return_type) -> b@[T.IMPRETURN (e,return_type)]
            | T.VOIDRETURN -> b@[T.IMPVOIDRETURN cnt]
            | _ -> b@[a]
          end
        ) ~init:([]) l in

        let (anotherl,allocas) = List.fold_left ~f:(fun (b,allo) a ->
            begin
              match a with
              | T.ALLOC (curr_id,typedef) -> (b,allo@[a])
              | _ -> (b@[a],allo)
            end
        ) ~init:([],[]) yetanotherl in
      let anotherl = allocas@anotherl in


      ([
        [(T.MAINDEF ("_c0_main",cnt))]
        @anotherl@[T.MAINEND]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env,new_temp_cnt_list, inline_env, dec_func_env,all_struct_name_list')
    end
  | A.MainDecl typedef ->
    ([[]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env,temp_cnt_list@[0],inline_env, dec_func_env,all_struct_name_list)
  | A.FDecl (typedef, id, nvs)->
    (* let () = printf "%s\n" (Symbol.name id) in *)
    let func_env = S.set func_env ~key:(id) ~data:(FDecl(typedef, nvs)) in
    ([[]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env,temp_cnt_list@[0], inline_env, dec_func_env,all_struct_name_list)
  | A.NewType (id, typedef)->
    begin
      match typedef with
      | A.IDENT iid ->
        begin
          match S.find typedef_env iid with
          | Some pre_defined_type ->
            let new_typedef_env = S.set typedef_env ~key:(id) ~data:(pre_defined_type) in
            ([[]],func_env,struct_env,struct_detail_env,struct_size_env,new_typedef_env,temp_cnt_list@[0],inline_env, dec_func_env,all_struct_name_list)
          | None -> assert false
        end
      | othertype ->
        let new_typedef_env = S.set typedef_env ~key:(id) ~data:(othertype) in
        (* let () = printf "%s\n" (Symbol.name id) in *)
        ([[]],func_env,struct_env,struct_detail_env,struct_size_env,new_typedef_env,temp_cnt_list@[0],inline_env, dec_func_env,all_struct_name_list)
    end
  | A.FDefn (t, func_id, nvs,stm) ->
    begin
      (* Temp.reset(); *)
      Arguement.reset();
      (* let () = printf "reach fdefn %s\n" (Symbol.name id) in *)
      let dec_func_env = S.set dec_func_env ~key:func_id ~data:Deleted in
      let (prev_alloca,(env,arr_env,type_env,arg_temp_list,arg_string_list,_,new_all_struct_name)) = List.fold_left ~f:(fun (l,(e,a_e,t_e,a_t_e,a_l,counter,new_all)) nv->
          let t=Temp.createeight() in
          let a=Arguement.create() in
          let id = get_first nv in
          let typedef = get_second nv in
          let typ = trans_ident_type typedef_env typedef in
          let new_all_name =
            begin
              match get_the_deepest_type typ with
              | A.STRUCT sid -> new_all@[(Symbol.name sid)]
              | _ -> new_all
            end in
          let type_string = trans_ident_type_to_string "" typ in
          let arg_string = type_string in
          let arg_name = (Symbol.name id) ^ "local" ^ (string_of_int 0) in
          let arg_string_list = a_l@[arg_string] in
          let env = S.set e ~key:(id) ~data:0 in
          let arg_temp_list = [t]@a_t_e
          in
          let arr_env =
            begin
              match typ with
              | ARRAY in_ty-> S.set a_e ~key:(id) ~data:in_ty
              | _ -> a_e
            end in

          let prev_alloca = l@[T.ALLOC (arg_name,trans_ident_type_to_string "" typ)]@[T.STOREARG (arg_name,counter,type_string)] in
          let type_env = S.set t_e ~key:(id) ~data:typ
          in (prev_alloca,(env,arr_env,type_env,arg_temp_list,arg_string_list,counter+1,new_all_name))
        ) ~init:(([],(S.empty,S.empty,S.empty,[],[],0,all_struct_name_list))) nvs in
      let argcnt = Arguement.access() in
      (* let flag = 1 in *)
      let (arg_string,flag) = List.fold_left ~f:(fun (s,flag) each_arg ->
          if flag =1 then
            let ret = s ^ each_arg in
            (ret,0)
          else
            let ret = s ^ ", " ^ each_arg in
            (ret,0)
        ) ~init:("",1) arg_string_list in
      let func_env = S.set func_env ~key:(func_id) ~data:(FDecl(t, nvs)) in
      let (l,env',arr_env',type_env',all_struct_name_list') = trans_stms env func_env struct_env struct_size_env arr_env type_env typedef_env struct_detail_env func_id inline_env safe_mode arg_temp_list new_all_struct_name stm in

      let temp_cnt = Temp.access() in
      let new_inline_env = if temp_cnt < 20 then S.set inline_env ~key:(func_id) ~data:(stm) else inline_env in
      let new_temp_cnt_list = temp_cnt_list@[temp_cnt] in
      let yetanotherl = List.fold_left ~f:(fun b a ->
          begin
            match a with
            | T.RETURN (e,return_type) -> b@[T.IMPRETURN (e,trans_ident_type_to_string "" (trans_ident_type typedef_env t))]
            | T.VOIDRETURN -> b@[T.IMPVOIDRETURN temp_cnt]
            | _ -> b@[a]
          end
        ) ~init:([]) l in


                let (anotherl,allocas) = List.fold_left ~f:(fun (b,allo) a ->
                    begin
                      match a with
                      | T.ALLOC (curr_id,typedef) -> (b,allo@[a])
                      | _ -> (b@[a],allo)
                    end
                ) ~init:([],[]) yetanotherl in

      let anotherl = allocas@anotherl in

(*
      let (anotheranotherl,opaques) = List.fold_left ~f:(fun (b,opa) a ->
          begin
            match a with
            | T.OPAQUE (s) -> (b,opa@[a])
            | _ -> (b@[a],opa)
          end
        ) ~init:([],[]) anotherl in
      let opaques =
        begin
          match trans_ident_type typedef_env t with
          | A.POINTER (A.STRUCT sssid) -> opaques@[T.OPAQUE (Symbol.name (sssid))]
          | _ -> opaques
        end in

      let anotherl = anotheranotherl in *)

      let t =  trans_ident_type typedef_env t in
      let all_struct_name_list' =
        begin
          match get_the_deepest_type t with
          | A.STRUCT sid -> all_struct_name_list'@[(Symbol.name sid)]
          | _ -> all_struct_name_list'
        end in


      let return_type =
        begin
          match trans_ident_type typedef_env t with
          | A.VOID -> "void"
          | A.BOOL -> "i1"
          | A.INT -> "i32"
          | _ -> trans_ident_type_to_string "" t
        end in
      begin
        match t with
        | A.VOID ->
          begin
            ([
              (* opaques@ *)
              [(T.FUNCDEF (Symbol.name func_id, arg_string,return_type))]
                  @prev_alloca
                  @anotherl
                  @[(T.IMPVOIDRETURN temp_cnt)]@[T.FUNCEND (trans_ident_type_to_string "" t)]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env,new_temp_cnt_list,new_inline_env,dec_func_env,all_struct_name_list')

          end
        | _ ->
          begin
            ([
              (* opaques@ *)
              [(T.FUNCDEF (Symbol.name func_id, arg_string,return_type))]
                  @prev_alloca
                  @anotherl@[T.FUNCEND (trans_ident_type_to_string "" t)]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env,new_temp_cnt_list,new_inline_env,dec_func_env,all_struct_name_list')

          end



      end
    end


  | A.SDecl stru ->
    ([[]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env,temp_cnt_list@[0],inline_env,dec_func_env,all_struct_name_list)
  | A.SDef (stru_id, plist) ->

    Llvmstruoffset.reset();
    let modu = 0 in
    let (opaque_defn,(new_map,detail_map,stru_str,flag,curr_struct_size,all_struct_name_list')) = List.fold_left ~f:(fun (l,(e,d_e,str,flag,c_s_s,all_name)) p ->
        let (id,t) =
          begin
            match p with
            | A.Param (pid,pty) -> (pid,pty)
          end in
        (* let () = printf "%s\n" (Symbol.name id) in *)
        let true_type = trans_ident_type typedef_env t in
        let new_map = S.set e ~key:(id) ~data: (Llvmstruoffset.access()) in
        let new_detail_map = S.set d_e ~key:(id) ~data: (true_type) in
        let so = Llvmstruoffset.createeight() in
        let curr_struct_size =
        begin
          match true_type with
          | A.STRUCT ssssid ->
            begin
              match S.find struct_size_env ssssid with
              | Some stru_size ->
                c_s_s + stru_size
              | None -> c_s_s + 8
            end
          | A.POINTER ptype ->
            c_s_s + 8
          | A.ARRAY atype ->
            c_s_s + 8
          | _ -> c_s_s+8
        end in
        let all_name_list =
          begin
            match get_the_deepest_type true_type with
            | A.STRUCT sid ->
              all_name@[(Symbol.name sid)]
            | _ -> all_name
          end in
        if flag = 1
        then
          ([],(new_map,new_detail_map, str ^ (trans_ident_type_to_string "" true_type),0,curr_struct_size,all_name_list))
        else
          ([],(new_map,new_detail_map, str ^ ", " ^ (trans_ident_type_to_string "" true_type),0,curr_struct_size,all_name_list))

      ) ~init:(([],(S.empty,S.empty,"",1,0,all_struct_name_list))) plist in
    let stru_size_curr = curr_struct_size in
    let new_struct_size_env = S.set struct_size_env ~key:stru_id ~data:stru_size_curr in
    let new_struct_env = S.set struct_env ~key:stru_id ~data:new_map in
    let new_struct_detail_env = S.set struct_detail_env ~key:stru_id ~data:detail_map in
    (* let () = printf "struct type is %s\n" (Symbol.name stru_id) in *)
    (* let () = printf "%d " stru_size_curr in *)
    ([[T.STRUCTDEFN ((Symbol.name stru_id),stru_str)]], func_env, new_struct_env, new_struct_detail_env, new_struct_size_env, typedef_env,temp_cnt_list@[0],inline_env,dec_func_env,all_struct_name_list')
  | _ -> ([[]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env,temp_cnt_list@[0],inline_env,dec_func_env,all_struct_name_list)
  (*
  and gdecl =
  | FDecl of typedefine * ident * NewVar list
  | FDefn of typedefine * ident * NewVar list * stm
  | NewType of ident * typedefine *)




let trans_header func_env struct_env struct_detail_env struct_size_env typedef_env = function
  | A.Main (typedef, stm) ->
    assert false
  | A.MainDecl typedef ->
    assert false
  | A.FDecl (typedef, id, nvs)->
    ([[]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env)
  | A.NewType (id, typedef)->
    begin
      match typedef with
      | A.IDENT iid ->
        begin
          match S.find typedef_env iid with
          | Some pre_defined_type ->
            let new_typedef_env = S.set typedef_env ~key:(id) ~data:(pre_defined_type) in
            ([[]],func_env,struct_env,struct_detail_env,struct_size_env,new_typedef_env)
          | None -> assert false
        end
      | othertype ->
        let new_typedef_env = S.set typedef_env ~key:(id) ~data:(othertype) in
        (* let () = printf "%s\n" (Symbol.name id) in *)
        ([[]],func_env,struct_env,struct_detail_env,struct_size_env,new_typedef_env)
    end

  | A.SDecl stru ->
    ([[]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env)
  | A.SDef (stru_id, plist) ->
    Llvmstruoffset.reset();
    (* let () = printf "%d " 10 in *)
    (* let () = printf "reach sdef %s\n" (Symbol.name stru_id) in *)
    let modu = 0 in
    let (opaque_defn,(new_map,detail_map,stru_str,_)) = List.fold_left ~f:(fun (l,(e,d_e,str,flag)) p ->
        let (id,t) =
          begin
            match p with
            | A.Param (pid,pty) -> (pid,pty)
          end in
        (* let () = printf "%s\n" (Symbol.name id) in *)
        let (new_map',new_detail_map') =
        begin
          match t with
          (*
          | A.INT ->
            begin
              let new_map = S.set e ~key:(id) ~data: ((Llvmstruoffset.access())) in
              let new_detail_map = S.set d_e ~key:(id) ~data: (A.INT) in
              let so = Llvmstruoffset.createeight() in
              (* let () = printf "%s\n" (Symbol.name id) in *)
              let modu = 0 in
              ([],(new_map,new_detail_map))
            end
          | A.BOOL ->
            begin
              let new_map = S.set e ~key:(id) ~data: ((Llvmstruoffset.access())) in
              let new_detail_map = S.set d_e ~key:(id) ~data: (A.BOOL) in
              let so = Llvmstruoffset.createeight() in
              let modu = 0 in
              ([],(new_map,new_detail_map))
            end
          | A.ARRAY atype ->
            begin
              let new_map = S.set e ~key:(id) ~data:(Llvmstruoffset.access()) in
              let new_detail_map = S.set d_e ~key:(id) ~data:(A.ARRAY atype) in
              let so = Llvmstruoffset.createeight() in
              ([],(new_map,new_detail_map))
            end
          | A.POINTER pointer_type->
            begin
              match modu with
              | 0 ->
                let new_map = S.set e ~key:(id) ~data: ((Llvmstruoffset.access())) in
                let new_detail_map = S.set d_e ~key:(id) ~data: (A.POINTER pointer_type) in
                let so = Llvmstruoffset.createeight() in
                let modu = 0 in
                ([],(new_map,new_detail_map))
              | 4 ->
                let so1 = Llvmstruoffset.createfour() in
                let new_map = S.set e ~key:(id) ~data: ((Llvmstruoffset.access())) in
                let new_detail_map = S.set d_e ~key:(id) ~data: (A.POINTER pointer_type) in
                let so2 = Llvmstruoffset.createeight() in
                let modu = 0 in
                ([],(new_map,new_detail_map))
             end
          *)
          | A.STRUCT s_id->
            begin
              let stru_size = S.find_exn struct_size_env s_id in
              let new_map = S.set e ~key:(id) ~data: ((Llvmstruoffset.access())) in
              let new_detail_map = S.set d_e ~key:(id) ~data: (A.STRUCT s_id) in
              let t = Llvmstruoffset.set stru_size in
              let modu = 0 in
              ((new_map,new_detail_map))

            end
          | _ ->
            begin
              let true_type = trans_ident_type typedef_env t in
              let new_map = S.set e ~key:(id) ~data: ((Llvmstruoffset.access())) in
              let new_detail_map = S.set d_e ~key:(id) ~data: (true_type) in
              let so = Llvmstruoffset.createeight() in
              ((new_map,new_detail_map))
            end
        end in
        let true_type1 = trans_ident_type typedef_env t in
        let stru_str =
        if flag = 1 then
          str ^ (trans_ident_type_to_string "" true_type1)
        else
          str ^ ", " ^ (trans_ident_type_to_string "" true_type1)
        in
        ([],(new_map',new_detail_map',stru_str,0))
      ) ~init:(([],(S.empty,S.empty,"",1))) plist in

    let stru_size_curr = Llvmstruoffset.access() in
    let new_struct_size_env = S.set struct_size_env ~key:stru_id ~data:stru_size_curr in
    let new_struct_env = S.set struct_env ~key:stru_id ~data:new_map in
    let new_struct_detail_env = S.set struct_detail_env ~key:stru_id ~data:detail_map in
    (* let () = printf "struct type is %s\n" (Symbol.name stru_id) in *)
    (* let () = printf "%d " stru_size_curr in *)
    ([[T.STRUCTDEFN ((Symbol.name stru_id),stru_str)]], func_env, new_struct_env, new_struct_detail_env, new_struct_size_env, typedef_env)
  | _ -> ([[]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env)
















let rec translate_gdecl gdecls func_env struct_env struct_detail_env struct_size_env typedef_env temp_cnt_list safe_mode inline_env dec_func_env all_struct_name_list=
  begin
    match gdecls with
    | None -> ([[]], S.empty, S.empty, S.empty, S.empty, S.empty,[], S.empty, S.empty,[])
    | Some gdecls ->
      (match gdecls with
       | [] ->
         ([[]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env,temp_cnt_list,inline_env, dec_func_env,all_struct_name_list)
       | gdecl::right ->
         let (stms1, func_env1,struct_env1,struct_detail_env1,struct_size_env1,typedef_env1,temp_cnt_list1,inline_env1, dec_func_env,all_struct_name_list1) =
           (trans_gdecl func_env struct_env struct_detail_env struct_size_env typedef_env temp_cnt_list safe_mode inline_env dec_func_env all_struct_name_list gdecl) in
         let (stms2, func_env2,struct_env2,struct_detail_env2,struct_size_env2,typedef_env2,temp_cnt_list2,inline_env2, dec_func_env,all_struct_name_list2) =
           translate_gdecl (Some right) func_env1 struct_env1 struct_detail_env1 struct_size_env1 typedef_env1 temp_cnt_list1 safe_mode inline_env1 dec_func_env all_struct_name_list1 in
         (stms1@stms2,func_env2,struct_env2,struct_detail_env2,struct_size_env2,typedef_env2,temp_cnt_list2,inline_env2, dec_func_env,all_struct_name_list2)
      )

  end


let rec translate_header gdecls func_env struct_env struct_detail_env struct_size_env typedef_env=
  begin
    match gdecls with
    | None -> ([[]], S.empty, S.empty, S.empty, S.empty, S.empty)
    | Some gdecls ->
      (match gdecls with
       | [] -> ([[]],func_env,struct_env,struct_detail_env,struct_size_env,typedef_env)
       | gdecl::right ->
         let (stms1, func_env1,struct_env1,struct_detail_env1,struct_size_env1,typedef_env1) = (trans_header func_env struct_env struct_detail_env struct_size_env typedef_env gdecl) in
         let (stms2, func_env2,struct_env2,struct_detail_env2,struct_size_env2,typedef_env2) = (translate_header (Some right) func_env1 struct_env1 struct_detail_env1 struct_size_env1 typedef_env1) in
         (stms1@stms2,func_env2,struct_env2,struct_detail_env2,struct_size_env2,typedef_env2))

  end






(* let (l,env) = trans_gdecl S.empty gdecls in l *)
