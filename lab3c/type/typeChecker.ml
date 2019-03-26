(* L1 Compiler
 * TypeChecker
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Simple typechecker that checks two properties:
 *  (1) If a variable is initialized, it has previously been declared.
 *  (2) If a variable is used, it has previously been initialized.
 * This is sufficient for now, since only int types are available in L1.
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now distinguishes between declarations and initialization
 * Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
 * Should be more up-to-date with modern spec.
 * Modified: Matt Bryant <mbryant@andrew.cmu.edu> Fall 2015
 * Handles undefined variables in unreachable code, significant simplifications
 * Modified: Alice Rao <alrao@andrew.cmu.edu> Fall 2017
 *
*)

open Core

module A = Ast
module S = Symbol.Map

type init_status = Decl of A.typedefine | Init of A.typedefine

type return_type =
  | VoidRet
  | Ret of A.typedefine

type func_init_status =
  | Deleted
  | FDecl of A.typedefine * A.param list
  | FDefn of A.typedefine * A.param list
  | FDeclUsed of A.typedefine * A.param list

let compare_type t1 t2 env =
  let t1 = match t1 with
     | A.IDENT id1 -> S.find env id1
 	   | _ -> Some t1
  and t2 = match t2 with
	   | A.IDENT id2 -> S.find env id2
	   | _ -> Some t2
  in match t1, t2 with
  | Some type1, Some type2 -> if type1 = type2 then true else false
  | _ -> false

let check_void t type_env ext =
  match t with
  | A.VOID -> ErrorMsg.error ext ("variable type should not be void");
    raise ErrorMsg.Error
  | A.IDENT id ->
    begin
      match S.find type_env id with
      | None -> ErrorMsg.error ext ("type not declared");
        raise ErrorMsg.Error
      | Some t' ->
        (match t' with
         | A.VOID -> ErrorMsg.error ext ("variable type should not be void");
           raise ErrorMsg.Error
         | _ -> t')
    end
  | _ -> t


(* tc_exp : init_status Symbol.Map.t -> Ast.exp -> Mark.ext option -> unit *)
let rec tc_exp env func_env type_env ast ext t =
  match ast with
    A.Var id ->
    begin
      match S.find env id with
      | None -> ErrorMsg.error ext
                  ("undeclared variable `" ^ Symbol.name id ^ "'");
        raise ErrorMsg.Error
      | Some (Decl t') -> ErrorMsg.error ext
                            ("uninitialized variable `" ^ Symbol.name id ^ "'") ;
        raise ErrorMsg.Error
      | Some (Init t') ->
        begin
          match (t,t') with
          | Some A.INT, A.INT -> (Some A.INT, func_env)
          | Some A.BOOL, A.BOOL -> (Some A.BOOL, func_env)
          | None, _ -> (Some t', func_env)
          | _ -> ErrorMsg.error ext
                   ("variable `" ^ Symbol.name id ^ "' type not match") ;
            raise ErrorMsg.Error
        end

    end
  | A.ConstExp c ->
    begin
      match t with
      | Some A.BOOL -> ErrorMsg.error ext
                    ("expecting an int, but a boolean was given");
        raise ErrorMsg.Error
      | _ -> (Some A.INT, func_env)
    end
  | A.BoolExp _ ->
    begin
      match t with
      | Some A.INT -> ErrorMsg.error ext
                   ("expecting a boolean, but an int was given");
        raise ErrorMsg.Error
      | _ -> (Some A.BOOL, func_env)
    end
  | A.Unop (oper, e) ->
    begin
      match oper with
      | A.LOGICALNOT ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ ->
           let (_, func_env) = tc_exp env func_env type_env e ext (Some A.BOOL)
           in (Some A.BOOL, func_env))
      | _ ->
        (match t with
         | Some A.BOOL -> ErrorMsg.error ext
                       ("expecting an int, but a boolean was given");
           raise ErrorMsg.Error
         | _ ->
           let (_, func_env) = tc_exp env func_env type_env e ext (Some A.INT)
           in (Some A.INT, func_env))
    end
  | A.Binop (oper, e1, e2) ->
    begin
      match oper with
      | A.LOGICALAND ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> let (_, func_env) = tc_exp env func_env type_env e1 ext (Some A.BOOL)
           in let (_, func_env) = tc_exp env func_env type_env e2 ext (Some A.BOOL)
           in (Some A.BOOL, func_env))
      | A.LOGICALOR ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> let (_, func_env) = tc_exp env func_env type_env e1 ext (Some A.BOOL)
           in let (_, func_env) = tc_exp env func_env type_env e2 ext (Some A.BOOL)
           in (Some A.BOOL, func_env))
      (* | A.ISEQ ->
        (match t with
         | A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | A.BOOL -> try tc_exp env e1 ext A.INT; tc_exp env e2 ext A.INT
           with ErrorMsg.Error -> tc_exp env e1 ext A.BOOL; tc_exp env e2 ext A.BOOL)
      | A.NOTEQ ->
        (match t with
         | A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | A.BOOL -> tc_exp env e1 ext A.INT; tc_exp env e2 ext A.INT) *)
      | A.ISEQ ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> begin
                  let (Some t1, func_env) = tc_exp env func_env type_env e1 ext None
                  in let (Some t2, func_env) = tc_exp env func_env type_env e2 ext None
                  in match (t1, t2) with
                  | A.INT, A.INT -> (Some A.BOOL, func_env)
                  | A.BOOL, A.BOOL -> (Some A.BOOL, func_env)
                  | _ -> ErrorMsg.error ext
                      ("Type mismatch between two sides of equality");
                  raise ErrorMsg.Error
                 end)
      | A.NOTEQ ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> begin
             let (Some t1, func_env) = tc_exp env func_env type_env e1 ext None
             in let (Some t2, func_env) = tc_exp env func_env type_env e2 ext None
             in match (t1, t2) with
             | A.INT, A.INT -> (Some A.BOOL, func_env)
             | A.BOOL, A.BOOL -> (Some A.BOOL, func_env)
             | _ -> ErrorMsg.error ext
                 ("Type mismatch between two sides of equality");
             raise ErrorMsg.Error
           end)
      | A.LESSTHAN ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> let (_, func_env) = tc_exp env func_env type_env e1 ext (Some A.INT)
           in let (_, func_env) = tc_exp env func_env type_env e2 ext (Some A.INT)
           in (Some A.BOOL, func_env))
      | A.LESSOREQ ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> let (_, func_env) = tc_exp env func_env type_env e1 ext (Some A.INT)
           in let (_, func_env) = tc_exp env func_env type_env e2 ext (Some A.INT)
           in (Some A.BOOL, func_env))
      | A.GREATERTHAN ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> let (_, func_env) = tc_exp env func_env type_env e1 ext (Some A.INT)
           in let (_, func_env) = tc_exp env func_env type_env e2 ext (Some A.INT)
           in (Some A.BOOL, func_env))
      | A.GREATEROREQ ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> let (_, func_env) = tc_exp env func_env type_env e1 ext (Some A.INT)
           in let (_, func_env) = tc_exp env func_env type_env e2 ext (Some A.INT)
           in (Some A.BOOL, func_env))
      | _ ->
        (match t with
         | Some A.BOOL -> ErrorMsg.error ext
                       ("expecting an int, but a boolean was given");
           raise ErrorMsg.Error
         | _ -> let (_, func_env) = tc_exp env func_env type_env e1 ext (Some A.INT)
           in let (_, func_env) = tc_exp env func_env type_env e2 ext (Some A.INT)
           in (Some A.INT, func_env))
    end
  | A.Question (e1, e2, e3) ->
    begin
      let (_, func_env) = tc_exp env func_env type_env e1 ext (Some A.BOOL) in
      let (Some t1, func_env) = tc_exp env func_env type_env e2 ext t
      in let (Some t2, func_env) = tc_exp env func_env type_env e3 ext t
      in match (t1, t2) with
      | A.INT, A.INT -> (Some A.INT, func_env)
      | A.BOOL, A.BOOL -> (Some A.BOOL, func_env)
      | A.INT, A.BOOL -> ErrorMsg.error ext
          ("First one is int and second one is bool");
      raise ErrorMsg.Error
      | A.BOOL, A.INT -> ErrorMsg.error ext
          ("First one is bool and second one is int");
      raise ErrorMsg.Error
    end

  | A.Marked marked_exp ->
    tc_exp env func_env type_env (Mark.data marked_exp) (Mark.ext marked_exp) t
  | A.Call (id, exps) ->
    (match S.find func_env id with
     | Some (FDefn (func_t, params)) ->
       begin
         (List.iter2_exn ~f:(fun p e ->
              let (A.Param (_, param_t)) = p and (Some exp_t, func_env) = tc_exp env func_env type_env e ext None in
              match (compare_type param_t exp_t type_env) with
              | false -> ErrorMsg.error ext
                  ("function call and function declaration don't match\n");
              raise ErrorMsg.Error
	      | _ -> ()) params exps;
          let func_t = (match func_t with
              | A.IDENT ident -> (match S.find type_env ident with
                  | Some func_t' -> func_t'
                  | None -> ErrorMsg.error ext
                              ("function type not found\n");
                    raise ErrorMsg.Error)
              | _ -> func_t) in
          match t with
          | None -> (Some func_t, func_env)
          | Some t' -> (match compare_type func_t t' type_env with
              | true -> (t, func_env)
              | _ -> ErrorMsg.error ext
                       ("function call type does not match with function declaration\n");
                raise ErrorMsg.Error))
       end
     | Some (FDecl (func_t, params)) ->
         (* ErrorMsg.error ext
           ("calling undeclared function\n");
         raise ErrorMsg.Error *)

       begin
         (List.iter2_exn ~f:(fun p e ->
              let (A.Param (_, param_t)) = p and (Some exp_t, func_env) = tc_exp env func_env type_env e ext None in
              match (compare_type param_t exp_t type_env) with
              | false -> ErrorMsg.error ext
                  ("function call and function declaration don't match\n");
              raise ErrorMsg.Error
	      | _ -> ()) params exps;
          let func_t = (match func_t with
              | A.IDENT ident -> (match S.find type_env ident with
                  | Some func_t' -> func_t'
                  | None -> ErrorMsg.error ext
                              ("function type not found\n");
                    raise ErrorMsg.Error)
              | _ -> func_t) in
          let func_env = S.set func_env ~key:id ~data:(FDeclUsed (func_t, params)) in
          match t with
          | None -> (Some func_t, func_env)
          | Some t' -> (match compare_type func_t t' type_env with
              | true -> (t, func_env)
              | _ -> ErrorMsg.error ext
                       ("function call type does not match with function declaration\n");
                raise ErrorMsg.Error))
       end
     | Some (FDeclUsed (func_t, params)) ->
       begin
         (List.iter2_exn ~f:(fun p e ->
              let (A.Param (_, param_t)) = p and (Some exp_t, func_env) = tc_exp env func_env type_env e ext None in
              match (compare_type param_t exp_t type_env) with
              | false -> ErrorMsg.error ext
                           ("function call and function declaration don't match\n");
                raise ErrorMsg.Error
              | _ -> ()) params exps;
          let func_t = (match func_t with
              | A.IDENT ident -> (match S.find type_env ident with
                  | Some func_t' -> func_t'
                  | None -> ErrorMsg.error ext
                              ("function type not found\n");
                    raise ErrorMsg.Error)
              | _ -> func_t) in
          match t with
          | None -> (Some func_t, func_env)
          | Some t' -> (match compare_type func_t t' type_env with
              | true -> (t, func_env)
              | _ -> ErrorMsg.error ext
                       ("function call type does not match with function declaration\n");
                raise ErrorMsg.Error))
       end
     | Some Deleted -> ErrorMsg.error ext
                    ("Function shadowing error\n");
       raise ErrorMsg.Error
     | _ -> ErrorMsg.error ext
                    ("Function not declared\n");
       raise ErrorMsg.Error)


let rec find_len l =
  match l with
  | [] -> 0
  | left::r -> 1+(find_len r)

let intersect env1 env2 =
  let maped = S.mapi env1 ~f:(fun ~key:k ~data:d -> match S.find env2 k with
      | Some (Init t) -> Some d
      | Some (Decl t) -> Some (Decl t)
      | None -> None)
  in S.map ~f:(fun a -> let Some v = a in v)
    (S.filter ~f:(fun d -> match d with
         | None -> false
         | _ -> true) maped)

let print_map env = S.iteri env
    ~f:(fun ~key:k ~data:d ->
        match d with
        | Decl _ -> printf "declared\n"
        | Init _ -> printf "initiated\n")

let union env1 env2 =
  let env = ref env2 in
  S.iteri env1 ~f:(fun ~key:k ~data:d ->
      match S.find env2 k with
      | Some (Init t) -> ()
      | _ -> env := S.set (!env) ~key:k ~data:d);
  !env

let check_ret_or ret1 ret2 ext type_env =
  match ret1, ret2 with
  | VoidRet, VoidRet -> VoidRet
  | VoidRet, Ret t -> Ret t
  | Ret t, VoidRet -> Ret t
  | Ret t, Ret t' ->
    (match compare_type t t' type_env with
     | true -> Ret t
     | _ -> ErrorMsg.error ext ("different return types in a function");
       raise ErrorMsg.Error)

let check_ret_and ret1 ret2 ext type_env =
  match ret1, ret2 with
  | VoidRet, VoidRet -> VoidRet
  | VoidRet, Ret t -> VoidRet
  | Ret t, VoidRet -> VoidRet
  | Ret t, Ret t' ->
    (match compare_type t t' type_env with
     | true -> Ret t
     | _ -> ErrorMsg.error ext ("different return types in a function");
       raise ErrorMsg.Error)

let intersect_func prev_env new_env =
  let env = ref prev_env in
  S.iteri prev_env ~f:(fun ~key:k ~data:d ->
     match S.find new_env k with
     | Some (FDeclUsed (t, l)) -> env := S.set (!env) ~key:k ~data:(FDeclUsed (t, l))
     | _ -> ());
  !env

let rec tc_stms env func_env type_env ast ext ret void_func =
  match ast with
  | A.Nop -> (env, ret, func_env)
  | A.Assign (id, e) ->
    begin
      match S.find env id with
      | None -> ErrorMsg.error ext ("undeclared variable `" ^ Symbol.name id ^ "'");
        raise ErrorMsg.Error
      | Some (Decl t) ->
        begin
          let (_, func_env) = tc_exp env func_env type_env e ext (Some t) in
          let env = S.set env ~key:(id) ~data:(Init t)
          in (env, VoidRet, func_env)
        end
      | Some (Init t) ->
        begin
          let (_, func_env) = tc_exp env func_env type_env e ext (Some t) in
          (env, VoidRet, func_env)
        end
    end
  | A.If (e, s1, s2) ->
    begin
      let (_, func_env) = tc_exp env func_env type_env e ext (Some A.BOOL) in
      let (env1, ret1, func_env) = tc_stms env func_env type_env s1 ext ret void_func in
      let (env2, ret2, func_env) = tc_stms env func_env type_env s2 ext ret void_func
      in let new_env = intersect env1 env2
      in let new_env = S.mapi env ~f:(fun ~key:k ~data:d ->
          match S.find new_env k with
          | Some (Init t) -> (Init t)
          | _ -> d)
      in (new_env, check_ret_and ret1 ret2 ext type_env, func_env)
    end
  | A.While (e, s) ->
    begin
      let (_, func_env) = tc_exp env func_env type_env e ext (Some A.BOOL) in
      let (_, ret', _) = tc_stms env func_env type_env s ext ret void_func in
      (env, ret', func_env)
    end
  | A.Return e ->
    begin
      match void_func with
      | true -> ErrorMsg.error ext ("void function shouldnt return value");
        raise ErrorMsg.Error
      | _ -> begin
          (* printf "got to return\n"; *)
          let (Some t, func_env) = tc_exp env func_env type_env e ext None in
          let new_env = S.map env
              ~f:(fun a -> match a with
                  | (Decl t) -> (Init t)
                  | _ -> a)
          in match ret with
          | VoidRet ->  (new_env, Ret t, func_env)
          | Ret t' -> (match compare_type t t' type_env with
              | true -> (new_env, Ret t, func_env)
              | _ -> ErrorMsg.error ext ("Unmatched return types in a function");
                raise ErrorMsg.Error)
      end
    end
  | A.Seq (s1, s2) ->
    begin
      let (env1, ret1, new_env) = tc_stms env func_env type_env s1 ext ret void_func
      in let env1 = (match ret1 with
          | VoidRet -> env1
          | Ret t -> (S.map env1 ~f:(fun a -> match a with
              | (Decl t) -> (Init t)
              | _ -> a)))
      in
      match s1 with
      | A.Block s ->
        (let intersect_env = S.mapi env ~f:(fun ~key:k ~data:d ->
             match S.find env1 k with
             | Some (Init t) -> (Init t)
             | _ -> d)
         in let func_env = intersect_func func_env new_env
         in let (env2, ret2, func_env) = tc_stms intersect_env func_env type_env s2 ext ret void_func
         in (env2, check_ret_or ret1 ret2 ext type_env, func_env))
      | _ ->
        (let (env2, ret2,func_env) = tc_stms env1 new_env type_env s2 ext ret void_func in
         (env2, check_ret_or ret1 ret2 ext type_env, func_env))
    end
  | A.Declare (decl, s) ->
    begin
      match decl with
      | A.NewVar (id, t) ->
        begin
          match S.find type_env id with
          | Some _ -> ErrorMsg.error None
            ("variable `" ^ Symbol.name id ^ " has the same name as a type");
            raise ErrorMsg.Error
          | _ -> begin
            let t = check_void t type_env ext in
            match S.find env id with
            | Some _ -> ErrorMsg.error None ("redeclared variable `" ^ Symbol.name id);
              raise ErrorMsg.Error
            | None -> let func_env = S.set func_env ~key:id ~data:Deleted in
		      tc_stms (S.set env ~key:id ~data:(Decl t)) func_env type_env s ext ret void_func
          end
        end
      | A.Init (id, t, e) ->
        begin
      	  let t = check_void t type_env ext in
      	  match S.find type_env id with
      	  | Some _ -> ErrorMsg.error None
                  ("variable `" ^ Symbol.name id ^ " has the same name as a type");
                  raise ErrorMsg.Error
      	  | _ -> begin
              match S.find env id with
              | Some _ -> ErrorMsg.error None ("redeclared variable `" ^ Symbol.name id);
                raise ErrorMsg.Error
      	      | None -> begin
                let (_, func_env) = tc_exp env func_env type_env e ext (Some t) in
            		let env = S.set env ~key:id ~data:(Init t)
            		and func_env = S.set func_env ~key:id ~data:Deleted in
            		tc_stms env func_env type_env s ext ret void_func
            		end
	         end
	     end
    end
  | A.Markeds marked_stm ->
    tc_stms env func_env type_env (Mark.data marked_stm) (Mark.ext marked_stm) ret void_func
  | A.Block stm -> tc_stms env func_env type_env stm ext ret void_func
  | A.Assert exp ->
    begin
      let (_, func_env) = tc_exp env func_env type_env exp ext (Some A.BOOL) in
      (env,VoidRet, func_env)
    end
  | A.Exp e ->
    begin
      let (_, func_env) = tc_exp env func_env type_env e ext None in
      (env, VoidRet, func_env)
    end
  | A.For s ->
    let (new_env, _, func_env) = tc_stms env func_env type_env s ext ret void_func
    in let env = S.mapi env ~f:(fun ~key:k ~data:d ->
        match S.find new_env k with
        | Some (Init t) -> (Init t)
        | _ -> d)
    in (env, VoidRet, func_env)
  | A.VoidReturn ->
    (let new_env = S.map env
                        ~f:(fun a -> match a with
                            | (Decl t) -> (Init t)
                            | _ -> a)
    in match ret with
    | VoidRet -> (new_env, VoidRet, func_env)
    | _ -> ErrorMsg.error ext "prev return is not void\n";
          raise ErrorMsg.Error)


(* tc_stms :
 *   init_status Symbol.Map.t -> Ast.program -> Mark.ext option -> bool -> bool
 *
 * tc_stms env ast ext ret
 *   env: environment under which to consider the ast, where:
 *     find env id = Some Init if id is declared and initialized
 *     find env id = Some Decl if id is declared but not initialized
 *     find env id = None      if id is not declared
 *
 *   ast: the sequence of statements to typecheck.
 *   ext: the mark of the current statement, if available.
 *   ret: whether a return statement has already been encountered.
 *
 *   Returns a boolean indicating whether a return statement was encountered in
 *   the sequence of statements.
*)
(* let rec tc_stms env ast ext ret =
   match ast with
   | [] -> ret
   | A.Declare (A.NewVar (id,t), stm ) :: stms ->
    begin
      match S.find env id with
      | Some _ ->
          ErrorMsg.error None ("redeclared variable `" ^ Symbol.name id);
          raise ErrorMsg.Error
      | None -> tc_stms (S.set env ~key:id ~data:Decl) stms ext ret
    end
    (* The following translation is sound:
     *
     * int x = expr;  ===>   int x;
     *                       x = expr;
     *
     * This is because the expression can't legally contain the identifier x.
     * NB: This property will no longer hold when function calls are introduced.
     *)
   | A.Declare (A.Init (id, e)) :: stms ->
      tc_stms env (A.Declare (A.NewVar id) :: A.Assign (id, e) :: stms) ext ret
   | A.Assign (id, e) :: stms ->
    begin
      tc_exp env e ext;
      match S.find env id with
      | None ->
          ErrorMsg.error ext ("undeclared variable `" ^ Symbol.name id ^ "'");
          raise ErrorMsg.Error
        (* just got initialized *)
      | Some Decl -> tc_stms (S.set env ~key:id ~data:Init) stms ext ret
        (* already initialized *)
      | Some Init -> tc_stms env stms ext ret
    end
   | A.Return e :: stms ->
      tc_exp env e ext;
      (* Define all variables declared before return *)
      let env = S.map env ~f:(fun _ -> Init) in
      tc_stms env stms ext true
   | A.Markeds marked_stm :: stms ->
      tc_stms env (Mark.data marked_stm :: stms) (Mark.ext marked_stm) ret *)

let rec check_voids l env =
  match l with
  | [] -> true
  | left::r -> let A.Param (_, t) = left in
    match t with
    | A.IDENT id ->
      begin
      	match S.find env id with
      	| Some t' -> if t' = A.VOID then false else check_voids r env
      	| None -> false
      end
    | A.VOID -> false
    | _ -> check_voids r env

let rec start_env params type_env func_env =
  match params with
  | [] -> (S.empty, func_env)
  | l::r -> let (env, func_env) = start_env r type_env func_env in
    let A.Param (id, t) = l in
    match S.find env id with
    | Some _ -> ErrorMsg.error None "repetitive param name\n";
      raise ErrorMsg.Error
    | None ->
      begin
        match S.find type_env id with
        | Some _ -> ErrorMsg.error None "Param name same as type name\n";
          raise ErrorMsg.Error
        | _ -> begin
            let t = (match t with
                | A.IDENT id' -> begin
                    match S.find type_env id' with
                    | Some t' -> t'
                    | _ -> ErrorMsg.error None "param type not defined";
                      raise ErrorMsg.Error
                  end
                | _ -> t) in
            (S.set env ~key:id ~data:(Init t), S.set func_env ~key:id ~data:Deleted)
          end
      end

let decl_helper p1 p2 ext env =
  let (A.Param (_, t1)) = p1
  and (A.Param (_, t2)) = p2 in
  match compare_type t1 t2 env with
  | true -> ()
  | _ -> ErrorMsg.error ext "conflict param in function redeclaration\n";
        raise ErrorMsg.Error

let rec tc_header func_env type_env prog ext =
  match prog with
  | [] -> (func_env, type_env)
  | gdecl::rest ->
    match gdecl with
    | A.FDecl (typedef, id, params) ->
      begin
        start_env params type_env func_env;
        match S.find func_env id, S.find type_env id with
        | Some (FDecl (t, l)), None ->
          ErrorMsg.error ext "function declarations shouldn't be in map\n";
          raise ErrorMsg.Error
        | Some (FDefn (t, l)), None -> begin
            match compare_type t typedef type_env with
            | true -> (try List.iter2_exn ~f:(fun p1 p2 -> decl_helper p1 p2 ext type_env) params l
                       with Invalid_argument _ ->
                         ErrorMsg.error ext "unmatched param length in function redeclaration\n";
                         raise ErrorMsg.Error); tc_header func_env type_env rest ext
            | _ -> ErrorMsg.error ext "unmatched return types in function redeclaration\n";
              raise ErrorMsg.Error
          end
        | _, None ->
          (match check_voids params type_env with
          | true -> (let func_env =
                    S.set func_env ~key:id ~data:(FDefn (typedef, params))
                     in tc_header func_env type_env rest ext)
          | _ -> ErrorMsg.error ext "param has undefined type or type void\n";
                raise ErrorMsg.Error)
	| _, Some _ -> ErrorMsg.error ext ("function and type can't have the same name");
		raise ErrorMsg.Error
      end
    | A.FDefn _ -> ErrorMsg.error ext "function shouldn't be defined in header\n";
      raise ErrorMsg.Error
    | A.NewType (id, typedef) ->
      begin
        match S.find type_env id, S.find func_env id with
        | None, None ->
          begin
            match typedef with
            | A.IDENT ident -> (match S.find type_env ident with
                | Some t -> (let type_env = S.set type_env ~key:id ~data:t
                             in tc_header func_env type_env rest ext)
                | None -> ErrorMsg.error ext "type not defined\n";
                  raise ErrorMsg.Error)
            | A.VOID -> ErrorMsg.error ext "void typedef\n";
              raise ErrorMsg.Error
            | _ -> let type_env = S.set type_env ~key:id ~data:typedef
              in tc_header func_env type_env rest ext
          end
        | _, None ->
          ErrorMsg.error ext "type redeclaration\n";
          raise ErrorMsg.Error
        | None, _ ->
          ErrorMsg.error ext "type collides with func name\n";
          raise ErrorMsg.Error
      end
    | A.Main _ ->
      ErrorMsg.error ext "main function shouldn't be defined in header\n";
      raise ErrorMsg.Error
    | A.MainDecl _ ->
      ErrorMsg.error ext "main function shouldn't be defined in header\n";
      raise ErrorMsg.Error
    | A.Markedg marked_stm ->
      tc_header func_env type_env (Mark.data marked_stm :: rest) (Mark.ext marked_stm)


let checkheader h_prog = tc_header S.empty S.empty h_prog None

let check_func_env func_env =
  S.iter func_env
    ~f:(fun d -> match d with
        | FDeclUsed _ -> ErrorMsg.error None "function called but not defined";
          raise ErrorMsg.Error
        | _ -> ())

let rec tc_gdecls func_env type_env prog ext ret =
  (* printf "in tc_gdecl\n"; *)
  match prog with
  | [] -> check_func_env func_env; ret
  | gdecl::rest ->
    match gdecl with
    | A.FDecl (typedef, id, params) ->
      begin
        start_env params type_env func_env;
        match S.find func_env id, S.find type_env id with
        | Some (FDecl (t, l)), None -> begin
            match compare_type t typedef type_env with
            | true -> (try List.iter2_exn ~f:(fun p1 p2 -> decl_helper p1 p2 ext type_env) params l
                       with Invalid_argument _ ->
                         ErrorMsg.error ext "unmatched param length in function redeclaration\n";
                         raise ErrorMsg.Error); tc_gdecls func_env type_env rest ext ret
            | _ -> ErrorMsg.error ext "unmatched return types in function redeclaration\n";
            raise ErrorMsg.Error
          end
        | Some (FDefn (t, l)), None -> begin
            match compare_type t typedef type_env with
            | true -> (try List.iter2_exn ~f:(fun p1 p2 -> decl_helper p1 p2 ext type_env) params l
                       with Invalid_argument _ ->
                         ErrorMsg.error ext "unmatched param length in function redeclaration\n";
                         raise ErrorMsg.Error); tc_gdecls func_env type_env rest ext ret
            | _ -> ErrorMsg.error ext "unmatched return types in function redeclaration\n";
              raise ErrorMsg.Error
          end
        | Some (FDeclUsed (t, l)), None -> begin
            match compare_type t typedef type_env with
            | true -> (try List.iter2_exn ~f:(fun p1 p2 -> decl_helper p1 p2 ext type_env) params l
                       with Invalid_argument _ ->
                         ErrorMsg.error ext "unmatched param length in function redeclaration\n";
                         raise ErrorMsg.Error); tc_gdecls func_env type_env rest ext ret
            | _ -> ErrorMsg.error ext "unmatched return types in function redeclaration\n";
              raise ErrorMsg.Error
          end
        | _, None ->
          (match check_voids params type_env with
            | true -> (let func_env =
                         S.set func_env ~key:id ~data:(FDecl (typedef, params))
                       in tc_gdecls func_env type_env rest ext ret)
            | _ -> ErrorMsg.error ext "param has undefined type or type void\n";
              raise ErrorMsg.Error)
	      | _, Some _ -> ErrorMsg.error ext "same function and type name";
	               raise ErrorMsg.Error
      end
    | A.FDefn (typedef, id, params, stm) ->
      begin
        match S.find func_env id, S.find type_env id with
        | Some (FDecl (t, l)), None ->
            (let func_env = S.set func_env ~key:id ~data:(FDefn (t, l)) in
             match compare_type t typedef type_env with
            | true -> begin
                 let () = List.iter2_exn params l ~f:(fun p1 p2 -> decl_helper p1 p2 ext type_env)
                 in let (env, func_env') = start_env params type_env func_env in
                 let void_func = compare_type typedef A.VOID type_env in
                 match tc_stms env func_env' type_env stm ext VoidRet void_func with
                 | _, Ret t, new_env ->
                   begin
                     let func_env = intersect_func func_env new_env in
                     match compare_type t typedef type_env with
                     | true -> tc_gdecls func_env type_env rest ext ret
                     | _ -> ErrorMsg.error ext "function doesnt return compatible type\n";
                       raise ErrorMsg.Error
                   end
                 | _, _, new_env ->
                   begin
                     let func_env = intersect_func func_env new_env in
                     match compare_type A.VOID typedef type_env with
                     | true -> tc_gdecls func_env type_env rest ext ret
                     | _ -> ErrorMsg.error ext "function does not return\n";
                       raise ErrorMsg.Error
                   end
              end
            | _ -> ErrorMsg.error ext "unmatched return types in function redeclaration\n";
                raise ErrorMsg.Error)
        | Some (FDefn (t, l)), None ->
          ErrorMsg.error ext ("function redefinition\n");
         raise ErrorMsg.Error
        | Some (FDeclUsed (t, l)), None ->
            (let func_env = S.set func_env ~key:id ~data:(FDefn (t, l)) in
            match compare_type t typedef type_env with
            | true -> begin
                let () = List.iter2_exn params l ~f:(fun p1 p2 -> decl_helper p1 p2 ext type_env)
                in let (env, func_env') = start_env params type_env func_env in
                let void_func = compare_type typedef A.VOID type_env in
                match tc_stms env func_env' type_env stm ext VoidRet void_func with
                | _, Ret t, new_env ->
                  begin
                    let func_env = intersect_func func_env new_env in
                    match compare_type t typedef type_env with
                    | true -> tc_gdecls func_env type_env rest ext ret
                    | _ -> ErrorMsg.error ext "function doesnt return compatible type\n";
                      raise ErrorMsg.Error
                  end
                | _, _, new_env ->
                  begin
                    let func_env = intersect_func func_env new_env in
                    match compare_type A.VOID typedef type_env with
                    | true -> tc_gdecls func_env type_env rest ext ret
                    | _ -> ErrorMsg.error ext "function does not return\n";
                      raise ErrorMsg.Error
                  end
              end
            | _ -> ErrorMsg.error ext "unmatched return types in function redeclaration\n";
              raise ErrorMsg.Error)
        | _, None ->
          (match check_voids params type_env with
           | true ->
             begin
               let func_env =
                       S.set func_env ~key:id ~data:(FDefn (typedef, params))
               in let (env, func_env') = start_env params type_env func_env in
               let void_func = compare_type typedef A.VOID type_env in
               match tc_stms env func_env' type_env stm ext VoidRet void_func with
               | _, Ret t, new_env ->
                 begin
                   let func_env = intersect_func func_env new_env in
                   match compare_type t typedef type_env with
                   | true -> tc_gdecls func_env type_env rest ext ret
                   | _ -> ErrorMsg.error ext "function doesnt return compatible type\n";
                      raise ErrorMsg.Error
                 end
               | _, _, new_env ->
                 begin
                   let func_env = intersect_func func_env new_env in
                   match compare_type A.VOID typedef type_env with
                   | true -> tc_gdecls func_env type_env rest ext ret
                   | _ -> ErrorMsg.error ext "function does not return\n";
                   raise ErrorMsg.Error
                 end
             end
           | _ -> ErrorMsg.error ext "param has undefined type or type void\n";
             raise ErrorMsg.Error)
	      | _, Some _ -> ErrorMsg.error ext "same function and type name";
	               raise ErrorMsg.Error
      end
    | A.NewType (id, typedef) ->
      begin
        match S.find type_env id, S.find func_env id with
        | None, None ->
          begin
            match typedef with
            | A.IDENT ident -> (match S.find type_env ident with
              | Some t -> (let type_env = S.set type_env ~key:id ~data:t
                           in tc_gdecls func_env type_env rest ext ret)
              | None -> ErrorMsg.error ext "type not defined\n";
                raise ErrorMsg.Error)
      	    | A.VOID -> ErrorMsg.error ext "void typedef\n";
      	        raise ErrorMsg.Error
            | _ -> let type_env = S.set type_env ~key:id ~data:typedef
                    in tc_gdecls func_env type_env rest ext ret
          end
	      | _, None ->
         ErrorMsg.error ext "type redeclaration\n";
          raise ErrorMsg.Error
	      | None, _ ->
         ErrorMsg.error ext "type collides with func name\n";
          raise ErrorMsg.Error
      end
    | A.Main (t', stm) ->
      (match ret with
       | true -> ErrorMsg.error ext "main defined more than once in file\n";
         raise ErrorMsg.Error
       | _ ->
         begin
           match compare_type t' A.INT type_env with
           | true ->
             begin
 	       let func_env = S.set func_env ~key:(Symbol.symbol "main") ~data:(FDefn (A.INT, [])) in
               match tc_stms S.empty func_env type_env stm ext VoidRet false with
               | _, Ret t, new_env ->
                 begin
                   let func_env = intersect_func func_env new_env in
                   match compare_type t A.INT type_env with
                   | true -> tc_gdecls func_env type_env rest ext true
                   | _ -> ErrorMsg.error ext "main doesnt return int in block\n";
                   raise ErrorMsg.Error
                 end
               | _, _, func_env -> ErrorMsg.error ext "main does not return\n";
                 raise ErrorMsg.Error
             end
           | _ -> ErrorMsg.error ext "main definition doesnt have type int\n";
             raise ErrorMsg.Error
         end)
    | A.MainDecl t ->
      begin
        match compare_type t A.INT type_env with
        | true -> let func_env = S.set func_env ~key:(Symbol.symbol "main") ~data:(FDecl (A.INT, [])) in
	          tc_gdecls func_env type_env rest ext ret
        | _ -> ErrorMsg.error ext "main declaration doesnt have type int\n";
          raise ErrorMsg.Error
      end
    | A.Markedg marked_stm ->
      tc_gdecls func_env type_env (Mark.data marked_stm :: rest) (Mark.ext marked_stm) ret


let typecheck prog envs =
  let ret = begin
    match envs with
    | Some (func_env, type_env) ->
      let func_env = S.set func_env ~key:(Symbol.symbol "main") ~data:(FDecl (A.INT, [])) in
      tc_gdecls func_env type_env prog None false
    | None ->
      let func_env = S.set S.empty ~key:(Symbol.symbol "main") ~data:(FDecl (A.INT, [])) in
      tc_gdecls func_env S.empty prog None false
  end
  in if not ret then
  begin
    ErrorMsg.error None "no main function in source file\n";
    raise ErrorMsg.Error
  end
