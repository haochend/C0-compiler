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

(* tc_exp : init_status Symbol.Map.t -> Ast.exp -> Mark.ext option -> unit *)
let rec tc_exp env ast ext t =
  match ast with
  | A.Var id ->
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
          | Some A.INT, A.INT -> Some A.BOOL
          | Some A.BOOL, A.BOOL -> Some A.INT
          | None, _ -> Some t'
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
      | _ -> Some A.INT
    end
  | A.BoolExp _ ->
    begin
      match t with
      | Some A.INT -> ErrorMsg.error ext
                   ("expecting a boolean, but an int was given");
        raise ErrorMsg.Error
      | _ -> Some A.BOOL
    end
  | A.Unop (oper, e) ->
    begin
      match oper with
      | A.LOGICALNOT ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> tc_exp env e ext (Some A.BOOL); (Some A.BOOL))
      | _ ->
        (match t with
         | Some A.BOOL -> ErrorMsg.error ext
                       ("expecting an int, but a boolean was given");
           raise ErrorMsg.Error
         | _ -> tc_exp env e ext (Some A.INT); (Some A.INT))
    end
  | A.Binop (oper, e1, e2) ->
    begin
      match oper with
      | A.LOGICALAND ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> tc_exp env e1 ext (Some A.BOOL); tc_exp env e2 ext (Some A.BOOL); (Some A.BOOL))
      | A.LOGICALOR ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> tc_exp env e1 ext (Some A.BOOL); tc_exp env e2 ext (Some A.BOOL); (Some A.BOOL))
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
                  let (Some t1) = tc_exp env e1 ext None
                  and (Some t2) = tc_exp env e2 ext None
                  in match (t1, t2) with
                  | A.INT, A.INT -> Some A.BOOL
                  | A.BOOL, A.BOOL -> Some A.BOOL
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
             let (Some t1) = tc_exp env e1 ext None
             and (Some t2) = tc_exp env e2 ext None
             in match (t1, t2) with
             | A.INT, A.INT -> Some A.BOOL
             | A.BOOL, A.BOOL -> Some A.BOOL
             | _ -> ErrorMsg.error ext
                 ("Type mismatch between two sides of equality");
             raise ErrorMsg.Error
           end)
      | A.LESSTHAN ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> tc_exp env e1 ext (Some A.INT); tc_exp env e2 ext (Some A.INT); Some A.BOOL)
      | A.LESSOREQ ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> tc_exp env e1 ext (Some A.INT); tc_exp env e2 ext (Some A.INT); Some A.BOOL)
      | A.GREATERTHAN ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> tc_exp env e1 ext (Some A.INT); tc_exp env e2 ext (Some A.INT); Some A.BOOL)
      | A.GREATEROREQ ->
        (match t with
         | Some A.INT -> ErrorMsg.error ext
                      ("expecting a boolean, but an int was given");
           raise ErrorMsg.Error
         | _ -> tc_exp env e1 ext (Some A.INT); tc_exp env e2 ext (Some A.INT); Some A.BOOL)
      | _ ->
        (match t with
         | Some A.BOOL -> ErrorMsg.error ext
                       ("expecting an int, but a boolean was given");
           raise ErrorMsg.Error
         | _ -> tc_exp env e1 ext (Some A.INT); tc_exp env e2 ext (Some A.INT); Some A.INT)
    end
  | A.Question (e1, e2, e3) ->
    begin
      tc_exp env e1 ext (Some A.BOOL);
      tc_exp env e2 ext t;
      tc_exp env e3 ext t
    end
  | A.Marked marked_exp ->
    tc_exp env (Mark.data marked_exp) (Mark.ext marked_exp) t

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

let rec tc_stms env ast ext ret =
  match ast with
  | A.Nop -> (env, ret)
  | A.Assign (id, e) ->
    begin
      match S.find env id with
      | None -> ErrorMsg.error ext ("undeclared variable `" ^ Symbol.name id ^ "'");
        raise ErrorMsg.Error
      | Some (Decl t) ->
        begin
          tc_exp env e ext (Some t);
          let env = S.set env ~key:(id) ~data:(Init t)
          in (env, false)
        end
      | Some (Init t) ->
        begin
          tc_exp env e ext (Some t);
          (env, false)
        end
    end
  | A.If (e, s1, s2) ->
    begin
      tc_exp env e ext (Some A.BOOL);
      let (env1, ret1) = tc_stms env s1 ext ret
      and (env2, ret2) = tc_stms env s2 ext ret
      in let new_env = intersect env1 env2
      in let new_env = S.mapi env ~f:(fun ~key:k ~data:d ->
          match S.find new_env k with
          | Some (Init t) -> (Init t)
          | _ -> d)
      in if ret1 && ret2 then (new_env, true) else (new_env, false)
    end
  | A.While (e, s) ->
    begin
      tc_exp env e ext (Some A.BOOL);
      tc_stms env s ext ret;
      (env, false)
    end
  | A.Return e ->
    begin
      tc_exp env e ext (Some A.INT);
      let new_env = S.map env
          ~f:(fun a -> match a with
              | (Decl t) -> (Init t)
              | _ -> a)
      in (new_env, true)
    end
  | A.Seq (s1, s2) ->
    begin
      let (env1, ret1) = tc_stms env s1 ext ret
      in let env1 = if ret1 then (S.map env1 ~f:(fun a -> match a with
          | (Decl t) -> (Init t)
          | _ -> a)) else env1
      in
      match s1 with
      | A.Block s ->
        (let intersect_env = S.mapi env ~f:(fun ~key:k ~data:d ->
             match S.find env1 k with
             | Some (Init t) -> (Init t)
             | _ -> d)
         in let (env2, ret2) = tc_stms intersect_env s2 ext ret
         in (env2, ret1 || ret2))
      | _ ->
        (let (env2, ret2) = tc_stms env1 s2 ext ret in (env2, ret1 || ret2))
    end
  | A.Declare (decl, s) ->
    begin
      match decl with
      | A.NewVar (id, t) ->
        begin
          match S.find env id with
          | Some _ -> ErrorMsg.error None ("redeclared variable `" ^ Symbol.name id);
            raise ErrorMsg.Error
          | None -> tc_stms (S.set env ~key:id ~data:(Decl t)) s ext ret
        end
      | A.Init (id, t, e) -> tc_stms env
                               (A.Declare(A.NewVar (id, t), A.Seq(A.Assign(id, e), s))) ext ret
    end
  | A.Markeds marked_stm ->
    tc_stms env (Mark.data marked_stm) (Mark.ext marked_stm) ret
  | A.Block stm -> tc_stms env stm ext ret
  | A.Exp e ->
    begin
      tc_exp env e ext (Some A.INT);
      (env, false)
    end
  | A.For s ->
    let (new_env, _) = tc_stms env s ext ret
    in let env = S.mapi env ~f:(fun ~key:k ~data:d ->
        match S.find new_env k with
        | Some (Init t) -> (Init t)
        | _ -> d)
    in (env, false)

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

let typecheck prog =
  let (_, ret) = tc_stms S.empty prog None false in
  if not ret
  then begin
    ErrorMsg.error None "main does not return\n";
    raise ErrorMsg.Error
  end
