(* L1 Compiler
 * AST -> IR Translator
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

open Core

module A = Ast
module T = Tree
module S = Symbol.Map

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

let rec trans_exp env func_env = function
    (* after type-checking, id must be declared; do not guard lookup *)
  | A.Var id -> T.TEMP (S.find_exn env id)
  | A.ConstExp c -> T.CONST c
  | A.BoolExp b -> T.BOOL b
  | A.Unop (unop, e) ->
    begin
      match unop with
      | A.NEGATIVE ->
        begin
          let t1 = Temp.create() in
          let t2 = Temp.create() in
          T.BINOP (trans_binop A.MINUS, T.CONST Int32.zero, trans_exp env func_env e,t1,t2)
        end
      | _ ->
        begin
          let t1 = Temp.create() in
          T.UNOP (trans_unop unop, trans_exp env func_env e,t1)
        end
    end

  | A.Binop (oper, e1, e2) ->
    let t1 = Temp.create() in
    let t2 = Temp.create() in
      T.BINOP (trans_binop oper, trans_exp env func_env e1, trans_exp env func_env e2, t1, t2)
  (* | A.OpExp (A.NEGATIVE, [e]) ->
      T.BINOP (trans_oper A.NEGATIVE, T.CONST Int32.zero, trans_exp env e) *)
  | A.Question (e1, e2, e3) ->
    (* let l1 = AS.LABEL (Label.create())
    and l2 = AS.LABEL (Label.create())
    and l3 = AS.LABEL (Label.create()) in
    T.IF (trans_exp e1, l1 , l2)
    @ T.BRANCHEXP (l1, trans_exp e2, l2)
    @ T.BRANCHEXP (l2, trans_exp e3, l3)
       @ T.EMPTYBRANCH (l3) *)
    begin
      let t1 = Temp.create() in
      let t2 = Temp.create() in
      T.QUESTION (trans_exp env func_env e1, trans_exp env func_env e2, trans_exp env func_env e3,t1,t2)
    end
  | A.Marked marked_exp -> trans_exp env func_env (Mark.data marked_exp)
  | A.Call (id, exps) ->
    begin
      Arguement.reset();
      let arg_list = List.fold_left ~f:(fun l e-> l@[Arguement.create()]) ~init:([]) exps in
      let transed_exps = List.map ~f:(fun e -> trans_exp env func_env e) exps in
      let exp_list = List.fold_left ~f:(fun l e-> [e]@l) ~init:([]) transed_exps in
      let temp_list = List.fold_left ~f:(fun l e-> [Temp.create()]@l) ~init:([]) transed_exps in
      begin
        match S.find func_env id with
        | Some (FDecl (t, l)) ->
          T.INTERNALCALL (Symbol.name id,
                          exp_list,
                            temp_list,
                          arg_list,Arguement.access())
      | None ->
        T.EXTERNALCALL (Symbol.name id,
                        exp_list,
                       temp_list,
                        arg_list,Arguement.access())
      end
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
let rec trans_stms env func_env= function
  | A.Declare (decl, stm) ->
    begin
      match decl with
      | A.NewVar (id, typedef) ->
        (* let () = printf "doing newvar decl \n" in  *)
        begin
          let t = Temp.create() in
          let env' = S.set env ~key:id ~data:t in
          trans_stms env' func_env stm
        end
        (* trans_stms env stm *)
      | A.Init (id, typedef, e) ->
        (* let () = printf "doing init decl \n" in  *)
        begin
          let t = Temp.create() in
          let env' = S.set env ~key:id ~data:t in
          trans_stms env' func_env (A.Seq ((A.Assign (id, e)), stm))
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
  | A.Assign (id, e) ->
    (* begin
      match S.find env id with
      | None -> (let t = Temp.create () in
                 (* let () = printf ("doing assignment to\n") in *)
                 (* let () = printf (Temp.name t) in *)
                 let env' = S.set env ~key:id ~data:t in
                 ([T.MOVE (T.TEMP t, trans_exp env' e)], env'))
      | Some t -> ([T.MOVE (T.TEMP t, trans_exp env e)], env)
    end *)
    ([T.MOVE (T.TEMP (S.find_exn env id), trans_exp env func_env e)], env)

  | A.Return e ->
    (* ignore code after return *)
    (* let () = printf "doing return \n" in *)
    ([T.RETURN (trans_exp env func_env e)], env)
  | A.VoidReturn ->
    ([T.VOIDRETURN],env)
  | A.Seq (stm1, stm2) ->

    let (l1, new_env) = trans_stms env func_env stm1
    in let (l2, new_env') = trans_stms new_env func_env stm2
    in (l1@l2, new_env')
      (* trans_stms env stm2 *)
      (* trans_stms env (stm1::stm2::stms) *)
  | A.Nop ->
    ([T.NOP], env)
      (* trans_stms env stms *)
  | A.If (e, s1, s2) ->
    let (l1, env1) = trans_stms env func_env s1
    and (l2, env2) = trans_stms env func_env s2 in
    let t = Temp.create() in
    (* ([T.IF (trans_exp env func_env e, l1, l2,t)], env1) *)
    let label1 = (Label.create())
    and label2 = (Label.create())
    and label3 = (Label.create()) in
    ([T.IF (trans_exp env func_env e, label1, label2,t)]
     @[T.WRITELABEL label1]
     @l1
     @[T.GOTO label3]
     @[T.WRITELABEL label2]
     @l2
     @[T.FINISHIF label3],env1)
  | A.While (e, s)->
      (* let l1 = A.LABEL (Label.create())
      and l2 = A.LABEL (Label.create())
      and l3 = A.LABEL (Label.create()) in
      T.EMPTYBRANCH l1
      @ T.IF (trans_exp env e, l2, l3)
      @ T.BRANCHSTM (l2, trans_stms env s, l1)
         @ T.EMPTYBRANCH l3 *)
    let (l1, newenv) = trans_stms env func_env s in
    let t = Temp.create() in
    let label1 = (Label.create())
    and label2 = (Label.create())
    and label3 = (Label.create()) in
    ([T.WRITELABEL label1]
     @[T.IF (trans_exp newenv func_env e,label2,label3,t)]
     @[T.WRITELABEL label2]
     @l1
     @[T.FINISHWHILE (label1,label3)],newenv)
    (* ([T.WHILE (trans_exp newenv func_env e, l1,t)], newenv) *)
  (* | A.Exp e ->
      assert false *)
  | A.Markeds marked_stm->
    trans_stms env func_env (Mark.data marked_stm)
  | A.Block stm -> trans_stms env func_env stm
  | A.Assert e ->
    begin
      let t = Temp.create() in
      ([T.ASSERT (trans_exp env func_env e, t)], env)
    end
  | A.Exp e ->
    let t = Temp.create() in
    (* let env' = S.set env ~key:id ~data:t in
    trans_stms env' (A.Seq ((A.Assign (id, e)), stm)) *)
    ([T.MOVE (T.TEMP t, trans_exp env func_env e)], env)
(* ([trans_exp env e], env) *)
  | A.For s -> trans_stms env func_env s
  | _ ->
    (* let () = printf "we got not found too \n" in  *)
    assert false                  (* There must be a return! *)

(* let translate stms = let (l, env) = trans_stms S.empty stms in l *)

let get_first param =
  let (A.Param (e1, e2)) = param in e1


let get_second param =
  let (A.Param (e1, e2)) = param in e2


    (* | FDecl of typedefine * ident * param list
    | FDefn of typedefine * ident * param list * stm
    | NewType of ident * typedefine
    | Main of typedefine * stm
    | MainDecl of typedefine
    | Markedg of gdecl Mark.marked *)


let trans_gdecl func_env = function
  | A.Main (typedef, stm) ->
    begin
      Temp.reset();
      Arguement.reset();
    let func_env = S.set func_env ~key:(Symbol.symbol "main") ~data:(FDecl(A.INT,[])) in
    let (l,env') = trans_stms S.empty func_env stm in
    let cnt = Temp.access() in
    let anotherl = List.fold_left ~f:(fun b a ->
      begin
        match a with
        | T.RETURN e -> b@[T.IMPRETURN (e,cnt)]
        | T.VOIDRETURN -> b@[T.IMPVOIDRETURN cnt]
        | _ -> b@[a]
      end
      ) ~init:([]) l in
    (* Arguement.reset(); *)

    ([(T.MAINDEF ("_c0_main",cnt))]
    @anotherl,func_env)
  end
  | A.MainDecl typedef -> ([],func_env)
  | A.FDecl (typedef, id, nvs)->
    let func_env = S.set func_env ~key:(id) ~data:(FDecl(A.INT, [])) in
    ([],func_env)
  | A.NewType (id, typedef)-> ([],func_env)
  | A.FDefn (t, id, nvs,stm) ->
    begin
      Temp.reset();
      Arguement.reset();
      let (argl,env) = List.fold_left ~f:(fun (l,e) nv->
          let t=Temp.create() in
          let a=Arguement.create() in
          let env = S.set e ~key:(get_first nv) ~data:t
          in (l@[T.PASSARG (T.TEMP t, T.BEGINARG a)],env)
        ) ~init:(([],S.empty)) nvs in
      let argcnt = Arguement.access() in
      let func_env = S.set func_env ~key:(id) ~data:(FDecl(A.INT, [])) in
      let (l,env') = trans_stms env func_env stm in
      let temp_cnt = Temp.access() in
      let anotherl = List.fold_left ~f:(fun b a ->
          begin
            match a with
            | T.RETURN e -> b@[T.IMPRETURN (e,temp_cnt)]
            | T.VOIDRETURN -> b@[T.IMPVOIDRETURN temp_cnt]
            | _ -> b@[a]
          end
        ) ~init:([]) l in
      let argltrue = List.fold_left ~f:(fun b a ->
        begin
          match a with
          | T.PASSARG (t,arg) -> b@[T.PASSARGTRUE (t,arg,argcnt)]
          | _ -> b@[a]
        end) ~init:([]) argl in
      begin
        match t with
        | A.VOID -> ([(T.FUNCDEF (Symbol.name id, temp_cnt))]
                     @argltrue
                     @anotherl
                     @[(T.IMPVOIDRETURN temp_cnt)],func_env)
        | _ -> ([(T.FUNCDEF (Symbol.name id, temp_cnt))]
                @argltrue
                @anotherl,func_env)
      end

    end
  | _ -> ([],func_env)
  (*
  and gdecl =
  | FDecl of typedefine * ident * NewVar list
  | FDefn of typedefine * ident * NewVar list * stm
  | NewType of ident * typedefine *)

let rec translate_gdecl gdecls env=
  begin
    match gdecls with
    | [] -> ([],env)
    | gdecl::right ->
      let (stms1, env1) = (trans_gdecl env gdecl) in
      let (stms2, env2) = (translate_gdecl right env1) in
      (stms1@stms2,env2)
  end


  (* let (l,env) = trans_gdecl S.empty gdecls in l *)
