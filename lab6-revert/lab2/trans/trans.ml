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

let rec trans_exp env = function
    (* after type-checking, id must be declared; do not guard lookup *)
  | A.Var id -> T.TEMP (S.find_exn env id)
  | A.ConstExp c -> T.CONST c
  | A.BoolExp b -> T.BOOL b
  | A.Unop (unop, e) ->
    begin
      match unop with
      | A.NEGATIVE -> T.BINOP (trans_binop A.MINUS, T.CONST Int32.zero, trans_exp env e)
      | _ -> T.UNOP (trans_unop unop, trans_exp env e)
    end

  | A.Binop (oper, e1, e2) ->
      T.BINOP (trans_binop oper, trans_exp env e1, trans_exp env e2)
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

    T.QUESTION (trans_exp env e1, trans_exp env e2, trans_exp env e3)
  | A.Marked marked_exp -> trans_exp env (Mark.data marked_exp)
  | _ -> assert false
(*
  stm =
    | If of exp * stm * stm
    | While of exp * stm*)


(* translate the statement *)
(* trans_stms : Temp.temp Symbol.table -> A.stm list -> Tree.stm list *)
let rec trans_stms env = function
  | A.Declare (decl, stm) ->
    begin
      match decl with
      | A.NewVar (id, typedef) ->
        (* let () = printf "doing newvar decl \n" in  *)
        begin
          let t = Temp.create() in
          let env' = S.set env ~key:id ~data:t in
          trans_stms env' stm
        end
        (* trans_stms env stm *)
      | A.Init (id, typedef, e) ->
        (* let () = printf "doing init decl \n" in  *)
        begin
          let t = Temp.create() in
          let env' = S.set env ~key:id ~data:t in
          trans_stms env' (A.Seq ((A.Assign (id, e)), stm))
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
    ([T.MOVE (T.TEMP (S.find_exn env id), trans_exp env e)], env)

  | A.Return e ->
    (* ignore code after return *)
    (* let () = printf "doing return \n" in *)
    ([T.RETURN (trans_exp env e)], env)
  | A.Seq (stm1, stm2) ->

    let (l1, new_env) = trans_stms env stm1
    in let (l2, new_env') = trans_stms new_env stm2
    in (l1@l2, new_env')
      (* trans_stms env stm2 *)
      (* trans_stms env (stm1::stm2::stms) *)
  | A.Nop ->
    ([T.NOP], env)
      (* trans_stms env stms *)
  | A.If (e, s1, s2) ->
      (* let l1 = A.LABEL (Label.create())
      and l2 = A.LABEL (Label.create())
      and l3 = A.LABEL (Label.create()) in

      (* let t = Temp.create () in
      let env' = S.set env ~key:id ~data:t in

      T.MOVE (T.TEMP t, trans_exp env e) *)
      T.IF (trans_exp env e, l1 , l2)
      @ T.BRANCHSTM (l1, trans_stms s1, l2)
      @ T.BRANCHSTM (l2, trans_stms s2, l3)
         @ T.BRANCH (l3) *)
    let (l1, env1) = trans_stms env s1
    and (l2, env2) = trans_stms env s2 in
    ([T.IF (trans_exp env e, l1, l2)], env1)
  | A.While (e, s)->
      (* let l1 = A.LABEL (Label.create())
      and l2 = A.LABEL (Label.create())
      and l3 = A.LABEL (Label.create()) in
      T.EMPTYBRANCH l1
      @ T.IF (trans_exp env e, l2, l3)
      @ T.BRANCHSTM (l2, trans_stms env s, l1)
         @ T.EMPTYBRANCH l3 *)
    let (l1, newenv) = trans_stms env s
    in ([T.WHILE (trans_exp newenv e, l1)], newenv)
  (* | A.Exp e ->
      assert false *)
  | A.Markeds marked_stm->
    trans_stms env (Mark.data marked_stm)
  | A.Block stm -> trans_stms env stm
  | A.Exp e ->
    let t = Temp.create() in
    (* let env' = S.set env ~key:id ~data:t in
    trans_stms env' (A.Seq ((A.Assign (id, e)), stm)) *)
    ([T.MOVE (T.TEMP t, trans_exp env e)], env)
(* ([trans_exp env e], env) *)
  | A.For s -> trans_stms env s
  | _ ->
    (* let () = printf "we got not found too \n" in  *)
    assert false                  (* There must be a return! *)

let translate stms = let (l, env) = trans_stms S.empty stms in l
