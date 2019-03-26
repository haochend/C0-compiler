(* L1 Compiler
 * AST -> IR Translator
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

module A = Ast
module T = Tree
module S = Symbol.Map

let trans_oper = function
  | A.PLUS -> T.ADD
  | A.MINUS -> T.SUB
  | A.TIMES -> T.MUL
  | A.DIVIDEDBY -> T.DIV
  | A.MODULO -> T.MOD
  | A.NEGATIVE -> T.SUB                 (* unary to binary!*)

let rec trans_exp env = function
    (* after type-checking, id must be declared; do not guard lookup *)
  | A.Var id -> T.TEMP (S.find_exn env id)
  | A.ConstExp c -> T.CONST c
  | A.OpExp (oper, [e1; e2]) ->
      T.BINOP (trans_oper oper, trans_exp env e1, trans_exp env e2)
  | A.OpExp (A.NEGATIVE, [e]) ->
      T.BINOP (trans_oper A.NEGATIVE, T.CONST Int32.zero, trans_exp env e)
  | A.Marked marked_exp -> trans_exp env (Mark.data marked_exp)
  | _ -> assert false

(* translate the statement *)
(* trans_stms : Temp.temp Symbol.table -> A.stm list -> Tree.stm list *)
let rec trans_stms env = function
  | A.Declare d :: stms ->
    begin
      match d with
      | A.NewVar id -> trans_stms env stms
      | A.Init (id, e) -> trans_stms env (A.Assign (id, e) :: stms)
    end
  | A.Assign (id, e) :: stms ->
      let t = Temp.create () in
      let env' = S.set env ~key:id ~data:t in
      T.MOVE (T.TEMP t, trans_exp env e) :: trans_stms env' stms
  | A.Return e :: _ ->
      (* ignore code after return *)
      T.RETURN (trans_exp env e) :: []
  | A.Markeds marked_stm :: stms ->
      trans_stms env ((Mark.data marked_stm)::stms)
  | [] -> assert false                  (* There must be a return! *)

let translate stms = trans_stms S.empty stms
