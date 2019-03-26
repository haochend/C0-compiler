(* L1 Compiler
 * Assembly Code Generator for FAKE assembly
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Based on code by: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 *
 * Implements a "convenient munch" algorithm
 *)

open Core

module T = Tree
module AS = Assem

let as_binop = function
  (* | T.ADD -> AS.ADD
  | T.SUB -> AS.SUB
  | T.MUL -> AS.MUL
     | T.DIV -> AS.DIV

  | T.MOD -> AS.MOD *)
  | T.ADD -> AS.ADD
  | T.SUB -> AS.SUB
  | T.MUL -> AS.MUL
  | T.DIV -> AS.DIV
  | T.MOD -> AS.MOD
  | T.LES -> AS.LES
  | T.LEQ -> AS.LEQ
  | T.GRE -> AS.GRE
  | T.GEQ -> AS.GEQ
  | T.IEQ -> AS.IEQ
  | T.NEQ -> AS.NEQ
  | T.LAND -> AS.LAND
  | T.LOR -> AS.LOR
  | T.BAND -> AS.BAND
  | T.BXOR -> AS.BXOR
  | T.BOR -> AS.BOR
  | T.SLEFT -> AS.SLEFT
  | T.SRIGHT -> AS.SRIGHT

let as_unop = function
  | T.LNOT -> AS.LNOT
  | T.BNOT -> AS.BNOT
  | T.NEGA -> AS.NEGA
  (* munch_exp : AS.operand -> T.exp -> AS.instr list *)
(* munch_exp d e
 * generates instructions to achieve d <- e
 * d must be TEMP(t) or REG(r)
 *)
let rec munch_exp d = function
  | T.CONST n -> [AS.MOV (d, AS.IMM n)]
  | T.BOOL b -> [AS.MOV (d, AS.BOOL b)]
  | T.TEMP t -> [AS.MOV(d, AS.TEMP t)]
  | T.BINOP (binop, e1, e2) ->
      munch_binop d (binop, e1, e2)
  | T.UNOP (unop, e) ->
    munch_unop d (unop, e)
  | T.QUESTION (e1, e2, e3) ->
    munch_question d (e1, e2, e3)
  (* | T.QUESTION (e1, e2, e3) ->
      munch_question d (e1, e2, e3) *)
(* munch_binop : AS.operand -> T.binop * T.exp * T.exp -> AS.instr list *)
(* munch_binop d (binop, e1, e2)
 * generates instruction to achieve d <- e1 binop e2
 * d must be TEMP(t) or REG(r)
 *)
and munch_binop d (binop, e1, e2) =
    let operator = as_binop binop
    and t1 = AS.TEMP (Temp.create ())
    and t2 = AS.TEMP (Temp.create ()) in
    begin
      match operator with
      (* | AS.SLEFT ->
        begin
          match e2 with
          | T.CONST n->
            munch_exp t1 e1
            @ [AS.BINOP (operator, d, t1, AS.IMM n)]
          | _ ->
            munch_exp t1 e1
            @ munch_exp t2 e2
            @ [AS.BINOP (operator, d, t1, t2)]
        end
        (* munch_exp t2 e2
        @ [AS.BINOP (operator, d, AS.IMM e1, t2)] *)
      | AS.SRIGHT ->
        begin
          match e2 with
          | T.CONST n->
            munch_exp t1 e1
            @ [AS.BINOP (operator, d, t1,AS.IMM n)]
          | _ ->
            munch_exp t1 e1
            @ munch_exp t2 e2
            @ [AS.BINOP (operator, d, t1, t2)]
         end *)
      | AS.LAND ->
        begin
          let l1 = AS.LABEL (Label.create())
          and l2 = AS.LABEL (Label.create())
          and l3 = AS.LABEL (Label.create())
          and l4 = AS.LABEL (Label.create())
          and t1 = AS.TEMP (Temp.create())
          and t2 = AS.TEMP (Temp.create()) in
          munch_exp t1 e1
          @ [AS.CP (t1, l1, l3)]
          @ [AS.WRITELABEL (l1)]
          @ munch_exp t2 e2
          @ [AS.CP (t2, l3, l2)]
          @ [AS.WRITELABEL (l2)]
          @ [AS.MOV (d, t2)]
          @ [AS.GOTO (l4)]
          @ [AS.WRITELABEL (l3)]
          @ [AS.MOV(d, t1)]
          @ [AS.WRITELABEL (l4)]
        end
      | AS.LOR ->
        begin
          let l1 = AS.LABEL (Label.create())
          and l2 = AS.LABEL (Label.create())
          and l3 = AS.LABEL (Label.create())
          and l4 = AS.LABEL (Label.create())
          and t1 = AS.TEMP (Temp.create())
          and t2 = AS.TEMP (Temp.create()) in
          munch_exp t1 e1
          @ [AS.CP (t1, l3, l1)]
          @ [AS.WRITELABEL (l1)]
          @ munch_exp t2 e2
          @ [AS.CP (t2, l2, l3)]
          @ [AS.WRITELABEL (l2)]
          @ [AS.MOV (d, t2)]
          @ [AS.GOTO (l4)]
          @ [AS.WRITELABEL (l3)]
          @ [AS.MOV(d, t1)]
          @ [AS.WRITELABEL (l4)]
        end
      | _ ->
        munch_exp t1 e1
        @ munch_exp t2 e2
        @ [AS.BINOP (operator, d, t1, t2)]
    end
and munch_unop d (unop, e) =
  let operator = as_unop unop
  and t = AS.TEMP (Temp.create ()) in
  munch_exp t e
  @ [AS.UNOP (operator, d, t)]
and munch_question d (e1, e2, e3) =
  let l1 = AS.LABEL (Label.create())
  and l2 = AS.LABEL (Label.create())
  and l3 = AS.LABEL (Label.create())
  and t1 = AS.TEMP (Temp.create())
  and t2 = AS.TEMP (Temp.create()) in
  munch_exp t1 e1
  @ [AS.CP (t1, l1, l2)]
  @ [AS.WRITELABEL ( l1)]
  @ munch_exp t2 e2
  @ [AS.GOTO (l3)]
  @ [AS.WRITELABEL (l2)]
  @ munch_exp t2 e3
  @ [AS.GOTO (l3)]
  @ [AS.WRITELABEL (l3)]
  @ [AS.MOV(d, t2)]
   (*
and munch_if (e, s1, s2) =
  let l1 = AS.LABEL (Label.create())
  and l2 = AS.LABEL (Label.create())
  and l3 = AS.LABEL (Label.create()) in

  munch_exp e
  @ AS.IF
  @ AS.LABEL (l1, munch_stm s1, l3)
  @ AS.LABEL l3 *)


(* munch_stm : T.stm -> AS.instr list *)
(* munch_stm stm generates code to execute stm *)
let rec munch_stm = function
  | T.MOVE (T.TEMP t1, e2) -> munch_exp (AS.TEMP t1) e2
  | T.RETURN e ->
      (* return e is implemented as %eax <- e *)
    munch_exp (AS.REG AS.EAX) e
    @ [AS.RETURN]
  | T.IF (e1, s1, s2) ->
    let t= AS.TEMP (Temp.create())
    and l1 = AS.LABEL (Label.create())
    and l2 = AS.LABEL (Label.create())
    and l3 = AS.LABEL (Label.create()) in
    munch_exp t e1
    @ [AS.CP (t,l1, l2)]
    @ [AS.WRITELABEL l1]
    @ List.fold_left ~f:(fun a b -> a @ (munch_stm b)) ~init:([]) s1
    @ [AS.GOTO l3]
    @ [AS.WRITELABEL l2]
    @ List.fold_left ~f:(fun a b -> a @ (munch_stm b)) ~init:([]) s2
    @ [AS.GOTO l3]
    @ [AS.WRITELABEL l3]
  (* | T.BRANCHSTM (l1, s, l2) ->
    [AS.WRITELABEL (AS.LABEL l1)]
    @ munch_stm s
    @ [AS.GOTO (AS.LABEL l2)] *)
  (* | T.BRANCHEXP (l1, e, l2) ->
    [AS.WRITELABEL (AS.LABEL l1)]
    @ munch_exp e
    @ [AS.GOTO (AS.LABEL l2)] *)
  (* | T.EMPTYBRANCH l ->
     [AS.GOTO (AS.LABEL l)] *)
  | T.WHILE (e, s) ->
    let t= AS.TEMP (Temp.create())
    and l1 = AS.LABEL (Label.create())
    and l2 = AS.LABEL (Label.create())
    and l3 = AS.LABEL (Label.create()) in
    [AS.WRITELABEL l1]
    @ munch_exp t e
    @ [AS.CP (t,l2,l3)]
    @ [AS.WRITELABEL l2]
    @ List.fold_left ~f:(fun a b -> a @ (munch_stm b)) ~init:([]) s
    @ [AS.GOTO l1]
    @ [AS.WRITELABEL l3]
  | T.NOP ->
    []
  | _ -> assert false

let rec codegen = function
  | [] -> []
  | stm::stms -> munch_stm stm @ codegen stms
