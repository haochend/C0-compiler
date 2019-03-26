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

let munch_op = function
  | T.ADD -> AS.ADD
  | T.SUB -> AS.SUB
  | T.MUL -> AS.MUL
  | T.DIV -> AS.DIV
  | T.MOD -> AS.MOD

(* munch_exp : AS.operand -> T.exp -> AS.instr list *)
(* munch_exp d e
 * generates instructions to achieve d <- e
 * d must be TEMP(t) or REG(r)
 *)
let rec munch_exp d = function
  | T.CONST n -> [AS.MOV (d, AS.IMM n)]
  | T.TEMP t -> [AS.MOV(d, AS.TEMP t)]
  | T.BINOP (binop, e1, e2) ->
      munch_binop d (binop, e1, e2)

(* munch_binop : AS.operand -> T.binop * T.exp * T.exp -> AS.instr list *)
(* munch_binop d (binop, e1, e2)
 * generates instruction to achieve d <- e1 binop e2
 * d must be TEMP(t) or REG(r)
 *)
and munch_binop d (binop, e1, e2) =
    let operator = munch_op binop
    and t1 = AS.TEMP (Temp.create ())
    and t2 = AS.TEMP (Temp.create ()) in
    munch_exp t1 e1
    @ munch_exp t2 e2
    @ [AS.BINOP (operator, d, t1, t2)]


(* munch_stm : T.stm -> AS.instr list *)
(* munch_stm stm generates code to execute stm *)
let munch_stm = function
  | T.MOVE (T.TEMP t1, e2) -> munch_exp (AS.TEMP t1) e2
  | T.RETURN e ->
      (* return e is implemented as %eax <- e *)
      munch_exp (AS.REG AS.EAX) e
  | _ -> assert false

let rec codegen = function
  | [] -> []
  | stm::stms -> munch_stm stm @ codegen stms
