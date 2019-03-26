(* L1 Compiler
 * IR Trees
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type bi = ADD | SUB | MUL | DIV | MOD | LES | LEQ | GRE | GEQ | IEQ | NEQ | LAND | LOR | BAND | BOR | BXOR | SLEFT | SRIGHT

(* type as = EQL | PEQ | MINEQ | MULEQ | DIVEQ | MODEQ | ANDEQ | XOREQ | OREQ | RSHEQ | LSHEQ *)

type un = LNOT | BNOT | NEGA

type label = Label.t

type exp =
  | CONST of Int32.t
  | BOOL of bool
  | TEMP of Temp.t
  | BINOP of bi * exp * exp
  | UNOP of un * exp
  | QUESTION of exp * exp * exp
and stm =
  | MOVE of exp * exp
  | IF of exp * stm list * stm list
  (* | WHILE of exp * stm *)
  (* | BRANCHEXP of label * exp * label *)
  (* | BRANCHSTM of label * stm * label
     | EMPTYBRANCH of label *)
  | WHILE of exp * stm list
  | RETURN of exp
  | NOP

type program = stm list

module type PRINT =
  sig
    val pp_exp : exp -> string
    val pp_stm : stm -> string
    val pp_program : program -> string
  end

module Print : PRINT
