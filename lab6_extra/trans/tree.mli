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
  | ARG of Arguement.t
  | BEGINARG of Arguement.t
  | BINOP of bi * exp * exp * Temp.t * Temp.t
  | UNOP of un * exp * Temp.t
  | QUESTION of exp * exp * exp * Temp.t * Temp.t * Temp.t
  | INTERNALCALL of string * exp list * Temp.t list * Arguement.t list * int
  | EXTERNALCALL of string * exp list * Temp.t list * Arguement.t list * int
  | NULL
  | INLINEFUNCTIONCALL of string * exp list * Temp.t * Temp.t * Temp.t * Temp.t * Temp.t * Temp.t
  | ADDRPREP of exp * int * Temp.t
  | DOT of exp * int * Temp.t
  | RARROW of exp * int * Temp.t
  | MALLOC of int
  | DEREF of exp * Temp.t
  | DONOTHING of exp * Temp.t
  | ALLOCARRY of int * exp * Temp.t
  | ARRDEREF of exp * exp * int * Temp.t * Temp.t
  | ARRDONOTHING of exp * exp * int * Temp.t * Temp.t
  | INLINECALL of string * exp list * Temp.t list
  (* | PASSARG of Arguement.t * exp *)
and stm =
  | JUSTWRITE of string
  | MOVE of exp * exp
  | MOVETOADDR of Temp.t * exp * Temp.t
  | MOVETOADDRFOURBYTE of Temp.t * exp * Temp.t
  | MOVETOADDRSHORTER of Temp.t * int
  | JUSTADDADDR of Temp.t * int
  | MOVEEXPTOADDR of exp * int * exp * Temp.t * Temp.t
  | CREATEBASEADDR of Temp.t * Temp.t
  | FINISHIF of Label.t
  | WRITELABEL of Label.t
  | IF of exp * Label.t * Label.t * Temp.t
  | FINISHWHILE of Label.t * Label.t
  (* | WHILE of exp * stm *)
  (* | BRANCHEXP of label * exp * label *)
  (* | BRANCHSTM of label * stm * label
     | EMPTYBRANCH of label *)
  (* | WHILE of exp * stm list * Temp.t *)
  | ASSERT of exp * Temp.t
  | RETURN of exp
  | VOIDRETURN
  | IMPRETURN of exp * int
  | IMPVOIDRETURN of int
  | NOP
  | FUNCTIONCALL of string
  | FUNCDEF of string * int
  | MAINDEF of string * int
  | GOTO of Label.t
  | PASSARG of exp * exp
  | PASSARGTRUE of exp * exp * int
  | ARRADDRCOMP of Temp.t * exp * exp * int * Temp.t * Temp.t
  | ASSIGNARRFOUR of exp * exp * int * exp * Temp.t * Temp.t * Temp.t
  | ASSIGNARR of exp * exp * int * exp * Temp.t * Temp.t * Temp.t
  | ADDRASNOP of bi* Temp.t * exp * Temp.t * Temp.t
  | ADDRASNOPFOURBYTE of bi * Temp.t * exp * Temp.t * Temp.t
  | TAILRECURSIVE of string * exp list * Temp.t list * int * Label.t
type program = stm list

module type PRINT =
  sig
    val pp_exp : exp -> string
    val pp_stm : stm -> string
    val pp_program : program -> string
  end

module Print : PRINT
