(* L1 Compiler
 * IR Trees
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
*)

open Base

type bi = ADD | SUB | MUL | DIV | MOD | LES | LEQ | GRE | GEQ | IEQ | NEQ | LAND | LOR | BAND | BOR | BXOR | SLEFT | SRIGHT

(* type as = EQL | PEQ | MINEQ | MULEQ | DIVEQ | MODEQ | ANDEQ | XOREQ | OREQ | RSHEQ | LSHEQ *)

type un = LNOT | BNOT | NEGA

type label = Label.t

type exp =
  | VAR of string * string
  | CONST of Int32.t
  | BOOL of bool
  | FCONST of Float.t
  | TEMP of Temp.t
  | ARG of Arguement.t
  | BEGINARG of Arguement.t
  | BINOP of bi * exp * exp * Temp.t * Temp.t * string
  | UNOP of un * exp * Temp.t
  | QUESTION of exp * exp * exp * Temp.t * Temp.t * Temp.t * string
  | INTERNALCALL of string * exp list * string list * string
  | EXTERNALCALL of string * exp list
  | NULL
  | INLINEFUNCTIONCALL of string * exp list * Temp.t * Temp.t * Temp.t * Temp.t * Temp.t * Temp.t
  | ADDRPREP of exp * int * Temp.t
  | DOT of exp * int * Temp.t
  | RARROW of string * exp * int * string
  | RARROWWITHOUTLOAD of string * exp * int * string
  | RARROWWITHOUTCHECK of string * exp * int * string
  | RARROWWITHOUTCHECKANDLOAD of string * exp * int * string
  | MALLOC of int * string
  | DEREF of exp * string
  | DONOTHING of exp * Temp.t
  | ALLOCARRY of int * exp * string
  | ARRDEREF of exp * exp * string
  | ARRDONOTHING of exp * exp * string
  | INLINECALL of string * exp list * Temp.t list
  (* | PASSARG of Arguement.t * exp *)
and stm =
  | OPAQUE of string
  | ALLOC of string * string
  | JUSTWRITE of string
  | MOVE of exp * exp
  | MOVETOADDR of exp * exp * string
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
  | RETURN of exp * string
  | VOIDRETURN
  | IMPRETURN of exp * string
  | IMPVOIDRETURN of int
  | NOP
  | FUNCTIONCALL of string
  | FUNCDEF of string * string * string
  | MAINDEF of string * int
  | GOTO of Label.t
  | PASSARG of exp * exp
  | PASSARGTRUE of exp * exp * int
  | ARRADDRCOMP of Temp.t * exp * exp * int * Temp.t * Temp.t
  | ASSIGNARRFOUR of exp * exp * int * exp * Temp.t * Temp.t * Temp.t
  | ASSIGNARR of exp * exp * exp * string
  | ADDRASNOP of bi* Temp.t * exp * Temp.t * Temp.t
  | ADDRASNOPFOURBYTE of bi * Temp.t * exp * Temp.t * Temp.t
  | TAILRECURSIVE of string * exp list * Temp.t list * int
  | STORE of string * exp * string
  | STORESPECIAL of string * exp * string
  | JUSTCOMPUTE of Temp.t * exp
  | FUNCEND of string
  | MAINEND
  | STOREARG of string * int * string
  | STRUCTDEFN of string * string
  | DOTASSIGN of string * exp * int * exp * string
  | DOTASSIGNCHECK of string * exp * int * exp * string
  | DOTASNOP of bi * string * exp * int * string * exp
  | DEREFASNOP of bi * exp * exp * string
  | ARRASNOP of bi * exp * exp * exp * string
type program = stm list

module type PRINT =
sig
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end

module Print : PRINT
