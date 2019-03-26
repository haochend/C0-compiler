(* L1 Compiler
 * Assembly language
 * Author: Kaustuv Chaudhuri <kaustuv+@andrew.cmu.edu>
 * Modified By: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Currently just a pseudo language with 3-operand
 * instructions and arbitrarily many temps
 *
 * We write
 *
 * BINOP  operand1 <- operand2,operand3
 * MOV    operand1 <- operand2
 *
 *)
type reg = EDX | RSP | EAX | ECX | EBX | ESI | EDI | R8D | R9D | R12D | R13D | R14D | R15D | STACK

type if_in_stack = T | F

type operand =
  | IMM of Int32.t
  | BOOL of bool
  | REG of reg
  | TEMP of Temp.t
  | ARGUEMENT of Arguement.t
  | BEGINARG of Arguement.t
  | OFFSET of Int32.t

type label =
    | LABEL of Label.t

type binop = ADD | SUB | MUL | DIV | MOD | LES | LEQ | GRE | GEQ | IEQ | NEQ | LAND | LOR | BAND | BOR | BXOR | SLEFT | SRIGHT


type unop = LNOT | BNOT | NEGA

type instr =
  | BINOP of binop * operand * operand * operand
  (* | IF of label * label * label *)
  | UNOP of unop * operand * operand
  (* | QUESTION of operand * operand *)
  | CP of operand * label * label
  | GOTO of label
  | WRITELABEL of label
  | MOV of operand * operand
  | MOVARGBEFORECALL of operand * operand
  | DIRECTIVE of string
  | COMMENT of string
  | RETURN
  | FUNCSTART of string
  | MAINSTART of string
  | INTERNALCALL of string
  | EXTERNALCALL of string * int
  | FUNCTIONABORT
  | ARGOFFSET of operand * int
  | ARGONSET of operand * int
  | STACKOFFSET of operand * int
  | STACKONSET of operand * int
  | PASS of operand * operand
  | PUSH of operand
  | POP of operand
  | PASSARG of operand * operand * operand
  | LEA of operand * int * label
  | DOTDEREF of operand * operand * label
  | MALLOC of operand * int
  | ALLOCARRY of operand * int * operand * label
  | ARRDEREF of operand * operand * operand * int * label * label * label
  | ARRDONOTHING of operand * operand * operand * int * label * label * label
  | NULL of operand
  | MOVETOADDR of operand * operand * label
  | MOVETOADDRFOURBYTE of operand * operand * label
  | GETCONTENT of operand * operand
  | GETCONTENTFOURBYTE of operand * operand
  | ARRADDRCOMP of operand * operand * operand * int * label * label * label
  | CALLRAISE

val format : instr -> string
