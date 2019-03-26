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
type reg = EDX | RSP | EAX | ECX | EBX | ESI | EDI | R8D | R9D | R11D | R12D | R13D | R14D | R15D | STACK

type if_in_stack = T | F

type operand =
  | NULLP
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
  | BINOP of binop * operand * operand * operand * Ast.typedefine
  | CLEANBINOP of binop * operand * operand * operand * Ast.typedefine
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
  | RETURN of operand * string
  | VOIDRETURN
  | FUNCSTART of string * string * string
  | MAINSTART of string
  | INTERNALCALL of operand * string * string * string
  | EXTERNALCALL of operand * string * string * string
  | FUNCTIONCALL of string
  | FUNCTIONABORT
  | ARGOFFSET of operand * int
  | ARGONSET of operand * int
  | STACKOFFSET of operand * int
  | STACKONSET of operand * int
  | PASS of operand * operand
  | PUSH of operand
  | POP of operand
  | PASSARG of operand * operand * operand
  | LEA of operand * int * label * label
  | MOVEEXPTOADDR of operand * operand * operand * label * label
  | DOTDEREF of operand * operand * label
  | MALLOC of operand * int * string
  | INLINEFUNCTIONCALL of string * operand * operand * operand * operand * operand * operand * operand
  | ALLOCARRY of operand * int * operand * label
  | ARRDEREF of operand * operand * operand * int * label * label * label
  | ARRDEREFSHORTCUT of operand * operand * operand * int
  | ARRDONOTHING of operand * operand * operand * int * label * label * label
  | ARRDONOTHINGSHORTCUT of operand * operand * operand
  | NULL of operand
  | MOVETOADDR of operand * operand * string
  | MOVETOADDRFOURBYTE of operand * operand * label
  | GETCONTENT of operand * operand
  | GETCONTENTFOURBYTE of operand * operand
  | ARRADDRCOMP of operand * operand * operand * int * label * label * label
  | ARRADDRCOMPSHORT of operand * operand * operand
  | CALLRAISE
  |  QUESTION of operand * operand * operand * operand
  | NOP
  | JUSTWRITE of string
  | JMP of string * int
  | ALLOC of string * string
  | STORE of string * operand * string
  | VAR of string * operand * string
  | PHI of operand * operand * label * operand * label * Ast.typedefine
  | STOREARG of string * int * string
  | FUNCEND of string
  | LOAD of operand * operand * string

val format : instr -> string
val format_operand : operand -> string
