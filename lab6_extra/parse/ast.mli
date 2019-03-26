(* L1 Compiler
 * Abstract Syntax Trees
 * Author: Alex Vaynberg
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Forward compatible fragment of C0
*)

open Base

type ident = Symbol.t
(*
type oper =
 | PLUS
 | MINUS
 | TIMES
 | DIVIDEDBY
 | MODULO
 | NEGATIVE                     (* unary minus *)
type exp =
 | Var of ident
 | ConstExp of Int32.t
 | OpExp of oper * exp list
 | Marked of exp Mark.marked
and stm =
 | Declare of decl
 | Assign of ident * exp
 | Return of exp
 | Markeds of stm Mark.marked
and decl =
 | NewVar of ident
 | Init of ident * exp *)

type empty =
  | EMPTY

type typedefine =
  | INT
  | BOOL
  | VOID
  | FLOAT
  | IDENT of ident
  | POINTER of typedefine
  | ARRAY of typedefine
  | STRUCT of ident
  (* type oper =
     | PLUS
     | MINUS
     | TIMES
     | DIVIDEDBY
     | MODULO
     | NEGATIVE                     (* unary minus *) *)
(* type asop =
   | EQUAL
   | PLUSEQ
   | MINUSEQ
   | MULTEQ
   | DIVEQ
   | MODEQ
   | ANDEQ
   | XOREQ
   | OREQ
   | RSHIFTEQ
   | LSHIFTEQ *)
type binop =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDEDBY
  | MODULO
  | LESSTHAN
  | LESSOREQ
  | GREATERTHAN
  | GREATEROREQ
  | ISEQ
  | NOTEQ
  | LOGICALAND
  | LOGICALOR
  | BINARYAND
  | BINARYXOR
  | BINARYOR
  | SHIFTLEFT
  | SHIFTRIGHT
type unop =
  | LOGICALNOT
  | BINARYNOT
  | NEGATIVE
  (* type postop =
     | PLUSPLUS
     | MINUSMINUS *)

type exp =
  | Var of ident
  | ConstExp of Int32.t
  | BoolExp of bool
  | FConstExp of Float.t
  (* | OpExp of oper * exp list *)
  | Unop of unop * exp
  | Binop of binop * exp * exp
  | Question of exp * exp * exp
  | Marked of exp Mark.marked
  | Call of ident * exp list
  | Null
  (* | Dot of exp * ident *)
  (* | RArrow of exp * ident *)
  | Alloc of typedefine
  (* | Deref of exp *)
  | AllocArray of typedefine * exp
  | LDot of exp * ident
  | LRArrow of exp * ident
  | LDeref of exp
  | LArrDeref of exp * exp
and stm =
  | Assign of exp * exp
  | If of exp * stm * stm
  | While of exp * stm
  | Return of exp
  | VoidReturn
  | Nop
  | Seq of stm * stm
  | Declare of decl * stm
  | Exp of exp
  | Markeds of stm Mark.marked
  | Block of stm
  | For of stm
  | Assert of exp
  | Asnop of exp * binop * exp
and decl =
  | NewVar of ident * typedefine
  | Init of ident * typedefine * exp
and param = Param of ident * typedefine
and gdecl =
  | FDecl of typedefine * ident * param list
  | FDefn of typedefine * ident * param list * stm
  | NewType of ident * typedefine
  | Main of typedefine * stm
  | MainDecl of typedefine
  | Markedg of gdecl Mark.marked
  | SDecl of ident
  | SDef of ident * param list

type program = gdecl list



(* print as source, with redundant parentheses *)
(* module type PRINT =
   sig
    val pp_exp : exp -> string
    val pp_stm : stm -> string
    val pp_program : program -> string
   end
   module Print : PRINT  *)
