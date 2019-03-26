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
  (* | OpExp of oper * exp list *)
  | Unop of unop * exp
  | Binop of binop * exp * exp
  | Question of exp * exp * exp
  | Marked of exp Mark.marked
and stm =
  | Assign of ident * exp
  | If of exp * stm * stm
  | While of exp * stm
  | Return of exp
  | Nop
  | Seq of stm * stm
  | Declare of decl * stm
  | Exp of exp
  | Markeds of stm Mark.marked
  | Block of stm
  | For of stm
and decl =
  | NewVar of ident * typedefine
  | Init of ident * typedefine * exp


type program = stm

(* print as source, with redundant parentheses *)
(* module type PRINT =
  sig
    val pp_exp : exp -> string
    val pp_stm : stm -> string
    val pp_program : program -> string
  end

module Print : PRINT  *)
