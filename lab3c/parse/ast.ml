(* L1 Compiler
 * Abstract Syntax Trees
 * Author: Alex Vaynberg
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 *
 * Forward compatible fragment of C0
*)

(* Consider using pretty printing *)

open Core

type ident = Symbol.t

type empty =
  | EMPTY

type typedefine =
  | INT
  | BOOL
  | VOID
  | IDENT of ident
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
  | Call of ident * exp list
and stm =
  | Assign of ident * exp
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

type program = gdecl list
(* and decl =
  | NewVar of ident
  | Init of ident * type * exp
   (*
and simp =
  | Declare of decl
  | Assign of ident * exp
  | Exp of exp
  | Post of ident * postop
and simpopt =
  | Emptysimp of empty
  | Simpopt of simp
and elseopt =
  | Emptyelse of empty
  | Stm of stm
and control =
  | If of exp * stm * elseopt
  | While of exp * stm
  | For of simpopt * exp * simpopt * stm
  | Return of exp *)

*)

(* type program = stm *)
(*
module type PRINT =
sig
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end

module Print : PRINT =
  struct

  let pp_ident id = Symbol.name id
(*
  let pp_control = function *)



  let pp_oper = function
      | _ -> ""
      | PLUS -> "+"
      | MINUS -> "-"
      | TIMES -> "*"
      | DIVIDEDBY -> "/"
      | MODULO -> "%"
      (* | NEGATIVE -> "-" *)

  let rec pp_exp = function
    | _ -> ""
    | Var id     -> pp_ident id
    | ConstExp c -> Int32.to_string c
    (* | OpExp (op, [e]) -> pp_oper op ^ "(" ^ pp_exp e ^ ")" *)
    (* | OpExp (op, [e1; e2]) ->
      "(" ^ pp_exp e1 ^ " " ^ pp_oper op ^ " " ^ pp_exp e2 ^ ")" *)
    | Marked exp -> pp_exp (Mark.data exp)
    | _ -> assert false

  let rec pp_stm = function
    | _ -> ""
    (* | Declare d -> pp_decl d *)
    | Assign (id, e) -> pp_ident id ^ " = " ^ pp_exp e ^ ";"
    | Return e -> "return " ^ pp_exp e ^ ";"
    | Markeds stm -> pp_stm (Mark.data stm)
  and pp_stms = function
    | _ -> ""
    | [] -> ""
    | s :: ss -> pp_stm s ^ "\n" ^ pp_stms ss
  and pp_decl = function
    | _ -> ""
    (* | NewVar (t, id) -> pp_type t ^ " " ^ pp_ident id ^ ";" *)
    (* | Init (id, t, e) -> pp_type t ^ " " ^ pp_stm (Assign (id, e)) *)

  let pp_program stms = "{\n" ^ pp_stms stms ^ "}"
end *)
