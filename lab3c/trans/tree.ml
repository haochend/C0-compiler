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

(* type post = PLUSPLUS | MINUSMINUS *)

(* type control = IF | WHILE | FOR *)

type exp =
  | CONST of Int32.t
  | BOOL of bool
  | TEMP of Temp.t
  | ARG of Arguement.t
  | BEGINARG of Arguement.t
  | BINOP of bi * exp * exp * Temp.t * Temp.t
  | UNOP of un * exp * Temp.t
  | QUESTION of exp * exp * exp * Temp.t * Temp.t
  | INTERNALCALL of string * exp list * Temp.t list * Arguement.t list * int
  | EXTERNALCALL of string * exp list * Temp.t list * Arguement.t list * int
  (* | PASSARG of Arguement.t * exp *)
and stm =
  | MOVE of exp * exp
  (* | IF of exp * label * label *)
  | FINISHIF of Label.t
  | WRITELABEL of Label.t
  | IF of exp * Label.t * Label.t * Temp.t
  | FINISHWHILE of Label.t * Label.t
  (* | WHILE of exp * stm list * Temp.t *)
  | ASSERT of exp * Temp.t
  (* | WHILE of exp * stm *)
  (* | BRANCHEXP of label * exp * label *)
  (* | BRANCHSTM of label * stm * label
  | EMPTYBRANCH of label *)
  | RETURN of exp
  | VOIDRETURN
  | IMPRETURN of exp * int
  | IMPVOIDRETURN of int
  | NOP
  | FUNCDEF of string * int
  | MAINDEF of string * int
  | GOTO of Label.t
  | PASSARG of exp * exp
  | PASSARGTRUE of exp * exp * int
type program = stm list

module type PRINT =
  sig
    val pp_exp : exp -> string
    val pp_stm : stm -> string
    val pp_program : program -> string
  end

module Print : PRINT =
  struct

    let pp_binop = function
      | _ -> "not implemented"
      | ADD -> "+"
      | SUB -> "-"
      | MUL -> "*"
      | DIV -> "/"
      | MOD -> "%"

    let rec pp_exp = function
      | _ -> "not implemented"
      | CONST x -> Int32.to_string x
      | TEMP t  -> Temp.name t
      | BINOP (op, e1, e2,t1,t2) ->
          "(" ^ pp_exp e1 ^ " " ^ pp_binop op ^ " " ^ pp_exp e2 ^ ")"

    let pp_stm = function
      | _ -> "not implemented"
      | MOVE (e1, e2) -> pp_exp e1 ^ "  <--  " ^ pp_exp e2
      | RETURN e -> "return " ^ pp_exp e

    let rec pp_program = function
      | _ -> "not implemented"
      | [] -> ""
      | stm :: stms -> pp_stm stm ^ "\n" ^ pp_program stms

  end
