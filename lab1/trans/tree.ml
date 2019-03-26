(* L1 Compiler
 * IR Trees
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type binop = ADD | SUB | MUL | DIV | MOD

type exp =
  | CONST of Int32.t
  | TEMP of Temp.t
  | BINOP of binop * exp * exp
and stm =
  | MOVE of exp * exp
  | RETURN of exp

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
      | ADD -> "+"
      | SUB -> "-"
      | MUL -> "*"
      | DIV -> "/"
      | MOD -> "%"

    let rec pp_exp = function
      | CONST x -> Int32.to_string x
      | TEMP t  -> Temp.name t
      | BINOP (op, e1, e2) ->
          "(" ^ pp_exp e1 ^ " " ^ pp_binop op ^ " " ^ pp_exp e2 ^ ")"

    let pp_stm = function
      | MOVE (e1, e2) -> pp_exp e1 ^ "  <--  " ^ pp_exp e2
      | RETURN e -> "return " ^ pp_exp e

    let rec pp_program = function
      | [] -> ""
      | stm :: stms -> pp_stm stm ^ "\n" ^ pp_program stms

  end
