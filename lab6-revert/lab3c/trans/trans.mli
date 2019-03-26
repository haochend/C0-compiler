(* L1 Compiler
 * AST -> IR Translator
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

(* translate abstract syntax tree to IR tree *)

type func_init_status =
  | Deleted
  | FDecl of Ast.typedefine * Ast.param list
  | FDefn of Ast.typedefine * Ast.param list

val translate_gdecl : Ast.program -> func_init_status Symbol.Map.t -> Tree.stm list * func_init_status Symbol.Map.t
