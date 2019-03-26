(* L1 Compiler
 * TypeChecker
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Simple typechecker that is based on a unit Symbol.table
 * This is all that is needed since there is only an integer type present
 * Also, since only straightline code is accepted, we hack our way
 * around initialization checks here.
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now distinguishes between declarations and initialization
 *)

(* prints error message and raises ErrorMsg.error if error found *)

type func_init_status =
  | Deleted
  | FDecl of Ast.typedefine * Ast.param list
  | FDefn of Ast.typedefine * Ast.param list
  | FDeclUsed of Ast.typedefine * Ast.param list

val checkheader: Ast.program -> func_init_status Symbol.Map.t * Ast.typedefine Symbol.Map.t


val typecheck : Ast.program -> (func_init_status Symbol.Map.t * Ast.typedefine Symbol.Map.t) option -> unit
