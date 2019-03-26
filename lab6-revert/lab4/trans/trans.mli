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
(*
type typedefine =
    | INT
    | BOOL
    | VOID
    | IDENT of Ast.ident
    | POINTER of Ast.typedefine
    | ARRAY of Ast.typedefine
    | STRUCT of Ast.ident *)
val translate_gdecl : Ast.program option ->
  func_init_status Symbol.Map.t ->
  int Symbol.Map.t Symbol.Map.t ->
  Ast.typedefine Symbol.Map.t Symbol.Map.t ->
  int Symbol.Map.t ->
  Ast.typedefine Symbol.Map.t -> int list ->
  Tree.stm list list * func_init_status Symbol.Map.t *
  int Symbol.Map.t Symbol.Map.t * Ast.typedefine Symbol.Map.t Symbol.Map.t *
  int Symbol.Map.t * Ast.typedefine Symbol.Map.t * int list

val translate_header : Ast.program option ->
  func_init_status Symbol.Map.t ->
  int Symbol.Map.t Symbol.Map.t ->
  Ast.typedefine Symbol.Map.t Symbol.Map.t ->
  int Symbol.Map.t ->
  Ast.typedefine Symbol.Map.t ->
  Tree.stm list * func_init_status Symbol.Map.t *
  int Symbol.Map.t Symbol.Map.t * Ast.typedefine Symbol.Map.t Symbol.Map.t *
  int Symbol.Map.t * Ast.typedefine Symbol.Map.t
