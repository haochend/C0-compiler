(* L1 Compiler
 * Parsing
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 * Gluing together the pieces produced by ocamllex and ocamlyacc
 *)

(* parse filename = ast
 * will raise ErrorMsg.Error in case of lexing or parsing error
 *)

open Core

let parse filename =
  try
    In_channel.with_file filename ~f:(
      fun chan ->
        let lexbuf = Lexing.from_channel chan in
        let _ = ErrorMsg.reset ()
        and _ = ParseState.setfile filename in
        let ast = C0Parser.program C0Lexer.initial lexbuf in
        let _ = if !ErrorMsg.anyErrors then raise ErrorMsg.Error else () in
        ast)
  with
  | Parsing.Parse_error ->
    ErrorMsg.error None "Parse error"; raise ErrorMsg.Error
  | Sys_error s -> ErrorMsg.error None s; raise ErrorMsg.Error
