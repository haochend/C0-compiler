(* L1 Compiler
 * Parse State System
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * This tracks filename and newline characters
 * so character positions in lexer tokens
 * can be converted to line.column format for error messages
 *)

(* setfile(filename) sets current filename and resets newline positions *)
val setfile : string -> unit

(* newline(pos) adds pos to current newline positions *)
val newline : int -> unit

(* returns the current position information based on two integer offsets *)
val ext : int * int -> Mark.ext option
