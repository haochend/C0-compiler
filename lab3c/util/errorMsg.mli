(* L1 Compiler
 * Error messages
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

(* clears out all errors from the system *)
val reset : unit -> unit

(* global flag that indicates whether there were errors *)
val anyErrors : bool ref

(* sets the error flag and prints out an error message, does NOT raise ERROR *)
val error : Mark.ext option -> string -> unit
(* same, but does not increment error count *)
val warn : Mark.ext option -> string -> unit

(* generic code stopping exception *)
exception Error
