(* L1 Compiler
 * Positional Markers
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations / bugfixes: Alex Vaynberg <alv@andrew.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type ext = (int * int) * (int * int) * string  (* position *)

val show : ext -> string (* converts the data into human readable form *)

type 'a marked                 (* value with positional information *)

(* INTRODUCTION FUNCTIONS for type 'a marked *)

(* put together a value and positional information *)
val mark : 'a * ext -> 'a marked

(* put together a value and an option of positional information *)
val mark' : 'a * ext option -> 'a marked

(* mark the value with no positional information *)
val naked : 'a -> 'a marked

(* ELIMINATION FUNCTIONS for type a' marked *)

(* data: remove the markings *)
val data : 'a marked -> 'a

(* ext: retrieve positional information from marked value*)
val ext : 'a marked -> ext option


(* USEFUL TOOLS *)

(* wrap:
 * returns SOME of positional information unit that contains each one
 * in the list; NONE if such wrap is not possible (spans several files,
 * etc.)
 *)
val wrap : ext option list -> ext option

(* map: make your function keep positional information *)
val map : ('a -> 'b) -> 'a marked -> 'b marked

(* map': similar to map, but f can now use positional information
 * and preserve it at the same time
 *)
val map' : ('a marked -> 'b) -> 'a marked -> 'b marked
