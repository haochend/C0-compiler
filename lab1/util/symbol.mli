(* C0 Compiler
 * The symbol tables
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 *)

open Core

type t [@@deriving compare, sexp]

include Comparable.S with type t := t

val compare : t -> t -> int (* compare symbols by their
                                         creation time *)



val bogus : t              (* a dummy symbol, less than others *)
val is_bogus : t -> bool

val reset : unit -> unit (* resets the hash table in which the
                            symbols are stored *)

val symbol : string -> t (* generates a new symbol with given name *)
val name : t -> string     (* returns a name associated with symbol *)
val format : Format.formatter -> t -> unit
