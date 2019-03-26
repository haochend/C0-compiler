(* L1 Compiler
 * Labels
 * Author: Jin Yan <jiny1@andrew.cmu.edu>
*)

open Core

type t [@@deriving compare, sexp]

include Comparable.S with type t := t

val reset   : unit -> unit      (* resets temp numbering *)
val create  : unit -> t         (* returns a unique new temp *)
val name    : t -> string       (* returns the name of a temp *)
val compare : t -> t -> int     (* comparison function *)
val set     : int -> t

val format  : Format.formatter -> t -> unit
