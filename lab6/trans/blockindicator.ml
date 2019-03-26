(* L1 Compiler
 * Temporaries
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
*)

open Core

module BI = struct
  type t = int [@@deriving compare, sexp]
end

include BI

let counter = ref 0

let reset () = counter := 0
let increment () =
  let t = !counter in
  let () = counter := !counter + 1 in
  t

let get() =
  let t = !counter in
  t


let set newval =
  let () = counter := newval in
  let t = !counter in
  t

let access () = !counter

let name t = string_of_int t
let compare a b = compare a b

let format ff t = Format.fprintf ff "%s" (name t)

include Comparable.Make(BI)
