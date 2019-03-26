(* L1 Compiler
 * Labels
 * Author: Jin Yan <jiny1@andrew.cmu.edu>
*)

open Core

module L = struct
  type t = int [@@deriving compare, sexp]
end

include L

let counter = ref 0

let reset () = counter := 0
let create () =
  let t = !counter in
  let () = counter := !counter + 1 in
  t

let set t = t


let name t = string_of_int t
let compare a b = compare a b

let format ff t = Format.fprintf ff "%s" (name t)

include Comparable.Make(L)
