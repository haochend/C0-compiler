(* L1 Compiler
 * Eight byte Temporaries
 * Author: Jin Yan <jiny1@andrew.cmu.edu>
*)

open Core

module ET = struct
  type t = int [@@deriving compare, sexp]
end

include ET

let counter = ref 0

let reset () = counter := 0
let create () =
  let t = !counter in
  let () = counter := !counter + 1 in
  t


let access () = !counter

let name t = string_of_int t
let compare a b = compare a b

let format ff t = Format.fprintf ff "%s" (name t)

include Comparable.Make(T)
