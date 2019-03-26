(* L1 Compiler
 * Labels
 * Author: Jin Yan <jiny1@andrew.cmu.edu>
*)

open Core

module LSO = struct
  type t = int [@@deriving compare, sexp]
end

include LSO

let counter = ref 0

let reset () = counter := 0
let createfour () =
  let t = !counter in
  let () = counter := !counter + 0 in
  t

let createeight () =
  let t = !counter in
  let () = counter := !counter + 1 in
  t

let set t =
  let () = counter := !counter + t
  in t

let access () = !counter

let name t = string_of_int t
let compare a b = compare a b

let format ff t = Format.fprintf ff "%s" (name t)

include Comparable.Make(LSO)
