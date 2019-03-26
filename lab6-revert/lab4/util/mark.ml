(* L1 Compiler
 * Positional Markers
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations / bugfixes: Alex Vaynberg <alv@andrew.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 *)

open Core

type ext = (int * int) * (int * int) * string

let pos = function
  | (row, 0)   -> string_of_int row
  | (row, col) -> string_of_int row ^ "." ^ string_of_int col

let show (l, r, file) = file ^ ":" ^ pos l ^ "-" ^ pos r

type 'a marked = 'a * ext option

let mark (d, e) = (d, Some e)
let mark' x = x
let naked d = (d, None)

let data = fst
let ext = snd

let extmin ((l1, c1), (l2, c2)) =
  if l1 < l2 then (l1, c1) else
  if l1 > l2 then (l2, c2) else
  (l1, min c1 c2)

let extmax ((l1, c1), (l2, c2)) =
  if l1 > l2 then (l1, c1) else
  if l1 < l2 then (l2, c2) else
  (l1, max c1 c2)

let rec wrap = function
  | []    -> None
  | e :: [] -> e
  | e :: el -> match wrap el with
    | None -> None
    | Some ((el1, el2, elf) as e') ->
        match e with
        | Some (e1, e2, ef) ->
            if compare ef elf = 0 then
              Some (extmin (e1, el1), extmax (e2, el2), ef)
            else None
        | None -> Some e'

let map f (d, e) = (f d, e)
let map' f ((d, e) as m) = (f m, e)
