open Core

module T = struct
  type t = string * int [@@deriving compare, sexp]
end

include T

let bogus = ("?", -1)
let is_bogus (_, x) = x = -1

let compare (_, i) (_, i') =
  if i < 0 || i' < 0 then 1
  else i - i'

let ht = String.Table.create ()
let nexts = ref 0
let reset () = nexts := 0; String.Table.clear ht
let types name =
  match Hashtbl.find ht name with
  | Some sym -> (name, sym)
  | None ->
    let i = !nexts in
    let () = incr nexts in
    let () = Hashtbl.add_exn ht name i in
    (name, i)

let find name =
  match Hashtbl.find ht name with
  | Some _ -> true
  | _ -> false

let name (n, _) = n

let format ff s =
  Format.fprintf ff "%s" (name s)

include Comparable.Make(T)
