(* L1 Compiler
 * Register Allocation
 * Author: David Dong <haochend@andrew.cmu.edu>
 *
 * Allocate available registers based on the 3-address language
 *
 *)

open Core

let intersect weights neighbors =
	List.filter ~f:(fun n -> List.exists ~f:(fun (i, w) -> i = n) weights) neighbors

let rec find_len l =
	match l with
	| [] -> 0
	| h::t -> (find_len t)+1

let compare_tuple (x1, y1) (x2, y2) = if y1 > y2 then (x1, y1) else (x2, y2)

let rec find_order g ordering weights =
	match weights with
	| [] -> ordering
	| w::rest ->
		let ind, _ = List.fold_left ~f:(compare_tuple) ~init:(w) rest
		in let new_ordering = ordering@[ind]
			and _, neighbors = Array.get g ind
		in let inter = intersect weights neighbors
		in let new_weights =
			List.map
				~f:(fun (i1, w1) ->
					if List.exists ~f:(fun i -> i = i1) inter then (i1, w1+1) else (i1, w1))
			weights
		in find_order g new_ordering (List.filter ~f:(fun (i, w) -> i <> ind) new_weights)


let rec set colors i min_color =
	match colors with
	| [] -> []
	| h::t -> let (k, c) = h in match i with
				| 0 -> [(k, min_color)]@t
				| _ -> [h]@(set t (i-1) min_color)

let rec regalloc_helper g order colors =
	match order with
	| [] -> Array.to_list colors
	| h::t ->
		let _, neighbors = Array.get g h
		in let colored_neighbors = List.filter ~f:(fun i -> let (_,  c) = (Array.get colors i) in c <> -1) neighbors
		in let min_color = Array.fold_right ~f:(max) ~init:(-1)
			(Array.map ~f:(fun (_, c) -> c) (Array.filter ~f:(fun (i, _) ->
				List.exists ~f:(fun ind -> i = ind) colored_neighbors) colors))
		in let () = Array.set colors h (h, min_color+1)
		in regalloc_helper g t colors

let regalloc g =
	let len = Array.length g in
	let weights = List.init len ~f:(fun i -> (i, 0)) in
	let order = find_order g [] weights
	and colors = Array.init len ~f:(fun i -> i, -1) in
  List.map ~f:(fun (a, b) -> b) (regalloc_helper g order colors)
