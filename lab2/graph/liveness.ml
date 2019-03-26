(* L1 Compiler
 * Assembly language
 * Author: Jin Yan <jiny1@andrew.cmu.edu>
 *		   David Dong <haochend@andrew.cmu.edu>
 * Checks the liveness in the assem and output a node->neighbourhood list
 *
 *
 *
 *)

open Core 

module AS = Assem


let rec find_len l = 
	match l with
	| [] -> 0
	| h::t -> (find_len t)+1

let rec set colors i min_color = 
	match colors with
	| [] -> []
	| h::t -> let (k, c) = h in match i with
				| 0 -> [(k, min_color)]@t
				| _ -> [h]@(set t (i-1) min_color)

let add_edge g d s = 
	let d_int = int_of_string (Temp.name d)
	and s_int = int_of_string (Temp.name s) in
	let _, dList = Array.get g d_int in
	if List.exists ~f:(fun i -> i = s_int) dList then ()
	else Array.set g d_int (d_int, dList@[s_int])


let rec add_to_graph g d slist = 
	match slist with
	| [] -> g
	| h::t -> 
					(let () = add_edge g d h in
					let () = add_edge g h d in
					add_to_graph g d t)

let rec create_graph_helper write read g prop = 
	match write, read with
	| [], _ -> g
	| _, [] -> g
	| wh::wt, rh::rt -> 
		let prop = List.filter ~f:(fun s -> s <> wh) (rh@prop) in
		let g = add_to_graph g wh prop in
		create_graph_helper wt rt g prop

let create_graph assems = 
	let read = List.tl_exn (List.map ~f:(fun assem -> 
										match assem with 
										| AS.MOV (d, s) ->  [s]
										| AS.BINOP (oper, d, s1, s2) -> [s1; s2]
							) assems)
	and write = List.map ~f:(fun assem -> 
										match assem with 
										| AS.MOV (d, s) -> d
										| AS.BINOP (oper, d, s1, s2) -> d
							) assems in
	let write_rev = List.map ~f:(fun x -> let AS.TEMP t = x in t) 
		(List.filter ~f:(fun x -> match x with AS.TEMP _ -> true | _ -> false) 
			(List.tl_exn (List.rev(write))))
	and read_rev = List.rev(read) in
	(* let () = List.iter ~f:(fun i-> printf "%s " (Temp.name i)) write_rev in *)
	let read_rev = List.map ~f:(fun l -> 
		List.map ~f:(fun e -> let AS.TEMP n = e in n) (List.filter ~f:(fun x -> 
			match x with AS.TEMP t -> true | _ -> false) l)) read_rev
	and largest_t = List.fold_left ~f:(max) ~init:(-1) 
		(List.map ~f:(fun x -> int_of_string (Temp.name x)) write_rev) in
	let g = Array.init (largest_t+1) ~f:(fun i -> i, []) in
	create_graph_helper write_rev read_rev g []
