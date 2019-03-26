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

module IS = Set.Make(Int32)

module S = Label.Map

let m = ref S.empty



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
        let Some d = Int32.to_int d in
	let _, dSet = Array.get g d in
	Array.set g d (d, IS.add dSet s)

let rec add_to_graph g d v =
  add_edge g d v;
  add_edge g v d


let rec create_graph_helper write read g =
	match write, read with
	| [], _ -> g
	| _, [] -> g
	| wh::wt, rh::rt ->
   (match wh with
   | None -> create_graph_helper wt rt g
   | Some d -> let () = IS.iter rh ~f:(fun v -> add_to_graph g d v) in
     create_graph_helper wt rt g)

let rec second_prop write_rev read_rev prop =
  match write_rev, read_rev with
  | [], _ -> []
  | _, [] -> []
  | wh::wt, rh::rt ->
    let (s, l, label) = rh in
    let l' = List.map ~f:(fun label -> S.find_exn !m label) l in
    let s' = List.fold_left ~f:(IS.union) ~init:(IS.union s prop) l' in
    let s' = (match wh with
        | None -> s'
        | Some d -> IS.remove s' d) in
    [s']@(second_prop wt rt s')

let rec first_prop write_rev read_rev prop =
  match write_rev, read_rev with
  | [], _ -> []
  | _, [] -> []
  | wh::wt, (s, l, label)::rt ->
    let s = IS.union s prop in
    let s = (match wh with
      | None -> s
      | Some d -> IS.remove s d) in
    match label with
    | None -> (s, l, label)::(first_prop wt rt s)
    | Some label' -> m := S.set !m ~key:label' ~data:s;
      (s, l, label)::(first_prop wt rt s)

let create_graph assems largest_t =
	let read = List.tl_exn (List.map ~f:(fun assem ->
					match assem with
       | AS.MOV (d, s) ->  ([s], [], None)
       | AS.BINOP (oper, d, s1, s2) -> ([s1; s2], [], None)
       | AS.UNOP (oper, d, s) -> ([s], [], None)
       | AS.GOTO l -> let AS.LABEL l = l in ([], [l], None)
       | AS.WRITELABEL l -> let AS.LABEL l = l in m := S.set !m ~key:l ~data:(IS.empty); ([], [], Some l)
       | AS.PUSH a -> ([], [], None)
       | AS.POP a -> ([], [], None)
       | AS.PASSARG (_, _, _) -> ([], [], None)
       | AS.MOVARGBEFORECALL (d, s) -> ([s], [], None)
       | AS.DOTDEREF (d, s, _) -> ([s], [], None)
       | AS.ALLOCARRY (d, _, s, _) -> ([s], [], None)
       | AS.ARRDEREF (d, s, s', _, _, _, _) -> ([s;s'], [], None)
       | AS.ARRDONOTHING (d, s, s', _, _, _, _) -> ([s;s'], [], None)
       | AS.MOVETOADDR (d, s, _) -> ([s;d], [], None)
       | AS.MOVETOADDRFOURBYTE (d, s, _) -> ([s;d], [], None)
       | AS.ARRADDRCOMP (d, s, s', _, _, _, _) -> ([s;s'], [], None)
       | AS.GETCONTENT (s, d) -> ([s], [], None)
       | AS.GETCONTENTFOURBYTE (s, d) -> ([s], [], None)
       | AS.CP (s, l1, l2) -> let AS.LABEL l1 = l1 and AS.LABEL l2 = l2 in ([s], [l1; l2], None)
       | AS.LEA (t, _, _) -> ([t], [], None)
       | _ -> ([], [], None)
							) assems)
	and write = List.map ~f:(fun assem ->
					match assem with
					| AS.MOV (d, s) -> Some d
          | AS.BINOP (oper, d, s1, s2) -> Some d
          | AS.UNOP (oper, d, s) -> Some d
          | AS.PASSARG (t, _, _) -> Some t
          | AS.MOVARGBEFORECALL (_, _) -> None
          | AS.DOTDEREF (d, s, _) -> Some d
          | AS.ALLOCARRY (d, _, s, _) -> Some d
          | AS.ARRDEREF (d, s, _, _, _, _, _) -> Some d
          | AS.ARRDONOTHING (d, s, _, _, _, _, _) -> Some d
          | AS.MOVETOADDR (d, s, _) -> None
          | AS.MOVETOADDRFOURBYTE (d, s, _) -> None
          | AS.ARRADDRCOMP (d, s, _, _, _, _, _) -> Some d
          | AS.GETCONTENT (s, d) -> Some d
          | AS.GETCONTENTFOURBYTE (s, d) -> Some d
          | AS.NULL d -> Some d
	        | AS.MALLOC (d, _) -> Some d
          | _ -> None
							) assems in
 let write_rev = List.map ~f:(fun x -> match x with
     | Some (AS.TEMP n) -> Int32.of_int (int_of_string (Temp.name n))
     | _ -> None) (List.tl_exn (List.rev(write)))
 and read_rev = List.rev(read) in
	(* let () = List.iter ~f:(fun i-> printf "%s " (Temp.name i)) write_rev in *)
 let read_rev = List.map ~f:(fun (l1, l2, l3) ->
     IS.of_list (List.map ~f:(fun e -> let AS.TEMP n = e in
                               let Some n = Int32.of_int (int_of_string (Temp.name n)) in
                             n)
        (List.filter ~f:(fun x ->
             match x with AS.TEMP t -> true | _ -> false) l1)), l2, l3) read_rev in
	(* and largest_t = List.fold_left ~f:(max) ~init:(-1)
		(List.map ~f:(fun x -> int_of_string (Temp.name x)) write_rev) in *)
 let g = Array.init (largest_t+1) ~f:(fun i -> i, IS.empty) in
 let read_rev = first_prop write_rev read_rev IS.empty in
 let read_rev = second_prop write_rev read_rev IS.empty in
 let g = create_graph_helper write_rev read_rev g in
 Array.map ~f:(fun (i, s) -> (i, List.map ~f:(fun e -> let Some n = Int32.to_int e in n) (IS.elements s))) g
