



open Core

module AS = Assem

let find_color t coloring = 
	let Some (i, newt) = List.nth coloring (int_of_string (Temp.name t)) in
	(* let () = printf "%d, %d\n" i newt in *)
	Temp.set newt

 
let apply_coloring coloring assem = 
	List.map ~f:(fun ass ->
						match ass with
						| AS.MOV (d, s) ->   (match (d,s) with 
											| AS.TEMP t1, AS.TEMP t2 -> 
												AS.MOV (AS.TEMP (find_color t1 coloring), AS.TEMP (find_color t2 coloring))
											| t1, AS.TEMP t2 -> 
												AS.MOV (t1, AS.TEMP (find_color t2 coloring))
											| AS.TEMP t1, t2 -> 
												AS.MOV (AS.TEMP (find_color t1 coloring), t2)
											| _, _ -> AS.MOV (d, s)
											)
						| AS.BINOP (oper, d, s1, s2) -> (match (d, s1, s2) with
											| AS.TEMP d1, AS.TEMP t1, AS.TEMP t2 -> 
												AS.BINOP (oper, AS.TEMP (find_color d1 coloring),AS.TEMP (find_color t1 coloring), AS.TEMP (find_color t2 coloring))
											| AS.TEMP d1, t1, AS.TEMP t2 -> 
												AS.BINOP (oper,AS.TEMP (find_color d1 coloring), t1, AS.TEMP (find_color t2 coloring))
											| AS.TEMP d1, AS.TEMP t1, t2 -> 
												AS.BINOP (oper,AS.TEMP (find_color d1 coloring), AS.TEMP (find_color t1 coloring), t2)
											| d1, AS.TEMP t1, AS.TEMP t2 -> 
												AS.BINOP (oper, d1, AS.TEMP (find_color t1 coloring), AS.TEMP (find_color t2 coloring))
											| d1, t1, AS.TEMP t2 -> 
												AS.BINOP (oper, d1, t1, AS.TEMP (find_color t2 coloring))
											| d1, AS.TEMP t1, t2 -> 
												AS.BINOP (oper, d1, AS.TEMP (find_color t1 coloring), t2)
											| _, _, _ -> AS.BINOP (oper, d, s1,s2)
											)
	) assem
