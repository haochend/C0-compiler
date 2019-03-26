



open Core

module AS = Assem

let find_color t coloring =
	let Some newt = List.nth coloring (int_of_string (Temp.name t)) in
	(* let () = printf "%d, %d\n" i newt in *)
 Temp.set newt

let apply_color o coloring =
  match o with
  | AS.TEMP t -> AS.TEMP (find_color t coloring)
  | _ -> o

let apply_coloring_helper coloring a maxColor =
  match a with
  | AS.MOV (d, s) -> AS.MOV (apply_color d coloring, apply_color s coloring)
  | AS.BINOP (o, d, s1, s2) -> AS.BINOP (o, apply_color d coloring, apply_color s1 coloring, apply_color s2 coloring)
  | AS.UNOP (o, d, s) -> AS.UNOP (o, apply_color d coloring, apply_color s coloring)
  | AS.MOVARGBEFORECALL (d, s) -> AS.MOVARGBEFORECALL (apply_color d coloring, apply_color s coloring)
  | AS.DOTDEREF (d, s, l) -> AS.DOTDEREF (apply_color d coloring, apply_color s coloring, l)
  | AS.ALLOCARRY (d, n, s, l) -> AS.ALLOCARRY (apply_color d coloring, n, apply_color s coloring, l)
  | AS.ARRDEREF (d, s, n1, n2, n3, n4, n5) -> AS.ARRDEREF (apply_color d coloring, apply_color s coloring, apply_color n1 coloring, n2, n3, n4, n5)
  | AS.ARRDONOTHING (d, s, n1, n2, n3, n4, n5) -> AS.ARRDONOTHING (apply_color d coloring, apply_color s coloring, apply_color n1 coloring, n2, n3, n4, n5)
  | AS.MOVETOADDR (d, s, l) -> AS.MOVETOADDR (apply_color d coloring, apply_color s coloring, l)
  | AS.MOVETOADDRFOURBYTE (d, s, l) -> AS.MOVETOADDRFOURBYTE (apply_color d coloring, apply_color s coloring, l)
  | AS.ARRADDRCOMP (d, s, n1, n2, n3, n4, n5) -> AS.ARRADDRCOMP (apply_color d coloring, apply_color s coloring, apply_color n1 coloring, n2, n3, n4, n5)
  | AS.GETCONTENT (s, d) -> AS.GETCONTENT (apply_color s coloring, apply_color d coloring)
  | AS.GETCONTENTFOURBYTE (s, d) -> AS.GETCONTENTFOURBYTE (apply_color s coloring, apply_color d coloring)
  | AS.CP (s, l1, l2) -> AS.CP (apply_color s coloring, l1, l2)
  | AS.PASSARG (t, n1, n2) -> AS.PASSARG (apply_color t coloring, n1, n2)
  | AS.NULL d -> AS.NULL (apply_color d coloring)
  | AS.MALLOC (d, size) -> AS.MALLOC (apply_color d coloring, size)
  | AS.LEA (t, o, l) -> AS.LEA (apply_color t coloring, o, l)
  | AS.STACKOFFSET (o, _) -> AS.STACKOFFSET (o, maxColor)
  | AS.STACKONSET (o, _) -> AS.STACKONSET (o, maxColor)
  | _ -> a

let apply_coloring coloring assem =
  let maxColor = List.fold_left ~f:(max) ~init:(-1) coloring in
	List.map ~f:(fun ass ->
						apply_coloring_helper coloring ass maxColor
	) assem
