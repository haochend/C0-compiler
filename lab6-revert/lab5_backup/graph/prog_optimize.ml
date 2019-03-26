



let rec prog_optimize cnts assem =
  match cnts, assem with
  | [], _ -> []
  | _, [] -> []
  | cl::cr, al::ar ->
    begin
      let rest_assem = prog_optimize cr ar in
      if cl = 0 then al::rest_assem
      else begin
        let g = Liveness.create_graph al cl in
        let coloring = Regalloc.regalloc g in
        let new_assem = Apply.apply_coloring coloring al in
        new_assem::rest_assem
      end
    end
