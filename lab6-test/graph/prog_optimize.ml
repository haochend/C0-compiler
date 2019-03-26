



let rec prog_optimize cnts assem f o =
  match cnts, assem with
  | [], _ -> []
  | _, [] -> []
  | cl::cr, al::ar ->
    begin
      let rest_assem = prog_optimize cr ar f o in
      if cl = 0 then al::rest_assem
      else begin
        let g = Liveness.create_graph al cl in
        let (coloring, maxColor) = Regalloc.regalloc g in
        (*        let after_coalescing = if f then coloring else Coalescing.coalesce g al coloring maxColor in 
*)    let new_assem = Apply.apply_coloring coloring al maxColor in
        let al = if ((not o) && (not f)) then Needness.needness_opt al else al in

        al::rest_assem
      end
    end
