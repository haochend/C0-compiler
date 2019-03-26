

open Core

module AS = Assem

module IS = Set.Make(Int32)

module S = Label.Map

let m = ref S.empty

let rec propagate write read needed prop =
  match write, read, needed with
  | [], _, _ -> []
  | _, [], _ -> []
  | _, _, [] -> []
  | wh::wt, (s,l,label)::rt, nh::nt ->
    begin
      match label with
      | Some label' -> m := S.set !m ~key:label' ~data:prop;
        prop::(propagate wt rt nt prop)
      | None ->
        begin
          let (prop', l') = (match l with
              | None -> (IS.union prop nh, [])
              | Some l' -> (nh, l')) in
          let l' = List.map ~f:(fun label -> S.find_exn !m label) l' in
          let prop' = List.fold_left ~f:(IS.union) ~init:(prop') l' in
          match wh with
          | None -> prop'::(propagate wt rt nt prop')
          | Some d ->
            begin
              match IS.mem prop d with
              | true ->
                let prop =  IS.union (IS.union s (IS.remove prop d)) nh in
                prop::(propagate wt rt nt prop)
              | _ -> prop'::(propagate wt rt nt prop')
            end
        end
    end

let rec compare_needed n1 n2 =
  match n1, n2 with
  | [], _ -> true
  | _, [] -> true
  | h1::t1, h2::t2 ->
    let diff = IS.diff h1 h2 in
    match IS.is_empty diff with
    | true -> compare_needed t1 t2
    | _ -> false

let rec propagate_helper write read needed =
  let needed' = propagate write read needed IS.empty in
  (* let needed' = propagate write read needed' IS.empty in
  let needed' = propagate write read needed' IS.empty in
  let needed' = propagate write read needed' IS.empty in
     needed' *)
  match compare_needed needed' needed with
  | true -> needed'
  | _ -> propagate_helper write read needed'

let rec apply assems write prop =
  match assems, write, prop with
  | [], _, _ -> []
  | ass, _, [] -> ass
  | ass, [], _ -> ass
  | assem::rest, wh::wt, ph::pt ->
    begin
      match wh with
      | None -> assem::(apply rest wt pt)
      | Some d -> (match IS.mem ph d with
          | false -> AS.NOP::(apply rest wt pt)
          | _ -> assem::(apply rest wt pt))
    end

let rec get_tmp_list n =
  match n with
  | 0 -> []
  | _ -> (AS.TEMP (Temp.set (n-1)))::(get_tmp_list (n-1))

let needness_opt assems =
  let read = (List.map ~f:(fun assem ->
					match assem with
       | AS.MOV (d, s) ->  ([s], None, None)
       | AS.BINOP (oper, d, s1, s2) -> ([s1; s2], None, None)
       | AS.UNOP (oper, d, s) -> ([s], None, None)
       | AS.GOTO l -> let AS.LABEL l = l in ([], Some [l], None)
       | AS.WRITELABEL l -> let AS.LABEL l = l in m := S.set !m ~key:l ~data:(IS.empty); ([], None, Some l)
       | AS.PUSH a -> ([], None, None)
       | AS.POP a -> ([], None, None)
       | AS.PASSARG (_, _, _) -> ([], None, None)
       | AS.MOVARGBEFORECALL (d, s) -> ([s], None, None)
       | AS.DOTDEREF (d, s, _) -> ([s], None, None)
       | AS.ALLOCARRY (d, _, s, _) -> ([s], None, None)
       | AS.ARRDEREF (d, s, s', _, _, _, _) -> ([s;s'], None, None)
       | AS.ARRDONOTHING (d, s, s', _, _, _, _) -> ([s;s'], None, None)
       | AS.ARRDEREFSHORTCUT (d, s, _,_) -> ([s], None, None)
       | AS.ARRDONOTHINGSHORTCUT (d, s, _) -> ([s], None, None)
       | AS.MOVETOADDR (d, s, _) -> ([s;d], None, None)
       | AS.MOVETOADDRFOURBYTE (d, s, _) -> ([s;d], None, None)
       | AS.MOVEEXPTOADDR (b,_,s,_,_) -> ([s;b],None, None)
       | AS.ARRADDRCOMP (d, s, s', _, _, _, _) -> ([s;s'], None, None)
       | AS.ARRADDRCOMPSHORT (d, s, _) -> ([s], None, None)
       | AS.GETCONTENT (s, d) -> ([s], None, None)
       | AS.GETCONTENTFOURBYTE (s, d) -> ([s], None, None)
       | AS.CP (s, l1, l2) -> let AS.LABEL l1 = l1 and AS.LABEL l2 = l2 in ([s], Some [l1; l2], None)
       | AS.LEA (t, _, _,_) -> ([t], None, None)
       | AS.QUESTION (d,s1,s2,s3) -> ([s1;s2;s3],None,None)
       | _ -> ([], None, None)
							) assems)
	and write = List.map ~f:(fun assem ->
					match assem with
					| AS.MOV (d, s) -> Some d
          | AS.BINOP (oper, d, s1, s2) -> Some d
          | AS.UNOP (oper, d, s) -> Some d
          | AS.PASSARG (t, _, _) -> Some t
          | AS.MOVARGBEFORECALL (d, s) -> Some d
          | AS.DOTDEREF (d, s, _) -> Some d
          | AS.ALLOCARRY (d, _, s, _) -> Some d
          | AS.ARRDEREF (d, s, _, _, _, _, _) -> Some d
          | AS.ARRDONOTHING (d, s, _, _, _, _, _) -> Some d
          | AS.ARRDEREFSHORTCUT (d, s, _ ,_) -> Some d
          | AS.ARRDONOTHINGSHORTCUT (d, s, _) -> Some d
          | AS.MOVETOADDR (d, s, _) -> None
          | AS.MOVETOADDRFOURBYTE (d, s, _) -> None
          | AS.ARRADDRCOMP (d, s, _, _, _, _, _) -> Some d
          | AS.ARRADDRCOMPSHORT (d, s, _) -> Some d
          | AS.GETCONTENT (s, d) -> Some d
          | AS.GETCONTENTFOURBYTE (s, d) -> Some d
          | AS.NULL d -> Some d
          | AS.MALLOC (d, _) -> Some d
          | AS.QUESTION (d,_,_,_) -> Some d
          | _ -> None
    ) assems in
 let needed = List.map ~f:(fun assem ->
     match assem with
     | AS.MOVETOADDR (d, s, _) -> [d;s]
     | AS.MOVETOADDRFOURBYTE (d, s, _) -> [d;s]
     | AS.CP (s, _, _) -> [s]
     | AS.RETURN -> [AS.REG AS.EAX]
     | AS.MOVARGBEFORECALL (d, s) -> [s]
     | AS.MOVEEXPTOADDR (b, _, s, _, _) -> [s;b]
     | AS.JMP (_, n) -> printf "get here\n"; (get_tmp_list n)
     | _ -> []) assems in
 let write = List.map ~f:(fun x -> match x with
     | Some (AS.TEMP n) -> Int32.of_int (int_of_string (Temp.name n))
     | Some (AS.REG AS.EAX) -> Some Int32.minus_one
     | _ -> None) write in
let write_rev = List.rev(write) in
let read_rev = List.map ~f:(fun (l1, l2, l3) ->
    IS.of_list (List.map ~f:(fun e -> let AS.TEMP n = e in
                              let Some n = Int32.of_int (int_of_string (Temp.name n)) in
                            n)
       (List.filter ~f:(fun x ->
                      match x with AS.TEMP t -> true | _ -> false) l1)), l2, l3) (List.rev read) in
 let needed_rev = List.map ~f:(fun l ->
     IS.of_list (List.map ~f:(fun e -> match e with
         | AS.TEMP n -> let Some n = Int32.of_int (int_of_string (Temp.name n)) in n
         | AS.REG AS.EAX -> Int32.minus_one)
                  (List.filter ~f:(fun x ->
                       match x with AS.TEMP t -> true | AS.REG AS.EAX -> true | _ -> false) l))) (List.rev(needed)) in
 (* let need_rev = propagate write_rev read_rev needed_rev IS.empty in
 let need_rev = propagate write_rev read_rev needed_rev IS.empty in
 let need_rev = propagate write_rev read_rev needed_rev IS.empty in *)
 let prop = propagate_helper write_rev read_rev needed_rev in
apply assems write (List.tl_exn (List.rev prop))
