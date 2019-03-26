
open Core

module AS = Assem

module IS = Set.Make(Int32)

let rec helper colors maxColor i =
  match colors with
  | [] -> if i > maxColor then None else (Some i)
  | l::r ->
    let (Some n) = Int32.to_int l in
    if n = i then helper r maxColor (i+1) else (Some i)

let find_color u_color maxColor =
  helper (IS.elements u_color) maxColor 0

let color_set s coloring =
  IS.fold s ~init:(IS.empty) ~f:(fun s1 e ->
      let (Some n) = Int32.to_int e in
      let c = Array.get coloring n in
      let (Some c) = Int32.of_int c in
    IS.add s1 c)

let rec coalesce g assems coloring maxColor =
  match assems with
  | [] -> coloring
  | l::r ->
    begin
      match l with
      | AS.MOV (d, s) ->
        begin
          match d, s with
          | AS.TEMP d, AS.TEMP s ->
            begin
              let d = int_of_string (Temp.name d)
              and s = int_of_string (Temp.name s) in
              let (Some setd) = Int32.of_int d
              and (Some sets) = Int32.of_int s in
              let (_, n1) = Array.get g d
              and (_, n2) = Array.get g s in
              match IS.mem n1 sets with
              | true -> coalesce g r coloring maxColor
              | _ ->
                (let u = IS.remove (IS.remove (IS.union n1 n2) setd) sets in
                let u_color = color_set u coloring in
                match find_color u_color maxColor with
                | Some c ->
                  begin
                    Array.set g d (d, u);
                    Array.set g s (s, u);
                    IS.iter u ~f:(fun e -> let (Some n) = Int32.to_int e in
                                   let (_, set) = Array.get g n in
                                   Array.set g n (n, IS.add (IS.add set setd) sets));
                    Array.set coloring d c;
                    Array.set coloring s c;
                    coalesce g r coloring maxColor
                  end
                | None -> coalesce g r coloring maxColor)
            end
          | _, _ -> coalesce g r coloring maxColor
        end
      | _ -> coalesce g r coloring maxColor
    end
