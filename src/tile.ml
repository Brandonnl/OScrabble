type t =
  | Letter of char
  | Blank
  | UsedBlank of char

let create_tile c =
  match c with
  | ' ' -> Blank
  | c -> Letter (Char.uppercase_ascii c)

let get_point_char chr =
  let upper_char = Char.uppercase_ascii chr in
  if List.mem upper_char [ 'A'; 'E'; 'I'; 'L'; 'N'; 'O'; 'R'; 'S'; 'T'; 'U' ]
  then 1
  else if List.mem upper_char [ 'D'; 'G' ] then 2
  else if List.mem upper_char [ 'B'; 'C'; 'M'; 'P' ] then 3
  else if List.mem upper_char [ 'F'; 'H'; 'V'; 'W'; 'Y' ] then 4
  else if List.mem upper_char [ 'K' ] then 5
  else if List.mem upper_char [ 'J'; 'X' ] then 8
  else 10

let get_points tile =
  match tile with
  | Blank -> 0
  | UsedBlank _ -> 0
  | Letter x -> get_point_char x

let get_points_list lst = List.fold_left (fun acc x -> acc + get_points x) 0 lst

let get_letter tile =
  match tile with
  | Letter c -> c
  | UsedBlank c -> c
  | Blank -> ' '