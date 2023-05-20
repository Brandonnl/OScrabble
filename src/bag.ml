open Tile

type t = char list

exception Empty of string

let create_bag_no_blanks () : t =
  [
    'A';
    'A';
    'A';
    'A';
    'A';
    'A';
    'A';
    'A';
    'A';
    'B';
    'B';
    'C';
    'C';
    'D';
    'D';
    'D';
    'D';
    'E';
    'E';
    'E';
    'E';
    'E';
    'E';
    'E';
    'E';
    'E';
    'E';
    'E';
    'E';
    'F';
    'F';
    'G';
    'G';
    'G';
    'H';
    'H';
    'I';
    'I';
    'I';
    'I';
    'I';
    'I';
    'I';
    'I';
    'I';
    'J';
    'K';
    'L';
    'L';
    'L';
    'L';
    'M';
    'M';
    'N';
    'N';
    'N';
    'N';
    'N';
    'N';
    'O';
    'O';
    'O';
    'O';
    'O';
    'O';
    'O';
    'O';
    'P';
    'P';
    'Q';
    'R';
    'R';
    'R';
    'R';
    'R';
    'R';
    'S';
    'S';
    'S';
    'S';
    'T';
    'T';
    'T';
    'T';
    'T';
    'T';
    'U';
    'U';
    'U';
    'U';
    'V';
    'V';
    'W';
    'W';
    'X';
    'Y';
    'Y';
    'Z';
  ]

let create_bag () = ' ' :: ' ' :: create_bag_no_blanks ()
let empty () : t = []
let is_empty (bag : t) = List.length bag = 0
let size_bag bag = List.length bag

let rec list_remove acc c n lst =
  match lst with
  | [] -> acc @ lst
  | h :: t ->
      if n = 0 then acc @ lst
      else if h = c then list_remove acc c (n - 1) t
      else list_remove (h :: acc) c n t

let remove (c : char) (bag : t) = list_remove [] c 1 bag

let print_bag (bag : t) =
  List.fold_left (fun acc x -> acc ^ String.make 1 x) "" bag

let draw_tile bag =
  if is_empty bag then raise (Empty "empty bag")
  else
    let size = List.length bag in
    Random.self_init ();
    let rand = Random.int size in
    let chr = List.nth bag rand in
    let tile = Tile.create_tile chr in
    let new_bag = remove (Tile.get_letter tile) bag in
    (tile, new_bag)

let rec to_char (bag : t) acc =
  match bag with
  | [] -> acc
  | h :: t -> to_char t (h :: acc)
