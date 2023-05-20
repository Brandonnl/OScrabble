type t = string

let from_string str = str

let check_valid set word =
  try List.exists (fun x -> x = word) set with Not_found -> false

let explode s = List.init (String.length s) (String.get s)
let random_char () = Char.uppercase_ascii (Char.chr (97 + Random.int 26))

let word_value word =
  List.fold_left
    (fun acc c -> acc + Tile.get_points c)
    0
    (List.map Tile.create_tile (explode word))
