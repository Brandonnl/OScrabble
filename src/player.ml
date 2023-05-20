open Bag

type t = {
  name : string;
  score : int;
  letters : Tile.t list;
}

exception InvalidChar of char

let valid_player num = if num < 2 || num > 4 then false else true

let rec fill_hand (lst : Tile.t list) (bag : Bag.t) =
  if (not (Bag.is_empty bag)) && List.length lst < 7 then
    match lst with
    | _ ->
        let draw = Bag.draw_tile bag in
        fill_hand (fst draw :: lst) (snd draw)
  else (lst, bag)

let new_player n = { name = n; score = 0; letters = [] }
let get_letters p = p.letters
let set_letters p lst = { p with letters = lst }
let get_name p = p.name
let get_score p = p.score
let add_score s p = { p with score = p.score + s }

let rec remove_from_t c letters =
  match letters with
  | [] -> letters
  | h :: t -> if h = c then t else h :: remove_from_t c t

let rec remove_letters lst p =
  match lst with
  | [] -> p
  | h :: t ->
      if List.exists (fun x -> x = h) p.letters then
        remove_letters t { p with letters = remove_from_t h p.letters }
      else raise (InvalidChar (Tile.get_letter h))

let rec tile_list_to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> String.make 1 (Tile.get_letter h) ^ tile_list_to_string t

let get_hand p = tile_list_to_string p.letters
