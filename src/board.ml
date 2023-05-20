open Tile

exception NoTile

type t' =
  | Full of Tile.t
  | TripleWord
  | DoubleWord
  | TripleLetter
  | DoubleLetter
  | Empty

type t = t' array array

let make_empty int1 : t = Array.make_matrix int1 int1 Empty

let make () : t =
  let row1 =
    [|
      TripleWord;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      TripleWord;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      TripleWord;
    |]
  in
  let row2 =
    [|
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
    |]
  in
  let row3 =
    [|
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
    |]
  in
  let row4 =
    [|
      DoubleLetter;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      DoubleLetter;
    |]
  in
  let row5 =
    [|
      Empty;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      Empty;
    |]
  in
  let row6 =
    [|
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
    |]
  in
  let row7 =
    [|
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
    |]
  in
  let row8 =
    [|
      TripleWord;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      TripleWord;
    |]
  in
  let row9 =
    [|
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
    |]
  in
  let row10 =
    [|
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
    |]
  in
  let row11 =
    [|
      Empty;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      Empty;
    |]
  in
  let row12 =
    [|
      DoubleLetter;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      DoubleLetter;
    |]
  in
  let row13 =
    [|
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
      Empty;
    |]
  in
  let row14 =
    [|
      Empty;
      DoubleWord;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      TripleLetter;
      Empty;
      Empty;
      Empty;
      DoubleWord;
      Empty;
    |]
  in
  let row15 =
    [|
      TripleWord;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      Empty;
      TripleWord;
      Empty;
      Empty;
      Empty;
      DoubleLetter;
      Empty;
      Empty;
      TripleWord;
    |]
  in
  [|
    row1;
    row2;
    row3;
    row4;
    row5;
    row6;
    row7;
    row8;
    row9;
    row10;
    row11;
    row12;
    row13;
    row14;
    row15;
  |]

let used_square board x y =
  match board.(y).(x) with
  | Full _ -> true
  | _ -> false

let get_tile board x y =
  let spot = Array.get (Array.get board y) x in
  match spot with
  | Full a -> a
  | _ -> raise NoTile

let str_of_tile tile =
  match tile with
  | Full a -> String.make 1 (get_letter a)
  | TripleWord -> "@"
  | DoubleWord -> "*"
  | TripleLetter -> "+"
  | DoubleLetter -> "-"
  | Empty -> " "

let arr_to_str f x arr =
  let y = if x < 10 then string_of_int x ^ " " else string_of_int x in
  y ^ Array.fold_left f "|" arr ^ "|" ^ y

let f acc tile = acc ^ str_of_tile tile ^ " "

let rec board_of_arr board acc n =
  if n = 15 then acc
  else
    let new_acc = acc ^ "\n" ^ arr_to_str f n (Array.get board n) in
    board_of_arr board new_acc (n + 1)

let to_string board =
  let index = "   A B C D E F G H I J K L M N O   " in
  let spacer = "   - - - - - - - - - - - - - - -    " in
  let body = board_of_arr board "" 0 in
  index ^ "\n" ^ spacer ^ body ^ "\n" ^ spacer ^ "\n" ^ index

let get_board (board : t) = board
let copy_board (b : t) : t = Array.map Array.copy b

let set_board (b1 : t) (b2 : t) : unit =
  let helper board1 board2 x y =
    let spot = Array.get (Array.get board1 y) x in
    Array.set (Array.get board2 y) x spot
  in
  let rec iterate board1 board2 x y =
    if x < 0 || y < 0 || x > 15 || y > 15 then failwith "ERROR"
    else
      let _ = helper board1 board2 x y in
      if x = 14 && y = 14 then ()
      else if y = 14 then iterate board1 board2 (x + 1) 0
      else iterate board1 board2 x (y + 1)
  in
  iterate b1 b2 0 0

let out_of_bounds (board : t) x y =
  if
    x < Array.length board
    && x > 1 && y > 1
    && y < Array.length (Array.get board 0)
  then true
  else false

let place_tile x y (t : Tile.t) (board : t) =
  Array.set (Array.get board y) x (Full t);
  board

let get_special_sq x y =
  match (x, y) with
  | 0, 0 | 0, 7 | 0, 14 | 7, 0 | 7, 14 | 14, 0 | 14, 7 | 14, 14 -> "TW"
  | 1, 1 | 2, 2 | 3, 3 | 4, 4 | 1, 13 | 2, 12 | 3, 11 | 4, 10 -> "DW"
  | 13, 1 | 12, 2 | 11, 3 | 10, 4 | 10, 10 | 11, 11 | 12, 12 | 13, 13 -> "DW"
  | 1, 5 | 1, 9 | 5, 1 | 5, 5 | 5, 9 | 5, 13 -> "TL"
  | 13, 5 | 13, 9 | 9, 1 | 9, 5 | 9, 9 | 9, 13 -> "TL"
  | 0, 3 | 0, 11 | 2, 6 | 2, 8 | 3, 0 | 3, 7 | 3, 14 -> "DL"
  | 6, 2 | 6, 6 | 6, 8 | 6, 12 | 7, 3 | 7, 11 -> "DL"
  | 8, 2 | 8, 6 | 8, 8 | 8, 12 -> "DL"
  | 14, 3 | 14, 11 | 12, 6 | 12, 8 | 11, 0 | 11, 7 | 11, 14 -> "DL"
  | _ -> "NO"
