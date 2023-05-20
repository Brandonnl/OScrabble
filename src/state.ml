open Player
open Board

type t = {
  players : Player.t list;
  turn : int;
  board : Board.t;
  transient_tiles : (Tile.t * int * int) list;
  bag : Bag.t;
}

type result =
  | Legal of (t * t)
  | Illegal of string

let rec flat_t_tiles (x : (Tile.t * int * int) list) =
  match x with
  | [] -> []
  | (tile, _, _) :: t -> tile :: flat_t_tiles t

let rec make_players names =
  match names with
  | [] -> []
  | h :: t -> Player.new_player h :: make_players t

let rec fill_hands bag (lst : Player.t list) : Player.t list * Bag.t =
  match lst with
  | [] -> ([], bag)
  | h :: t -> begin
      match Player.fill_hand (Player.get_letters h) bag with
      | fs, snd -> (Player.set_letters h fs :: fst (fill_hands bag t), snd)
    end

let rec get_names players =
  match players with
  | [] -> []
  | h :: t -> Player.get_name h :: get_names t

let get_players s = s.players
let player_size s = List.length s.players
let player_turn s = List.nth s.players s.turn

let new_state_helper names =
  {
    players = make_players names;
    turn = 0;
    board = Board.make ();
    transient_tiles = [];
    bag = Bag.create_bag_no_blanks ();
  }

let new_state names =
  let st = new_state_helper names in
  match fill_hands st.bag st.players with
  | fst, snd -> { st with players = fst; bag = snd }

let rec fill_players_hand (players : Player.t list) (st : t) (bag : Bag.t) :
    Player.t list * Bag.t =
  match players with
  | [] -> ([], st.bag)
  | h :: t -> (
      match fill_hand h.letters bag with
      | new_hand, new_bag ->
          ( { h with letters = new_hand } :: fst (fill_players_hand t st new_bag),
            new_bag ))

let rec add_players_hand players pl letters =
  match players with
  | [] -> []
  | h :: t ->
      if h = pl then set_letters h (letters @ h.letters) :: t
      else h :: add_players_hand t pl letters

let rec add_players_score players st score =
  match players with
  | [] -> []
  | h :: t ->
      if h = player_turn st then add_score score h :: t
      else h :: add_players_score t st score

let rec remove_players_hand players st letters =
  match players with
  | [] -> []
  | h :: t ->
      if h = player_turn st then remove_letters letters h :: t
      else h :: remove_players_hand t st letters

let rec tile_list_to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> String.make 1 (Tile.get_letter h) ^ tile_list_to_string t

let copy_state st1 st2 =
  Board.set_board st1.board st2.board;
  {
    players = st1.players;
    turn = st1.turn;
    board = st2.board;
    transient_tiles = [];
    bag = st1.bag;
  }

let rec hstart_idx board x y =
  if x = 0 then 0
  else if Board.used_square board (x - 1) y then hstart_idx board (x - 1) y
  else x

let rec hend_idx board x y =
  if x = 14 then 14
  else if Board.used_square board (x + 1) y then hend_idx board (x + 1) y
  else x

let rec vstart_idx board x y =
  if y = 0 then 0
  else if Board.used_square board x (y - 1) then vstart_idx board x (y - 1)
  else y

let rec vend_idx board x y =
  if y = 14 then 14
  else if Board.used_square board x (y + 1) then vend_idx board x (y + 1)
  else y

let check_horizontal (board : Board.t) (x : int) (y : int) : Tile.t list =
  let start_int = hstart_idx board x y in
  let end_int = hend_idx board x y in
  let rec get_tile_list (idx : int) (end_idx : int) =
    if idx = end_idx then [ Board.get_tile board idx y ]
    else Board.get_tile board idx y :: get_tile_list (idx + 1) end_idx
  in
  get_tile_list start_int end_int

let get_horizontal_score (board : Board.t) (x : int) (y : int) : int =
  let start_int = hstart_idx board x y in
  let end_int = hend_idx board x y in
  let rec get_tile_score (idx : int) (end_idx : int) =
    if idx = end_idx then
      match Board.get_special_sq idx y with
      | "TL" -> 3 * Tile.get_points (Board.get_tile board idx y)
      | "DL" -> 3 * Tile.get_points (Board.get_tile board idx y)
      | _ -> Tile.get_points (Board.get_tile board idx y)
    else
      match Board.get_special_sq idx y with
      | "TL" ->
          (3 * Tile.get_points (Board.get_tile board idx y))
          + get_tile_score (idx + 1) end_idx
      | "DL" ->
          (3 * Tile.get_points (Board.get_tile board idx y))
          + get_tile_score (idx + 1) end_idx
      | _ ->
          Tile.get_points (Board.get_tile board idx y)
          + get_tile_score (idx + 1) end_idx
  in
  let score_no_wmult = get_tile_score start_int end_int in
  let rec find_wmult idx end_idx =
    match (idx = end_idx, Board.get_special_sq idx y) with
    | true, "TW" -> 3
    | false, "DW" -> 2
    | _, "TW" -> 3 * find_wmult (idx + 1) end_idx
    | _, "DW" -> 2 * find_wmult (idx + 1) end_idx
    | _, _ -> find_wmult (idx + 1) end_idx
  in
  score_no_wmult * find_wmult start_int end_int

let rec check_vertical (board : Board.t) (x : int) (y : int) : Tile.t list =
  let start_int = vstart_idx board x y in
  let end_int = vend_idx board x y in
  let rec get_tile_list (idx : int) (end_idx : int) =
    if idx = end_idx then [ Board.get_tile board x idx ]
    else Board.get_tile board x idx :: get_tile_list (idx + 1) end_idx
  in
  get_tile_list start_int end_int

let get_vertical_score (board : Board.t) (x : int) (y : int) : int =
  let start_int = vstart_idx board x y in
  let end_int = vend_idx board x y in
  let rec get_tile_score (idx : int) (end_idx : int) =
    if idx = end_idx then
      match Board.get_special_sq idx y with
      | "TL" -> 3 * Tile.get_points (Board.get_tile board x idx)
      | "DL" -> 3 * Tile.get_points (Board.get_tile board x idx)
      | _ -> Tile.get_points (Board.get_tile board x idx)
    else
      match Board.get_special_sq idx y with
      | "TL" ->
          (3 * Tile.get_points (Board.get_tile board x idx))
          + get_tile_score (idx + 1) end_idx
      | "DL" ->
          (3 * Tile.get_points (Board.get_tile board x idx))
          + get_tile_score (idx + 1) end_idx
      | _ ->
          Tile.get_points (Board.get_tile board x idx)
          + get_tile_score (idx + 1) end_idx
  in
  let score_no_wmult = get_tile_score start_int end_int in
  let rec find_wmult idx end_idx =
    match (idx = end_idx, Board.get_special_sq x idx) with
    | true, "TW" -> 3
    | false, "DW" -> 2
    | _, "TW" -> 3 * find_wmult (idx + 1) end_idx
    | _, "DW" -> 2 * find_wmult (idx + 1) end_idx
    | _, _ -> find_wmult (idx + 1) end_idx
  in
  score_no_wmult * find_wmult start_int end_int

let get_transient_dim st =
  let trans_lst = st.transient_tiles in
  let rec split lst =
    match lst with
    | [] -> ([], [])
    | (t1, x1, y1) :: t ->
        let rest = split t in
        (x1 :: fst rest, y1 :: snd rest)
  in
  let xlst, ylst = split trans_lst in
  if List.length xlst != List.length ylst then failwith "check_transient error"
  else
    match (xlst, ylst) with
    | [], [] -> ""
    | x1 :: t1, y1 :: t2 ->
        if List.for_all (fun x -> x = x1) t1 then "V"
        else if List.for_all (fun y -> y = y1) t2 then "H"
        else "Neither"
    | _, _ -> failwith "shouldn't reach"

let helperx (board : Board.t) st =
  let tlst = st.transient_tiles in
  match tlst with
  | [] -> 0
  | (_, x, y) :: t ->
      let htile_score = get_horizontal_score board x y in
      let rec helper lst =
        match lst with
        | [] -> 0
        | (_, x, y) :: t ->
            let vlst = get_vertical_score board x y in
            vlst + helper t
      in
      let vtile_score = helper tlst in
      htile_score + vtile_score

let helpery board st =
  let tlst = st.transient_tiles in
  match tlst with
  | [] -> 0
  | (_, x, y) :: t ->
      let vtile_lst = get_vertical_score board x y in
      let rec helper lst =
        match lst with
        | [] -> 0
        | (_, x, y) :: t ->
            let hlst = get_horizontal_score board x y in
            hlst + helper t
      in
      let htile_score = helper tlst in
      htile_score + vtile_lst

let calc_score state =
  let board = state.board in
  let dim = get_transient_dim state in
  match dim with
  | "H" -> helperx board state
  | "V" -> helpery board state
  | _ -> 0

let rec calc_score1 lst = Tile.get_points_list (flat_t_tiles lst)
let efficient_score = true

let next_turn (o_st : t) (c_st : t) =
  let filled_players =
    fill_players_hand
      (add_players_score c_st.players c_st
         (if efficient_score then calc_score1 c_st.transient_tiles
         else calc_score c_st))
      c_st c_st.bag
  in
  let new_st =
    {
      c_st with
      turn = (c_st.turn + 1) mod player_size c_st;
      transient_tiles = [];
      players = fst filled_players;
      bag = snd filled_players;
    }
  in
  (copy_state new_st o_st, new_st)

let convert_ascii x = Char.code (Char.uppercase_ascii x) - Char.code 'A'

let place_tile o_st c_st x y tile =
  try
    let cx = convert_ascii x in
    if Board.used_square c_st.board cx y then Illegal "Tile is taken"
    else if List.exists (fun x -> x = tile) (player_turn c_st).letters then
      Legal
        ( o_st,
          {
            players = remove_players_hand c_st.players c_st [ tile ];
            turn = c_st.turn;
            board = Board.place_tile cx y tile c_st.board;
            transient_tiles = (tile, cx, y) :: c_st.transient_tiles;
            bag = c_st.bag;
          } )
    else Illegal "Player hand does not contain tile"
  with _ -> Illegal "Invalid command"

let get_board st = st.board
let inc_turn st = { st with turn = (st.turn + 1) mod player_size st }

let rec to_words (lst : Tile.t list) =
  match lst with
  | [] -> ""
  | h :: t -> String.make 1 (Tile.get_letter h) ^ to_words t

let to_wordt str = Word.from_string str

let lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let valid_tile board x y : bool =
  let (lst : string list) = lines ("bin" ^ Filename.dir_sep ^ "dict.txt") in
  if Board.used_square board x y then
    (*if square has no tile, tile cannot be invalid*)
    let hword = to_words (check_horizontal board x y) in
    let vword = to_words (check_vertical board x y) in
    let hnone = String.length hword = 1 in
    let vnone = String.length vword = 1 in
    if hnone && vnone then false
    else
      let hval = Word.check_valid lst (to_wordt hword) in
      let vval = Word.check_valid lst (to_wordt vword) in
      match (hval, vval, hnone, vnone) with
      | true, true, _, _ -> true (*valid horizontal and verical word*)
      | true, _, _, true -> true (*valid horizontal word, no vertical word*)
      | _, true, true, _ -> true (*valid vertical word, no horizontal word*)
      | _ -> false (*else invalid*)
  else true (*if square has no tile, tile cannot be invalid*)

let rec board_helper board x y =
  if board = Board.make () then true
  else
    Board.used_square board 7 7 (*if board isnt empty, center must be full*)
    &&
    match (x, y) with
    | 14, 14 -> valid_tile board 14 14
    | 14, _ -> valid_tile board x y && board_helper board 0 (y + 1)
    | _, _ -> valid_tile board x y && board_helper board (x + 1) y

let check_transient st =
  let trans_lst = st.transient_tiles in
  let rec split lst =
    match lst with
    | [] -> ([], [])
    | (t1, x1, y1) :: t ->
        let rest = split t in
        (x1 :: fst rest, y1 :: snd rest)
  in
  let xlst, ylst = split trans_lst in
  if List.length xlst != List.length ylst then false
  else
    match (xlst, ylst) with
    | [], [] -> true
    | x1 :: t1, y1 :: t2 ->
        List.for_all (fun x -> x = x1) t1 || List.for_all (fun y -> y = y1) t2
    | _, _ -> failwith "shouldn't reach"

let valid_board t : bool = check_transient t && board_helper t.board 0 0