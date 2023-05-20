(* coordinates on transient *)
open Scrabble
open Command
open State
open Tile
open Player

(** [erase_above] clears the the text in the terminal to maintain cleaniness in
    the interface. *)
let rec erase_above () =
  ANSITerminal.erase Above;
  ANSITerminal.set_cursor 0 0

let two_players =
  "████████╗ ██╗  ██╗ ███████╗\n\
   ╚══██╔══╝ ██║  ██║ ██╔════╝\n\
  \   ██║    ███████║ █████╗  \n\
  \   ██║    ██╔══██║ ██╔══╝  \n\
  \   ██║    ██║  ██║ ███████╗\n\
  \   ╚═╝    ╚═╝  ╚═╝ ╚══════╝\n\n\
  \ ██████╗   █████╗  ███╗   ███╗ ███████╗\n\
   ██╔════╝  ██╔══██╗ ████╗ ████║ ██╔════╝\n\
   ██║  ███╗ ███████║ ██╔████╔██║ █████╗  \n\
   ██║   ██║ ██╔══██║ ██║╚██╔╝██║ ██╔══╝  \n\
   ╚██████╔╝ ██║  ██║ ██║ ╚═╝ ██║ ███████╗\n\
  \ ╚═════╝  ╚═╝  ╚═╝ ╚═╝     ╚═╝ ╚══════╝\n\n\
   ██╗  ██╗  █████╗  ███████╗\n\
   ██║  ██║ ██╔══██╗ ██╔════╝\n\
   ███████║ ███████║ ███████╗\n\
   ██╔══██║ ██╔══██║ ╚════██║\n\
   ██║  ██║ ██║  ██║ ███████║\n\
   ╚═╝  ╚═╝ ╚═╝  ╚═╝ ╚══════╝\n\n\
   ██████╗      ██████╗  ██╗       █████╗  ██╗   ██╗ ███████╗ ██████╗  \
   ███████╗ ██╗\n\
   ╚════██╗     ██╔══██╗ ██║      ██╔══██╗ ╚██╗ ██╔╝ ██╔════╝ ██╔══██╗ \
   ██╔════╝ ██║\n\
  \ █████╔╝     ██████╔╝ ██║      ███████║  ╚████╔╝  █████╗   ██████╔╝ \
   ███████╗ ██║\n\
   ██╔═══╝      ██╔═══╝  ██║      ██╔══██║   ╚██╔╝   ██╔══╝   ██╔══██╗ \
   ╚════██║ ╚═╝\n\
   ███████╗     ██║      ███████╗ ██║  ██║    ██║    ███████╗ ██║  ██║ \
   ███████║ ██╗\n\
   ╚══════╝     ╚═╝      ╚══════╝ ╚═╝  ╚═╝    ╚═╝    ╚══════╝ ╚═╝  ╚═╝ \
   ╚══════╝ ╚═╝\n\n"

let three_players =
  "     ████████╗ ██╗  ██╗ ███████╗\n\
  \    ╚══██╔══╝ ██║  ██║ ██╔════╝\n\
  \       ██║    ███████║ █████╗  \n\
  \       ██║    ██╔══██║ ██╔══╝  \n\
  \       ██║    ██║  ██║ ███████╗\n\
  \       ╚═╝    ╚═╝  ╚═╝ ╚══════╝\n\
  \   \n\
  \     ██████╗   █████╗  ███╗   ███╗ ███████╗\n\
  \    ██╔════╝  ██╔══██╗ ████╗ ████║ ██╔════╝\n\
  \    ██║  ███╗ ███████║ ██╔████╔██║ █████╗  \n\
  \    ██║   ██║ ██╔══██║ ██║╚██╔╝██║ ██╔══╝  \n\
  \    ╚██████╔╝ ██║  ██║ ██║ ╚═╝ ██║ ███████╗\n\
  \     ╚═════╝  ╚═╝  ╚═╝ ╚═╝     ╚═╝ ╚══════╝\n\
  \   \n\
  \    ██╗  ██╗  █████╗  ███████╗\n\
  \    ██║  ██║ ██╔══██╗ ██╔════╝\n\
  \    ███████║ ███████║ ███████╗\n\
  \    ██╔══██║ ██╔══██║ ╚════██║\n\
  \    ██║  ██║ ██║  ██║ ███████║\n\
  \    ╚═╝  ╚═╝ ╚═╝  ╚═╝ ╚══════╝\n\
  \   \n\
  \    ██████╗      ██████╗  ██╗       █████╗  ██╗   ██╗ ███████╗ ██████╗  \
   ███████╗ ██╗\n\
  \    ╚════██╗     ██╔══██╗ ██║      ██╔══██╗ ╚██╗ ██╔╝ ██╔════╝ ██╔══██╗ \
   ██╔════╝ ██║\n\
  \     █████╔╝     ██████╔╝ ██║      ███████║  ╚████╔╝  █████╗   ██████╔╝ \
   ███████╗ ██║\n\
  \     ╚═══██╗     ██╔═══╝  ██║      ██╔══██║   ╚██╔╝   ██╔══╝   ██╔══██╗ \
   ╚════██║ ╚═╝\n\
  \    ██████╔╝     ██║      ███████╗ ██║  ██║    ██║    ███████╗ ██║  ██║ \
   ███████║ ██╗\n\
  \    ╚═════╝      ╚═╝      ╚══════╝ ╚═╝  ╚═╝    ╚═╝    ╚══════╝ ╚═╝  ╚═╝ \
   ╚══════╝ ╚═╝\n\n"

let four_players =
  " ████████╗ ██╗  ██╗ ███████╗\n\
  \ ╚══██╔══╝ ██║  ██║ ██╔════╝\n\
  \    ██║    ███████║ █████╗  \n\
  \    ██║    ██╔══██║ ██╔══╝  \n\
  \    ██║    ██║  ██║ ███████╗\n\
  \    ╚═╝    ╚═╝  ╚═╝ ╚══════╝\n\n\
  \  ██████╗   █████╗  ███╗   ███╗ ███████╗\n\
  \ ██╔════╝  ██╔══██╗ ████╗ ████║ ██╔════╝\n\
  \ ██║  ███╗ ███████║ ██╔████╔██║ █████╗  \n\
  \ ██║   ██║ ██╔══██║ ██║╚██╔╝██║ ██╔══╝  \n\
  \ ╚██████╔╝ ██║  ██║ ██║ ╚═╝ ██║ ███████╗\n\
  \  ╚═════╝  ╚═╝  ╚═╝ ╚═╝     ╚═╝ ╚══════╝\n\n\
  \ ██╗  ██╗  █████╗  ███████╗\n\
  \ ██║  ██║ ██╔══██╗ ██╔════╝\n\
  \ ███████║ ███████║ ███████╗\n\
  \ ██╔══██║ ██╔══██║ ╚════██║\n\
  \ ██║  ██║ ██║  ██║ ███████║\n\
  \ ╚═╝  ╚═╝ ╚═╝  ╚═╝ ╚══════╝\n\n\
  \ ██╗  ██╗     ██████╗  ██╗       █████╗  ██╗   ██╗ ███████╗ ██████╗  \
   ███████╗ ██╗\n\
  \ ██║  ██║     ██╔══██╗ ██║      ██╔══██╗ ╚██╗ ██╔╝ ██╔════╝ ██╔══██╗ \
   ██╔════╝ ██║\n\
  \ ███████║     ██████╔╝ ██║      ███████║  ╚████╔╝  █████╗   ██████╔╝ \
   ███████╗ ██║\n\
  \ ╚════██║     ██╔═══╝  ██║      ██╔══██║   ╚██╔╝   ██╔══╝   ██╔══██╗ \
   ╚════██║ ╚═╝\n\
  \      ██║     ██║      ███████╗ ██║  ██║    ██║    ███████╗ ██║  ██║ \
   ███████║ ██╗\n\
  \      ╚═╝     ╚═╝      ╚══════╝ ╚═╝  ╚═╝    ╚═╝    ╚══════╝ ╚═╝  ╚═╝ \
   ╚══════╝ ╚═╝\n\n"

let print_player input =
  if input = 2 then ANSITerminal.print_string [ ANSITerminal.blue ] two_players
  else if input = 3 then
    ANSITerminal.print_string [ ANSITerminal.blue ] three_players
  else ANSITerminal.print_string [ ANSITerminal.blue ] four_players

let try_command comm o_st c_st =
  match parse comm with
  | exception Empty -> Illegal "Bag is empty"
  | exception Malform -> Illegal "Inavlid command"
  | Quit ->
      print_endline "Leaving scrabble. Goodbye!";
      exit 0
  | Place_Tile [ x; y; t ] ->
      place_tile o_st c_st (String.get x 0) (int_of_string y)
        (create_tile (String.get t 0))
  | Place_Tile _ -> Illegal "Invalid Tile Placement"
  | Help ->
      Illegal
        ("\n Here's how to play:\n "
       ^ "1. You have a gameboard, 100\n\
         \       letter tiles, a letter bag\n\
         \ 2. You will be promted to give the number\n\
         \       of players that you'd like  to play the game (2-4). \n\
         \ 3. Each player\n\
         \       will automatically be drawn 7 tiles.\n\
         \ 4. Once the game has started,\n\
         \       you can place the tile by typing in  the terminal the following\n\
         \       format: (x (A-O), y (0-14), *your  word*). \n\
         \ 5. When you place your\n\
         \       word, you will be able to see the player's  words on the side. \n\
         \ 6.\n\
         \       You can simply end your turn by typing \"end turn \".\n\
         \ 7. When a\n\
         \       player plays their last tile, and no more tiles are  \
          availible to\n\
         \       draw, then the game is over. The player with the  highest \
          score wins\n\
         \       the game.\n\
         \ 8. Have fun! You can type in help at any point if you're\n\
         \       stuck.\n\
         \  ")
  | Endturn ->
      if State.valid_board c_st then Legal (State.next_turn o_st c_st)
      else Illegal "Not a valid board state"
  | Clear ->
      erase_above ();
      Legal (o_st, c_st)
  | Reset -> Legal (o_st, State.copy_state o_st c_st)

(** [play_game f] starts the scrabble game. *)
let lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let (list : string list) = lines ("bin" ^ Filename.dir_sep ^ "dict.txt")

let rec num_player () : int =
  erase_above ();
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    " ██╗    ██╗ ███████╗ ██╗       ██████╗  ██████╗  ███╗   ███╗ ███████╗\n\
    \  ██║    ██║ ██╔════╝ ██║      ██╔════╝ ██╔═══██╗ ████╗ ████║ ██╔════╝\n\
    \  ██║ █╗ ██║ █████╗   ██║      ██║      ██║   ██║ ██╔████╔██║ █████╗  \n\
    \  ██║███╗██║ ██╔══╝   ██║      ██║      ██║   ██║ ██║╚██╔╝██║ ██╔══╝  \n\
    \  ╚███╔███╔╝ ███████╗ ███████╗ ╚██████╗ ╚██████╔╝ ██║ ╚═╝ ██║ ███████╗\n\
    \   ╚══╝╚══╝  ╚══════╝ ╚══════╝  ╚═════╝  ╚═════╝  ╚═╝     ╚═╝ ╚══════╝\n\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "  ████████╗  ██████╗ \n\
    \  ╚══██╔══╝ ██╔═══██╗\n\
    \     ██║    ██║   ██║\n\
    \     ██║    ██║   ██║\n\
    \     ██║    ╚██████╔╝\n\
    \     ╚═╝     ╚═════╝ \n\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "   ███████╗  ██████╗ ██████╗   █████╗  ██████╗  ██████╗  ██╗      \
     ███████╗ ██╗\n\
    \  ██╔════╝ ██╔════╝ ██╔══██╗ ██╔══██╗ ██╔══██╗ ██╔══██╗ ██║      ██╔════╝ \
     ██║\n\
    \  ███████╗ ██║      ██████╔╝ ███████║ ██████╔╝ ██████╔╝ ██║      █████╗   \
     ██║\n\
    \  ╚════██║ ██║      ██╔══██╗ ██╔══██║ ██╔══██╗ ██╔══██╗ ██║      ██╔══╝   \
     ╚═╝\n\
    \  ███████║ ╚██████╗ ██║  ██║ ██║  ██║ ██████╔╝ ██████╔╝ ███████╗ ███████╗ \
     ██╗\n\
    \  ╚══════╝  ╚═════╝ ╚═╝  ╚═╝ ╚═╝  ╚═╝ ╚═════╝  ╚═════╝  ╚══════╝ ╚══════╝ \
     ╚═╝";
  print_endline "\n\n\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Enter the number of players (2-4). \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> num_player ()
  | num -> (
      try
        let input = int_of_string num in
        if Player.valid_player input then begin
          erase_above ();
          print_player input;
          input
        end
        else begin
          print_endline
            "This is an invalid number of players, please try again. \n";
          num_player ()
        end
      with Failure _ -> num_player ())

let rec play_game word =
  if Word.check_valid list word then (
    print_endline "Valid \n";
    word |> Word.word_value |> string_of_int |> String.cat "Word value: "
    |> print_endline;
    print_endline
      (string_of_int (Bag.size_bag (snd (Bag.draw_tile (Bag.create_bag ()))))
      ^ " letters remaining"))
  else (
    print_endline "Invalid \n";
    match read_line () with
    | exception End_of_file -> ()
    | word -> word |> String.uppercase_ascii |> Word.from_string |> play_game)

let rec get_names n n0 : string list =
  let n0 = ref n0 in
  if !n0 = n then []
  else begin
    print_newline ();
    print_endline ("Enter player " ^ string_of_int !n0 ^ "'s name");
    print_string "─────────────────────────";
    print_newline ();
    match read_line () with
    | exception End_of_file -> get_names n !n0
    | name -> name :: get_names n (!n0 + 1)
  end

let show_all_hands = false

let rec play_scrabble (o_st : State.t) (c_st : State.t) (p_str : string) =
  print_instructions c_st p_str;
  match read_line () with
  | exception End_of_file -> ()
  | str -> (
      match try_command str o_st c_st with
      | Illegal s -> play_scrabble o_st c_st s
      | Legal (ost, new_st) -> play_scrabble ost new_st "")

and print_instructions st p_str =
  erase_above ();
  print_endline (Board.to_string (get_board st));
  print_newline ();
  print_string "───────────────────────────────────────────────────";
  print_newline ();
  print_newline ();
  print_endline "For more information on how to play the game: \"help\"";
  print_endline "To place a tile: \"X Y Letter\", i.e. \"A 2 B\"";
  print_endline "To reset your turn: \"reset\"";
  print_endline "To end turn: \"end\"";
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.red ] (p_str ^ "\n");
  print_endline "Place a tile >";
  ANSITerminal.save_cursor ();
  ANSITerminal.move_cursor 45 (-20);
  ANSITerminal.print_string [ ANSITerminal.green ]
    (get_name (player_turn st) ^ "'s turn\n\n");
  ANSITerminal.move_cursor 45 0;
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Your score is: " ^ string_of_int (get_score (player_turn st)) ^ "\n\n");
  ANSITerminal.move_cursor 45 0;
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Your hand is: " ^ get_hand (player_turn st) ^ "\n\n");
  let next_st = inc_turn st in
  ANSITerminal.move_cursor 75 (-6);

  ANSITerminal.print_string [ ANSITerminal.green ]
    (get_name (player_turn next_st) ^ "'s turn is next \n\n");
  ANSITerminal.move_cursor 75 0;
  if show_all_hands then
    ANSITerminal.print_string [ ANSITerminal.green ]
      (get_name (player_turn next_st)
      ^ "'s hand is: "
      ^ get_hand (player_turn next_st)
      ^ "\n\n");
  ANSITerminal.print_string [ ANSITerminal.green ]
    (get_name (player_turn next_st)
    ^ "'s score is: "
    ^ string_of_int (get_score (player_turn next_st))
    ^ "\n\n");
  if show_all_hands then (
    ANSITerminal.print_string [ ANSITerminal.green ]
      (get_name (player_turn next_st)
      ^ " hand is: "
      ^ get_hand (player_turn next_st)
      ^ "\n\n");
    ANSITerminal.move_cursor 80 0);
  ANSITerminal.move_cursor 0 20

let scrabble () =
  let n = num_player () in
  let o_st = State.new_state (get_names (n + 1) 1) in
  let c_st = State.new_state (State.get_names (State.get_players o_st)) in
  play_scrabble o_st (State.copy_state o_st c_st) ""

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
    \ Welcome to Scrabble! Enter start to start or type \"help\" for more \
     information.\n";
  print_string "> ";
  scrabble ()

(* Execute the game engine. *)
let () =
  try main ()
  with _ ->
    print_endline "Unexpected Error";
    main ()
