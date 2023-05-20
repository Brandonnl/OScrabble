(* Approach to Testing:

   - The tests cases were developed using glass box testing as there were very
   hard ways to come up with many test cases for black box testing.

   Module testing:

   - The majority of Board was tested manually since our Board is an array and
   mutable, which makes it much easier to test playing through the game rather
   than unit testing.

   - Tile and Bag were fully tested in OUnit as they had relatively simple
   methods to check without involving other modules.

   - Player and State had their getters, setters, and functions modifying the
   type t tested in OUnit. As state has many functions/helper functions that
   deal with other parts including Bag, Board, and multiple Players, these
   functions were tested manually since they were almost impossible to be tested
   with OUnit.

   Correctness:

   - Our testing approach demonstrates the correctness of our system as our test
   suite in OUnit will cover the basics of the system and base functionality of
   our game. All manual testing was done between every member of the team to
   ensure complicated functions would have no edge cases break the game. We have
   done many run throughs of the game trying to catch any possible mistakes and
   are satisfied with the working implementation of Scrabble *)

open OUnit2
open Scrabble
open Bag
open Tile
open Command
open Player

let temp_hand1 = [ Tile.create_tile 'c' ]
let temp_hand2 = [ Tile.create_tile 'd'; Tile.create_tile 'l' ]

let temp_hand3 =
  [
    Tile.create_tile 'l';
    Tile.create_tile 'c';
    Tile.create_tile 'e';
    Tile.create_tile 'p';
    Tile.create_tile 'd';
    Tile.create_tile 'q';
    Tile.create_tile 'u';
  ]

let tile_test name chr expec =
  name >:: fun _ -> assert_equal expec (Tile.get_letter (Tile.create_tile chr))

let tile_point_test name chr expec =
  name >:: fun _ -> assert_equal expec (Tile.get_points (Tile.create_tile chr))

let get_point_list_test name expected_output lst =
  name >:: fun _ ->
  assert_equal expected_output (Tile.get_points_list lst) ~printer:string_of_int

let create_tile_tests =
  [
    tile_test "getting the tile A with 'a'" 'a' 'A';
    tile_test "getting the tile C with 'c'" 'c' 'C';
    tile_test "getting the tile C with 'C'" 'C' 'C';
    tile_test
      "getting the tile 0, this isn't possible in a normal game but the user \
       can't control the tiles"
      '0' '0';
    tile_point_test
      "getting score of any tile that isn't a letter is 10, also not possible \
       but again user has no control over tiles"
      '0' 10;
    tile_point_test "getting the score for A using create_tile" 'a' 1;
    tile_point_test "getting the score of a blank tile using create_tile" ' ' 0;
    get_point_list_test "Points for the list C is 3" 3 temp_hand1;
    get_point_list_test "Points for the list DL is 3" 3 temp_hand2;
    get_point_list_test "Points for the list LCEPDQU is 21" 21 temp_hand3;
  ]

(* This test prints the board out, commented out of board tests to reduce
   clutter *)
let print_board_test name =
  name >:: fun _ ->
  let board = Board.make () in
  let board_str = Board.to_string board in
  assert_equal 1 1;
  print_endline board_str

let place_tile_test expected name x y letter =
  let board = Board.place_tile x y (create_tile letter) (Board.make ()) in
  name >:: fun _ ->
  assert_equal (create_tile expected) (Board.get_tile board x y)

let board_tests =
  [
    (* print_board_test ""; *)
    place_tile_test 'A' "Should be A" 1 1 'a';
    place_tile_test 'B' "Should be B" 1 1 'b';
  ]

let bag_empty_test name expected_output bag =
  name >:: fun _ -> assert_equal expected_output (is_empty bag)

let bag_empty_tests =
  [
    bag_empty_test "testing for an empty bag" true (Bag.empty ());
    bag_empty_test "testing for an non-empty bag" false (create_bag ());
  ]

let size_test name expected_output bag =
  name >:: fun _ ->
  assert_equal expected_output (size_bag bag) ~printer:string_of_int

let print_bag_test name expected_output bag =
  name >:: fun _ -> assert_equal expected_output (print_bag bag) ~printer:Fun.id

let size_assert_test name expected_output bag =
  name >:: fun _ -> assert_raises expected_output (fun () -> size_bag bag)

let bag1 = Bag.create_bag ()

let bag_tests =
  [
    size_test "testing the size of the bag" 100 bag1;
    size_test "testing for when an a tile drawn" 99 (snd (draw_tile bag1));
    print_bag_test "remove a specific letter from the bag"
      "YYXWWVVUUUUTTTTTTSSSSRRRRRRQPPOOOOOOOONNNNNNMMLLLLKJIIIIIIIIIHHGGGFFEEEEEEEEEEEEDDDDCCBBAAAAAAAAA  "
      (remove 'Z' bag1);
    size_test "drawing a tile after a tile has been removed " 98
      (snd (draw_tile (remove 'Z' bag1)));
  ]

let check_verb_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (Command.check_verb input)

let excep_check_verb_test name expected_output input =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> Command.check_verb input)

let check_verb_tests =
  [
    check_verb_test "tests for quit" Quit [ "quit" ];
    check_verb_test "tests for clear" Clear [ "clear" ];
    check_verb_test "tests for end" Endturn [ "end" ];
    check_verb_test "tests for help" Help [ "help" ];
    check_verb_test "tests for endturn" Endturn [ "endturn" ];
    check_verb_test "tests for reset" Reset [ "reset" ];
    check_verb_test "tests for place_tile"
      (Place_Tile [ "a"; "2"; "hi" ])
      [ "a"; "2"; "hi" ];
    excep_check_verb_test "checks for an empty command" Command.Empty [];
    excep_check_verb_test "checks for an malformed command" Command.Malform
      [ "asdasd" ];
    excep_check_verb_test "checks for an misplacement of tile" Command.Malform
      [ "a"; "b"; "bad" ];
    excep_check_verb_test "checks for an misplacement of tile with malform y"
      Command.Malform [ "1"; "b"; "bad" ];
    excep_check_verb_test "checks for an misplacement of tile with malform x"
      Command.Malform [ "2"; "2"; "bad" ];
  ]

let get_letter_test name expected_output tile =
  name >:: fun _ ->
  assert_equal expected_output (Tile.get_letter tile) ~printer:Char.escaped

let get_letter_tests =
  [
    get_letter_test "Regular letter on a tile" 'C' (Tile.create_tile 'c');
    get_letter_test "Blank tile test" ' ' (Tile.create_tile ' ');
    get_letter_test "UsedBlank tile test" ' ' (Tile.create_tile ' ');
  ]

let get_point_char_test name expected_output chr =
  name >:: fun _ ->
  assert_equal expected_output (Tile.get_point_char chr) ~printer:string_of_int

let get_point_char_tests =
  [
    get_point_char_test "testing conversion of lowercase to uppercase chr" 1 'a';
    get_point_char_test "testing 1 point letters" 1 'A';
    get_point_char_test "testing 1 point letters" 1 'E';
    get_point_char_test "testing 1 point letters" 1 'I';
    get_point_char_test "testing 1 point letters" 1 'L';
    get_point_char_test "testing 1 point letters" 1 'N';
    get_point_char_test "testing 1 point letters" 1 'O';
    get_point_char_test "testing 1 point letters" 1 'R';
    get_point_char_test "testing 1 point letters" 1 'S';
    get_point_char_test "testing 1 point letters" 1 'T';
    get_point_char_test "testing 1 point letters" 1 'U';
    get_point_char_test "testing 2 point letters" 2 'D';
    get_point_char_test "testing 2 point letters" 2 'G';
    get_point_char_test "testing 3 point letters" 3 'B';
    get_point_char_test "testing 3 point letters" 3 'C';
    get_point_char_test "testing 3 point letters" 3 'M';
    get_point_char_test "testing 3 point letters" 3 'P';
    get_point_char_test "testing 4 point letters" 4 'F';
    get_point_char_test "testing 4 point letters" 4 'H';
    get_point_char_test "testing 4 point letters" 4 'V';
    get_point_char_test "testing 4 point letters" 4 'W';
    get_point_char_test "testing 4 point letters" 4 'Y';
    get_point_char_test "testing 5 point letters" 5 'K';
    get_point_char_test "testing 8 point letters" 8 'J';
    get_point_char_test "testing 8 point letters" 8 'X';
    get_point_char_test "testing 10 point letters" 10 'Z';
    get_point_char_test "testing 10 point letters" 10 'Q';
  ]

let get_points_test name expected_output tile =
  name >:: fun _ ->
  assert_equal expected_output (Tile.get_points tile) ~printer:string_of_int

let get_points_tests =
  [
    get_points_test "testing points for blank" 0 (create_tile ' ');
    get_points_test "testing points for used blank" 0 (create_tile ' ');
    get_points_test "testing points for blank" 1 (create_tile 'a');
  ]

let check_valid_test name expected_output set word =
  name >:: fun _ ->
  assert_equal expected_output
    (Word.check_valid set word)
    ~printer:string_of_bool

let lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let list = lines ("bin" ^ Filename.dir_sep ^ "dict.txt")

let check_valid_tests =
  [
    check_valid_test "a word that is not in the set" false list
      (Word.from_string "i");
    check_valid_test "instant is lowercase so it is not in the set" false list
      (Word.from_string "instant");
    check_valid_test "INSTANT is a word thats in the set" true list
      (Word.from_string "INSTANT");
    check_valid_test "a word thats in the set" true list
      (Word.from_string "NICE");
  ]

let word_value_test name expected_output word =
  name >:: fun _ ->
  assert_equal expected_output (Word.word_value word) ~printer:string_of_int

let word_value_tests =
  [
    word_value_test "testing a random word" 6 (Word.from_string "NICE");
    word_value_test "testing a two letter words" 2 (Word.from_string "AA");
    word_value_test "testing a random word with O" 8 (Word.from_string "POOP");
    word_value_test "testing a random word with K" 8 (Word.from_string "KITE");
    word_value_test "testing a two letter word that is high in points AX (9) " 9
      (Word.from_string "AX");
    word_value_test "testing a two letter word that is high in points EX (9) " 9
      (Word.from_string "EX");
    word_value_test "testing a two letter word that is high in points JO (9) " 9
      (Word.from_string "JO");
    word_value_test "testing a two letter word that is high in points OX (9) " 9
      (Word.from_string "OX");
    word_value_test "testing a two letter word that is high in points QI (9) "
      11 (Word.from_string "QI");
    word_value_test "testing a two letter word that is high in points XI (9) " 9
      (Word.from_string "XI");
    word_value_test "testing a two letter word that is high in points XU (9) " 9
      (Word.from_string "XU");
    word_value_test "testing a three letter word that is high in points " 16
      (Word.from_string "ZEK");
    word_value_test "testing a three letter word that is high in points " 15
      (Word.from_string "FIZ");
    word_value_test "testing a three letter word that is high in points " 15
      (Word.from_string "FEZ");
    word_value_test "testing a three letter word that is high in points " 15
      (Word.from_string "PYX");
    word_value_test "testing a three letter word that is high in points " 15
      (Word.from_string "WIZ");
    word_value_test "testing a four letter word that is high in points " 22
      (Word.from_string "QUIZ");
    word_value_test "testing a five letter word that is high in points " 33
      (Word.from_string "JAZZY");
    word_value_test "testing a word that is high in points " 41
      (Word.from_string "Oxyphenbutazone");
  ]

let temp_hand4 = temp_hand2 @ [ Tile.create_tile 'm' ]
let temp_hand5 = temp_hand4 @ [ Tile.create_tile 'p'; Tile.create_tile 'a' ]
let p1 = Player.new_player "Sean"
let p2 = Player.set_letters p1 temp_hand1
let p3 = Player.set_letters p2 temp_hand2
let p4 = Player.set_letters p2 temp_hand3
let p5 = Player.remove_letters temp_hand2 p4
let bag2 = Bag.create_bag ()
let rand_hand = fst (Player.fill_hand temp_hand1 bag2)
let p6 = Player.set_letters p1 rand_hand
let half_hand_fill = fst (Player.fill_hand temp_hand5 bag2)
let p10 = Player.set_letters p1 half_hand_fill

let player_letters_test name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (Player.get_hand player) ~printer:Fun.id

let player_letters_length_test name (expected_output : int) (player : Player.t)
    =
  name >:: fun _ ->
  assert_equal expected_output
    (String.length (Player.get_hand player))
    ~printer:string_of_int

let player_letters_invalid_test name output_char hand player =
  name >:: fun _ ->
  assert_raises (InvalidChar output_char) (fun () ->
      Player.remove_letters hand player)

let player_tile_list_test name expected_output player =
  name >:: fun _ -> assert_equal expected_output (Player.get_letters player)

let player_letters_tests =
  [
    player_letters_test "A new player has an empty hand" "" p1;
    player_letters_test "Adding a tile to a player returns the same hand" "C" p2;
    player_letters_test
      "Adding a list of tiles D, L to a hand returns the same hand" "DL" p3;
    player_letters_test
      "Adding a list L,C,E,P,D,Q,U to a player returns the same hand" "LCEPDQU"
      p4;
    player_letters_test "Removing D,L from the hand L,C,E,P,D,Q,U returns CEPQU"
      "CEPQU" p5;
    player_letters_length_test
      "Filling a random hand should make the length of the hand 7" 7 p6;
    player_letters_length_test
      "Filling a hand with 5 characters should add 2 tiles to make the lenght 7"
      7 p10;
    player_letters_invalid_test
      "Trying to remove M from hand LCEPDQU is invalid" 'M' temp_hand4 p4;
    player_letters_invalid_test
      "Trying to remove anything from new player is invalid" 'D' temp_hand2 p1;
    player_tile_list_test
      "Getting the hand list from p4 is the same as the list of tiles \
       temp_hand3"
      temp_hand3 p4;
  ]

let p7 = Player.add_score 10 p1
let p8 = Player.add_score (-10) p1
let p9 = Player.add_score 1030 p7

let player_score_test name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (Player.get_score player) ~printer:string_of_int

let player_score_tests =
  [
    player_score_test "The score of a new player is 0" 0 p1;
    player_score_test "The score after adding 10 is 10" 10 p7;
    player_score_test
      "The score after adding -10 is -10, this shouldn't be possible but it \
       will never happen"
      (-10) p8;
    player_score_test "Adding 1030 to a score of 10 makes it 1040" 1040 p9;
  ]

let player_name_test name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (Player.get_name player) ~printer:Fun.id

let player_name_tests =
  [
    player_name_test "Name of p1 is Sean" "Sean" p1;
    player_name_test "Name of p2 is Sean since the name hasn't changed" "Sean"
      p2;
    player_name_test "Name of p6 is Sean since the name hasn't changed" "Sean"
      p6;
    player_name_test "Name of p10 is Sean since the name hasn't changed" "Sean"
      p10;
  ]

let name_lst1 = [ "John"; "Bobby"; "Jimmy" ]
let name_lst2 = [ "Joe"; "Sarah" ]
let name_lst3 = [ "Amy"; "Johan"; "Rohan"; "James"; "Arn" ]
let st1 = State.new_state name_lst1
let st2 = State.new_state name_lst2
let st3 = State.copy_state st1 st2
let st4 = State.new_state name_lst3
let player_list1 = State.get_players st1
let st1_next1 = snd (State.next_turn st2 st1)
let st1_next2 = snd (State.next_turn st2 st1_next1)
let st1_next3 = snd (State.next_turn st2 st1_next2)
let st1_next4 = snd (State.next_turn st2 st1_next3)

let rec player_list_getter x (lst : Player.t list) =
  match lst with
  | [] -> failwith "Not possible"
  | [ h ] -> h
  | h :: t -> if x = 0 then h else player_list_getter (x - 1) t

let state_player_names_test name expected_output state =
  name >:: fun _ ->
  assert_equal expected_output (State.get_names (State.get_players state))

let state_copy_test name st1 st2 =
  name >:: fun _ -> assert_equal st1 (State.copy_state st1 st2)

let state_player_size_test name expected_output state =
  name >:: fun _ ->
  assert_equal expected_output (State.player_size state) ~printer:string_of_int

let state_player_turn_test name expected_output state =
  name >:: fun _ ->
  assert_equal expected_output (State.player_turn state)
    ~printer:Player.get_name

let state_player_tests =
  [
    state_player_names_test
      "A new state with players John, Bobby, Jimmy has a list of names of the \
       same"
      name_lst1 st1;
    state_player_names_test
      "A new state with players Joe, Sarah has a list of names of the same"
      name_lst2 st2;
    state_player_names_test
      "A copied state from st1 to st2 is the same list of names from st1"
      name_lst1 st3;
    state_player_names_test
      "A new state with the 5 names has the same name list" name_lst3 st4;
    state_copy_test "Copying st1 onto st2 results in the same state" st1 st2;
    state_player_size_test "Player size of st1 is 3" 3 st1;
    state_player_size_test "Player size of st2 is 2" 2 st2;
    state_player_size_test
      "Player size of st4 is 5, shouldn't be possible but that case is handled \
       in the game loop"
      5 st4;
    state_player_turn_test "The turn should be the first player in the list"
      (List.hd player_list1) st1;
    state_player_turn_test
      "The next turn of a state should be the second player of the list"
      (player_list_getter 1 player_list1)
      st1_next1;
    state_player_turn_test
      "The third turn of a state should be the third player of the list"
      (player_list_getter 2 player_list1)
      st1_next2;
    state_player_turn_test
      "The fourth turn of a state should be the first player of the list"
      (player_list_getter 0 player_list1)
      st1_next3;
    state_player_turn_test
      "The fifth turn of a state should be the second player of the list"
      (player_list_getter 1 player_list1)
      st1_next4;
  ]

let suite =
  "test suite for Scrabble"
  >::: List.flatten
         [
           create_tile_tests;
           board_tests;
           bag_empty_tests;
           bag_tests;
           check_verb_tests;
           get_letter_tests;
           get_point_char_tests;
           get_points_tests;
           check_valid_tests;
           word_value_tests;
           player_letters_tests;
           player_score_tests;
           player_name_tests;
           state_player_tests;
         ]

let _ = run_test_tt_main suite