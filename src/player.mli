type t = {
  name : string;
  score : int;
  letters : Tile.t list;
}
(** The abstract type of values representing players. *)

exception InvalidChar of char

val valid_player : int -> bool
(** [valid_player num] checks whether or not the user inputs the correct number
    of players to play the game. True if num is in between 2 and 4, otherwise,
    false. *)

val get_letters : t -> Tile.t list
(** [get_letters t] returns the letters that t currently has. Requires: [t] is a
    valid player type. *)

val set_letters : t -> Tile.t list -> t
(** [set_letters t] sets the letters of a player t given a Tile.t list *)

val new_player : string -> t
(** [new_player n -> t] returns a player type t with 0 points and an empty hand
    of Tiles *)

val get_name : t -> string
(** [get_name t] returns the name that t currently has. Requires: [t] is a valid
    player type. *)

val get_score : t -> int
(** [get_score t] returns the score that t currently has. Requires: [t] is a
    valid player type. *)

val add_score : int -> t -> t
(** [add_score int -> t -> t] returns the letters that t currently has.
    Requires: [t] is a valid player type. *)

val remove_letters : Tile.t list -> t -> t
(** [remove_letters char list -> t -> t] removes used letters from [t].
    Requires: [t] is a valid player type and [char list] is a valid list of
    chars. *)

val fill_hand : Tile.t list -> Bag.t -> Tile.t list * Bag.t
(** [fill_hand Tile.t list -> Tile.t list] takes in a current list of Tiles and
    adds until there are 7 total tiles in the list, returns the full hand *)

val get_hand : t -> string
(** [get_hand t -> string] takes in a player t and returns the hand as a string
    of characters *)
