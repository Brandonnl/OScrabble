type t
(** [t] represents the tiles on the board where the tiles could represent
    triple, double, letter score or triple, double word score. *)

exception NoTile
(**Raised when there is no tile placed at that slot*)

val make_empty : int -> t
(** [make_empty int1] creates empty square board*)

val make : unit -> t
(** [make ()] creates real scrabble board *)

val used_square : t -> int -> int -> bool
(** [used_squre board x y] returns true if tile is placed there, false otherwise *)

val get_tile : t -> int -> int -> Tile.t
(** [get_tile board x y] gets tile at certain point*)

val get_board : t -> t
(** [get_board board] returns the board*)

val copy_board : t -> t
(** [copy_board board] creates a copy of the board*)

val set_board : t -> t -> unit
(** [set_board board1 board2] sets board2 to be the same as board1*)

val to_string : t -> string
(** [to_string board] returns a string to print the boad in the terminal*)

val place_tile : int -> int -> Tile.t -> t -> t
(** [place_tile x y t board] places the tile t onto board at location (x, y)*)

val get_special_sq : int -> int -> string
(** [get_special_sq x y returns a string that tells if a square has a multiplier]*)
