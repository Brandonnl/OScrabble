type t
(**The type t is information about a game state: players, turn number, board,
   and bag*)

(** The type representing the result of an attempted action. *)
type result =
  | Legal of (t * t)
  | Illegal of string

val get_names : Player.t list -> string list
(** [get_names Player.t list -> string list] returns a list of names of the
    players in a list *)

val get_players : t -> Player.t list
(** [get_players t -> Player.t list] returns the list of players in state t*)

val player_size : t -> int
(** [player_size t -> int] returns the number of players in a state t *)

val player_turn : t -> Player.t
(** [player_turn t -> Player.t] returns the Player.t who's turn it is *)

val next_turn : t -> t -> t * t
(** [next_turn t -> t -> t * t] returns the old state t with values of the new
    state and the new state with its turn shifted with the turn number shifted
    to the next person *)

val new_state : string list -> t
(** [new_state string list -> t] creates a state t with a list of names and
    turns them into players for the state *)

val copy_state : t -> t -> t
(** [copy_state t -> t -> t] copies the entire state t of first state into the
    second state t *)

val place_tile : t -> t -> char -> int -> Tile.t -> result
(** [place_tile t -> t -> char -> int -> Tile.t -> result] checks if a tile
    placement is valid or not on the board and returns the state if the tile
    placement was legal *)

val get_board : t -> Board.t
(** [get_board t -> Board.t] returns the current board in a state *)

val inc_turn : t -> t
(** [inc_turn t -> t] returns the given state with turn incremented *)

val valid_board : t -> bool
(** [valid_board t -> bool] checks a the board of t and returns true if its in a
    valid state*)
