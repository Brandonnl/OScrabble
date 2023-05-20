type t
(** A tile has a letter w/ a score, is a blank (Hand only) or a used blank
    (board only)*)

val create_tile : char -> t
(** [create_tile c] returns of tile of Letter char*)

val get_point_char : char -> int
(** [get_point_char c] returns the points of how much a tile is worth given a
    letter. *)

val get_points : t -> int
(** [get_points tile] returns points from the tile*)

val get_points_list : t list -> int
(** [get_points tile] returns total points from a tile list*)

val get_letter : t -> char
(** gets the char off the tile*)
