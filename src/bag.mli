type t
(** [t] represents a list whose elements are of type char. *)

exception Empty of string
(** Raised when drawing or removing a tile from an empty bag. *)

val create_bag : unit -> t
(** [create_bag] is type unit that initilizes the bag with the correct number
    tiles in a given scrabble game. *)

val create_bag_no_blanks : unit -> t
(** [create_bag] is type unit that initilizes the bag with the correct number
    tiles in a given scrabble game but with no blanks. *)

val draw_tile : t -> Tile.t * t
(** [draw_tile bag] draws a random tile from the bag, and returns a tuple of the
    tile and the updated bag. *)

val empty : unit -> t
(** [empty] is the bag containing no tiles *)

val size_bag : t -> int
(** [size_bag bag] returns the the size of the bag *)

val remove : char -> t -> t
(** [remove c bag] removes the character, c, from the bag. *)

val print_bag : t -> string
(** [print_bag] prints all the tiles in the bag so far *)

val is_empty : t -> bool
(** [is_empty bag] checks whether the bag is empty; true if bag is empty, false
    otherwise. *)

val to_char : t -> char list -> char list
(** [to_char bag] converts the type t to a char list *)
