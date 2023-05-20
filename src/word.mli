type t

val from_string : string -> t
(**[ from_string s] returns back s*)

val check_valid : string list -> t -> bool
(**[ check_valid s wrd] checks whether or not a wrd is in the list set*)

val word_value : t -> int
(**[ word_value wrd] is a word that is worth some amount of points based off
   each of its character's value *)

val random_char : unit -> char
(**[ random_char] is a method that returns a random letter in the alphabet.*)
