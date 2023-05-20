type object_phrase = string list

type command =
  | Quit
  | Help
  | Place_Tile of object_phrase
  | Clear
  | Endturn
  | Reset

exception Empty
exception Malform

let is_int x =
  try
    ignore (int_of_string x);
    true
  with _ -> false

let check_verb comm =
  match comm with
  | [] -> raise Empty
  | [ h ] ->
      if h = "quit" then Quit
      else if h = "clear" then Clear
      else if h = "end" then Endturn
      else if h = "help" then Help
      else if h = "endturn" then Endturn
      else if h = "reset" then Reset
      else raise Malform
  | [ x; y; t ] -> begin
      try
        let _ = String.get x 0 in
        if is_int x then raise Malform
        else
          let _ = int_of_string y in
          Place_Tile [ x; y; t ]
      with _ -> raise Malform
    end
  | _ -> raise Malform

let parse str =
  String.split_on_char ' ' str |> List.filter (fun s -> s <> "") |> check_verb