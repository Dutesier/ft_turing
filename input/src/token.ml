(** The main token type *)
type t =
  | LParen
  | RParen
  | LSquareBracket
  | RSquareBracket
  | Comma
  | Collon
  | Word of word

and word =
  | Name
  | Alphabet
  | Blank
  | States
  | Initial
  | Finals
  | Transitions
  | Read
  | ToState
  | Write
  | Action
  | Other of string

let lookup_words w =
  match w with
  | "name" -> Word Name
  | "alphabet" -> Word Alphabet
  | "blank" -> Word Blank
  | "states" -> Word States
  | "initial" -> Word Initial
  | "finals" -> Word Finals
  | "transitions" -> Word Transitions
  | "read" -> Word Read
  | "to_state" -> Word ToState
  | "write" -> Word Write
  | "action" -> Word Action
  | _ -> Word (Other w)
;;

let print_token = function
  | None -> Printf.sprintf "None"
  | Some tok ->
    (match tok with
     | LParen -> Printf.sprintf "{"
     | RParen -> Printf.sprintf "}"
     | LSquareBracket -> Printf.sprintf "["
     | RSquareBracket -> Printf.sprintf "]"
     | Comma -> Printf.sprintf ","
     | Collon -> Printf.sprintf ":"
     | Word w ->
       (match w with
        | Other s -> Printf.sprintf "\"%s\"" s
        | Name -> Printf.sprintf "Name"
        | Alphabet -> Printf.sprintf "Alphabet"
        | Blank -> Printf.sprintf "Blank"
        | States -> Printf.sprintf "States"
        | Initial -> Printf.sprintf "Initial"
        | Finals -> Printf.sprintf "Finals"
        | Transitions -> Printf.sprintf "Transitions"
        | Read -> Printf.sprintf "Read"
        | ToState -> Printf.sprintf "ToState"
        | Write -> Printf.sprintf "Write"
        | Action -> Printf.sprintf "Action"))
;;
