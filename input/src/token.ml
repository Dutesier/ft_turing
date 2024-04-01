(** The main token type *)
type t = 
  | LParen
  | RParen
  | LSquareBracket
  | RSquareBracket
  | Comma
  | Collon
  | Word of string
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

let lookup_words w = match w with
  | "name" -> Name
  | "alphabet" -> Alphabet
  | "blank" -> Blank
  | "states" -> States
  | "initial" -> Initial
  | "finals" -> Finals
  | "transitions" -> Transitions
  | "read" -> Read
  | "to_state" -> ToState
  | "write" -> Write
  | "action" -> Action
  | _ -> Word w


let print_token = function
  | None -> Printf.sprintf "None"
  | Some tok -> 
    match tok with 
    | LParen -> Printf.sprintf "{"
    | RParen -> Printf.sprintf "}"
    | LSquareBracket -> Printf.sprintf "["
    | RSquareBracket -> Printf.sprintf "]"
    | Comma -> Printf.sprintf ","
    | Collon -> Printf.sprintf ":"
    | Word s -> Printf.sprintf "Word: |%s|" s
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
    | Action -> Printf.sprintf "Action"