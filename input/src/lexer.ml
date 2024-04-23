(** Our implementation of a Lexer *)

type t =
  { input : string
  ; position : int
  ; ch : char option
  }

let init = function
  | s when String.length s = 0 -> { input = ""; position = 0; ch = None }
  | s -> { input = s; position = 0; ch = Some (String.get s 0) }
;;

let advance = function
  | { input; position = p; ch } when ch <> None ->
    { input
    ; position = p + 1
    ; ch = (if p + 1 < String.length input then Some (String.get input (p + 1)) else None)
    }
  | { input; position; ch } -> { input; position; ch }
;;

(** Seek gives the position of the next time the condition
    evaluates to true on the lexer.*)
let seek lexer condition =
  let rec loop lexer = if condition lexer.ch then loop @@ advance lexer else lexer in
  let lexer = loop lexer in
  lexer, lexer.position
;;

let is_whitespace c = c = ' ' || c = '\t' || c = '\n'
let isnt_closing_quote c = c <> '"'

let skip_whitespace lexer =
  let lex, _ =
    seek lexer (fun ch ->
      match ch with
      | None -> false
      | Some ch -> is_whitespace ch)
  in
  lex
;;

let read_word lexer =
  let lexer = advance lexer in
  (*Advance from the first quote*)
  let str_start = lexer.position in
  let lexer, str_end =
    seek lexer (fun ch ->
      match ch with
      | Some c -> isnt_closing_quote c
      | None -> false)
  in
  (* Now lexer is poiting at end quote*)
  advance lexer, String.sub lexer.input str_start (str_end - str_start)
;;

let next_token (lexer : t) =
  let open Token in
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> lexer, None
  | Some ch ->
    let lexer, token =
      match ch with
      | '"' ->
        let lex, tok = read_word lexer in
        lex, lookup_words tok
      | '{' -> advance lexer, LParen
      | '}' -> advance lexer, RParen
      | '[' -> advance lexer, LSquareBracket
      | ']' -> advance lexer, RSquareBracket
      | ',' -> advance lexer, Comma
      | ':' -> advance lexer, Collon
      | ' ' -> failwith "Unparsed whitespace"
      | ch -> failwith (Printf.sprintf "Unquoted string argument at %c" ch)
    in
    lexer, Some token
;;
