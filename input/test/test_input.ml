open OUnit2
open Input

(* * [make_i n i s] makes an OUnit test named [n] that expects [s] to evaluate to [i]
let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_int i) ((*interpret*) s)) *)
   

let make_t n t s =
  n >:: (fun _ ->  assert_equal (t) (s) ~printer:Token.print_token)

let token_from_lextoken (_, a) = a

let rec get_next_n_token lex n =
  let n = n - 1 in
  let lexer, tok = Lexer.next_token lex in
  match tok with
    | None -> None
    | _ when n > 0 -> get_next_n_token lexer n
    | _ -> tok

(** PARSER helper*)
let parser_of_string s =
  let lexer = Lexer.init s in
  let parser = Parser.init lexer in parser

let make_test_parser name outcome inputs =
  name >:: (fun _ ->
    let parser = parser_of_string inputs in
    let tree = Parser.parse parser in
    assert_equal outcome tree ~printer:Ast.print_node
    )



let tests = [
  (* make_i "int" 22 "22"; *)
  make_t "Basic lexing" (Some (Token.Word "word")) (token_from_lextoken (Lexer.init "\"word\"" |> Lexer.next_token));
  make_t "Check no token" (None) (token_from_lextoken (Lexer.init "" |> Lexer.next_token));
  make_t "Check L Paren" (Some (Token.LParen)) (token_from_lextoken (Lexer.init "{" |> Lexer.next_token));
  make_t "Check Multiple tokens" (Some Token.RParen) (get_next_n_token (Lexer.init "{ }") 2);
  make_t "Check Mult tokens with word" (Some (Token.Word "here")) (get_next_n_token (Lexer.init "{ \"here\"") 2);
  make_t "Check Mult tokens with keyword" (Some (Token.ToState )) (get_next_n_token (Lexer.init "{ \"here\" : \"to_state\" }") 4);
  make_t "Check token exhaustion none" (None) (get_next_n_token (Lexer.init "{ \"here\" : \"to_state\" }") 15);
  (* Parser Tests *)
  make_test_parser "Simple object" (Ast.Main(
    [
      Named{
        key = Token.Word "TO";
        value = Unnamed(Word "DO")
      }
    ]
    ))
    "{\"TO\": \"DO\"}";
  make_test_parser "Array" (Ast.Main(
    [
      Named{
        key = Token.Word "TO";
        value = Array([Unnamed(Word "DO")])
      }
    ]
    ))
    "{\"TO\": [\"DO\"]}";
  make_test_parser "Array with mult elements" (Ast.Main(
    [
      Named{
        key = Token.Word "TO";
        value = Array([Unnamed(Word "DO"); Named{
          key = Token.Word "TO";
          value = Array([Unnamed(Word "DO")])
        }])
      }
    ]
    ))
    "{\"TO\": [\"DO\", {\"TO\": [\"DO\"]}]}";
]

let _ = run_test_tt_main ("suite" >::: tests)
