type t =
  { lexer : Lexer.t
  ; current_tok : Token.t option
  ; peek_tok : Token.t option
  }

let print_parser p =
  "\n------------------\n"
  ^ "[Parser]: {\n\tCurrent -> "
  ^ Token.print_token p.current_tok
  ^ "\n\tPeek -> "
  ^ Token.print_token p.peek_tok
  ^ "\n}\n------------------\n"
;;

(* The token we had peeked is now the current and
   we peek in the lexer to check what the next one is.*)
let advance parser =
  let lexer, peek_tok = Lexer.next_token parser.lexer in
  { lexer; current_tok = parser.peek_tok; peek_tok }
;;

let init lexer =
  let parser = { lexer; current_tok = None; peek_tok = None } in
  let parser = advance parser in
  let parser = advance parser in
  parser
;;

(** EXPLORE ERROR HANDLING*)

(** [expect_and_advance expectation parser] expects that [expectation]
    is true when applied to the [parser]s current token.
    If so it advances and returns the parser, else it returns None.*)
(* let expect_and_advance expectation parser =
   match parser.current_tok with
   | None -> None
   | Some t when expectation t -> Some (advance parser)
   | _ -> None *)

(** Common expectations *)
let expect_token_equal expectation actual =
  if expectation = actual
  then true
  else (
    print_endline
      (Printf.sprintf
         "Expected: %s Actual: %s"
         (Token.print_token (Some expectation))
         (Token.print_token (Some actual)));
    false)
;;

let expect_then_advance expectation parser =
  match parser.current_tok with
  | Some t when expectation t -> advance parser
  | _ -> failwith ("Unmet expectation: " ^ print_parser parser)
;;

(* let expect_peek expectation parser =
   match parser.peek_tok with
   | Some t when expectation t -> parser
   | _ -> failwith ("Unmet expectation: " ^ print_parser parser) *)

let is_valid_object_start = function
  | None -> false
  | Some t ->
    (match t with
     | Token.Word _ -> true
     | Token.LParen -> true
     | Token.LSquareBracket -> true
     | _ -> false)
;;

let rec parse_json_array parser =
  let parser =
    print_endline ("parse json array:" ^ print_parser parser);
    parser
  in
  let parser = expect_then_advance (expect_token_equal Token.LSquareBracket) parser in
  let rec parse_array_aux parser objects =
    let parser =
      print_endline ("parse json array AUX:" ^ print_parser parser);
      parser
    in
    match parser.current_tok with
    | Some Token.RSquareBracket -> parser, List.rev objects
    | Some t when is_valid_object_start (Some t) ->
      (* Should be the beginning of a new object*)
      let parser, obj = parse_json_object parser in
      parse_array_aux parser (obj :: objects)
    | Some t when t == Token.Comma ->
      let parser = expect_then_advance (expect_token_equal Token.Comma) parser in
      let parser, obj = parse_json_object parser in
      parse_array_aux parser (obj :: objects)
    | Some x -> failwith ("Not implemented: " ^ Token.print_token (Some x))
    | None -> failwith "Invalid JSON: Array never ended"
  in
  let parser, objs = parse_array_aux parser [] in
  let parser = expect_then_advance (expect_token_equal Token.RSquareBracket) parser in
  parser, Ast.Array objs

(** Parses a JSON object of type "name" : something , where something might be either a string, an array or another object*)
and parse_named_object parser =
  let parser =
    print_endline ("parse named object:" ^ print_parser parser);
    parser
  in
  let key = parser.current_tok in
  let key =
    match key with
    | None -> failwith "Expect key not None"
    | Some t -> t
  in
  let parser = advance parser in
  (* Process the colon*)
  let parser = advance parser in
  match parser.current_tok with
  | None -> failwith "Invalid Json: Ends in :"
  | Some t ->
    (match t with
     | Token.Word s -> parser, Ast.Named { key; value = Ast.Unnamed (Token.Word s) }
     | Token.LParen ->
       let parser, obj = parse_json_object parser in
       parser, Ast.Named { key; value = obj }
     | Token.LSquareBracket ->
       let parser, arr = parse_json_array parser in
       parser, Ast.Named { key; value = arr }
     | _ ->
       failwith "Invalid Json: either an array, a string or a new object must follow a :")

(** [parse_json_object parser] gives the next json object inside the [parser]
    and returns a new parser at the end of said object. An object can be
    either a named kv, an unnamed value, an array of values*)
and parse_json_object parser =
  (* let parser = advance parser in *)
  let parser =
    print_endline ("parse json object:" ^ print_parser parser);
    parser
  in
  match parser.current_tok with
  | None -> failwith "Missing closing bracket: }"
  | Some t ->
    (match t with
     | Token.LParen ->
       let parser, nested = parse_json_object (advance parser) in
       parser, Ast.Nested nested
     | Word _ when parser.peek_tok = Some Token.Collon ->
       let parser, objects = parse_named_object parser in
       let parser = advance parser in
       parser, objects
     | Word w ->
       let parser = advance parser in
       parser, Ast.Unnamed (Token.Word w)
     | t -> failwith ("TODO: handle " ^ Token.print_token (Some t)))
;;

let parse parser =
  let parser =
    print_endline "Parsing";
    parser
  in
  let parser = expect_then_advance (expect_token_equal Token.LParen) parser in
  let rec parse_aux parser objects =
    match parser.current_tok with
    | None -> parser, List.rev objects
    | Some _ ->
      (* Should be the beginning of a new object*)
      let parser, obj = parse_json_object parser in
      parse_aux (advance parser) (obj :: objects)
    (* | _ -> failwith ("unimplemented: " ^ (Ast.print_json_object_list objects) ^ "Parser: " ^ print_parser parser) *)
  in
  let _, objs = parse_aux parser [] in
  Ast.Main objs
;;
