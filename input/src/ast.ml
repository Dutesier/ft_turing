(** Our Abstract Syntax Tree *)
type node = 
 | Main of json_object list

and json_object = 
  | Named of 
    {
      key: Token.t;
      value: json_object
    }
  | Unnamed of Token.t
  | Array of json_object list
  | Nested of json_object

let rec print_json_object_list lst =
  let str = "{" in
  let str = ref str in
  List.iter (fun i -> str :=  !str ^ print_json_object i ) lst;
  !str ^ "}" 

and print_json_object = function
 | Named {key; value } -> (Token.print_token (Some key)) ^ ":" ^ print_json_object value
 | Unnamed t -> (Token.print_token (Some t))
 | Array[] -> Printf.sprintf("[]")
 | Array(h::t) -> (print_json_object  h) ^ "," ^ print_json_object_list t
 | Nested j -> print_json_object j

let print_node = function
  | Main lst -> print_json_object_list lst