(** Our Abstract Syntax Tree *)
type node = Main of json_object list

and json_object =
  | Named of
      { key : Token.t
      ; value : json_object
      }
  | Unnamed of Token.t
  | Array of json_object list
  | Nested of json_object

let rec print_list = function
  | [] -> ""
  | e :: l -> print_json_object e ^ "," ^ print_list l

and print_json_object = function
  | Named { key; value } ->
    "(named)" ^ Token.print_token (Some key) ^ ":" ^ print_json_object value
  | Unnamed t -> "(unnamed)" ^ Token.print_token (Some t)
  | Array t -> "(array)" ^ "[" ^ print_list t ^ "]"
  | Nested j -> "(nested)" ^ print_json_object j
;;

let print_json_object_list lst =
  let str = "(main)" ^ "{" in
  let str = ref str in
  List.iter (fun i -> str := !str ^ print_json_object i) lst;
  !str ^ "}"
;;

let print_node = function
  | Main lst -> print_json_object_list lst
;;
