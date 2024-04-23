open Core

type t =
  { filepath : string
  ; contents : string option
  }

let init filepath =
  let read_file file = In_channel.read_lines file in
  { filepath
  ; contents =
      (match read_file filepath with
       | [] -> None
       | lst ->
         let rec join_str = function
           | [] -> ""
           | h :: t -> h ^ join_str t
         in
         Some (join_str lst))
  }
;;

let contents = function
  | { filepath = _; contents = Some x } -> Some x
  | _ -> None
;;
