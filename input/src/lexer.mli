(** The public interface of the Lexer *)

(** Our main type *)
type t

(** [init string] creates a basic lexer [t] from an input string. *)
val init : string -> t

(** [next_token] returns a new lexer at the next token. It might also
    return a token if such exists. *)
val next_token : t -> t * Token.t option