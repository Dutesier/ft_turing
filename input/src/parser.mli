(** Public interface for our parser *)
type t

val init : Lexer.t -> t
val parse : t -> Ast.node
