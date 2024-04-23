(** Public interface for our file handler *)
type t

val init : string -> t
val contents : t -> string option
