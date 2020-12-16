type collection

val empty : collection
val iter : (string -> int -> unit) -> collection -> unit
val get : string -> collection -> int
val insert : collection -> string -> int -> unit
val add : collection -> collection -> unit