type 'a collection

val empty : 'a collection
val get : 'a -> 'a collection -> int
val insert : ('a * int) -> 'a collection -> 'a collection
val add : 'a collection -> 'a collection -> 'a collection