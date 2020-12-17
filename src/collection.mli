type 'a collection

val empty : 'a collection
val get : 'a -> 'a collection -> int
val insert : ('a * int) -> 'a collection -> 'a collection
val add : 'a collection -> 'a collection -> 'a collection
val subtract : 'a collection -> 'a collection -> 'a collection

val distinct : 'a collection -> 'a collection

val debug_string_collection_tostring : string collection -> string