type collection

val empty : collection
val get : int -> collection -> int
val insert : collection -> collection -> collection
val make : (int * int) list -> collection
val to_list : collection -> (int * int) list