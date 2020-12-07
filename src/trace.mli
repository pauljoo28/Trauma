open Collection

type trace

val empty : trace
val make : collection list -> trace
val version : int -> trace -> collection
val to_list : trace -> collection list