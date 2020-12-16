type diffs

val empty : diffs
val total : diffs -> int list -> int
val insert : int list -> int -> diffs -> unit
val get_last : diffs -> int list