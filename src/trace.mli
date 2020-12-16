open Collection

type 'a trace

val empty : 'a trace
val populate : collection list -> 'a trace -> unit
val state_at : int list -> 'a trace -> collection