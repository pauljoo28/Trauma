open Collection

type 'a trace

(** An empty collection *)
val empty : 'a trace

(** [add_diff t1 t2] adds the diff [t1] to the trace [t2] *)
val add_diff : 'a collection -> 'a trace -> 'a trace

(** [add_dim t] creates a new trace from [t] with an extra dimension *)
val add_dim : 'a trace -> 'a trace

(** [get_version k t] creates the version of [t] at [k] *)
val get_diff_version : int list -> 'a trace -> 'a trace

(** [get_version k t] creates the version of [t] at [k] *)
val get_version : int list -> 'a trace -> 'a trace