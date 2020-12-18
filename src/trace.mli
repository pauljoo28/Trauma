open Collection

type 'a trace

(** An empty collection *)
val init : 'a collection -> 'a trace

(** Make base into collection *)
val to_collection : 'a trace -> 'a collection

(** Swap a collection in trace *)
val swap : int list -> 'a collection -> 'a trace -> 'a trace

(** [add_diff t1 t2] adds the diff [t1] to the trace [t2] *)
val add_diff : int list -> 'a collection -> 'a trace -> 'a trace

(** [add_dim t] creates a new trace from [t] with an extra dimension *)
val add_dim : 'a trace -> 'a trace

(** [get_version k t] creates the version of [t] at [k] *)
val get_diff_version : int list -> 'a trace -> 'a trace

(** [get_version k t] creates the version of [t] at [k] *)
val get_version : int list -> 'a trace -> 'a trace

(** [distince tr] creates the output trace *)
val distinct : 'a trace -> 'a trace

(** [get_dim tr] gets the dimesions of current trace *)
val get_dim : 'a trace -> int

(** Debug *)
val debug_get_dim : 'a trace -> int
val debug_iter_tostring : 'a trace -> string
val debug_empty_output : 'a trace -> 'a trace