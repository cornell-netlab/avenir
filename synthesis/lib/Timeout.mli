type t

val start : float option -> t

val restart : t -> t

val timed_out : t -> bool

val union : t -> t -> t
