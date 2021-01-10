type t

val make : (int * int) -> t
val big_make : (Bigint.t * int) -> t
val str_make : (string * int) -> t
(** [unsafe_make] forgoes all bounds checking -- used for testing *)
val unsafe_make : (int * int) -> t
val to_string : t -> string
val to_bmv2_string : t -> string
val to_sexp_string : t -> string
val to_smt : t -> Z3.Smtlib.term
val ueq : t -> Bigint.t -> bool
val eq : t -> t -> bool
val neq : t -> t -> bool
val leq : t -> t -> bool

val zero : int -> t

val get_bigint : t -> Bigint.t
val get_int_exn : t -> int
val size : t -> int
val same_size : t -> t -> bool

val add : t -> t -> t
val sat_add : t -> t -> t
val multiply : t -> t -> t
val subtract : t -> t -> t
val sat_subtract : t -> t -> t
val mask : t -> t -> t
val xor : t -> t -> t
val or_ : t -> t -> t
val shl : t -> t -> t
val cast : int -> t -> t
val resize : t -> int -> t

(** [slice hi lo v] returns a value [v[hi:lo]] of size [hi - lo] *)
val slice : int -> int -> t -> t

val concat : t -> t -> t
