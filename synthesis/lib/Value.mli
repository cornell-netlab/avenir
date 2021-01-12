
(* A value represents a fixed-width bitvector*)
type t

(** [make (i,sz)] constructs a bitvector wiht value [i] of size [sz].
 ** Throws an exception when [i > 2^sz -1] or when [i < 0].
 ***)
val make : (int * int) -> t

(** [big_make] is the same as [make] mutatis mutandi.*)
val big_make : (Bigint.t * int) -> t

(** [str_make (s,sz)] converts [s], which is a hex, bin, or dec string,
 ** into a bitvector if size [sz]
 ***)
val str_make : (string * int) -> t

(** [unsafe_make] forgoes all bounds checking -- used for testing *)
val unsafe_make : (int * int) -> t

(** [to_string v] converts a value into a string*)
val to_string : t -> string

(** [to_bmv2_string v] converts [v] into a bmv2-parseable string*)
val to_bmv2_string : t -> string

(** [to_sexp_string v] converts [v] into an sexp string. *)
val to_sexp_string : t -> string

(** [to_smt v] is the Z3 term corresponding to [v]  *)
val to_smt : t -> Z3.Smtlib.term


(** [eq v v'] is true when [v] and [v] are the same bitvector. It throws an
 ** exception if [v] and [v'] have different sizes
 ***)
val eq : t -> t -> bool


(** [equals v v'] is true when [v] and [v] are the same bitvector and when they
 ** have the same size. Does not throw an exception when [v] and [v'] are
 ** differently sized, it simply returns false
 ***)
val equals : t -> t -> bool

(** [big_eq v i] ignores sizing information when checking that [v] is equivalent
   to [i] *)
val big_eq : t -> Bigint.t -> bool

(** [neq v v'] is true when [eq v v'] is false, and throws exceptions in the
 ** same situations. *)
val neq : t -> t -> bool

(** [leq v v'] is true when [v] is less than or equal to [v] and throws an
 ** exception when they have different sizes *)
val leq : t -> t -> bool

(** [zero sz] is equivalent to [make (0,sz)] *)
val zero : int -> t

(** [get_bigint v] is a Bigint.t representation of the bitvector *)
val get_bigint : t -> Bigint.t

(** [get_bigint v] is a int representation of the bitvector, throwing an
 ** exception when the underlying bitvector is too large  *)
val get_int_exn : t -> int

(** [size v] gets the size of the bitvector*)
val size : t -> int

(** [same_size v v'] is true when [v] and [v'] have the same size*)
val same_size : t -> t -> bool

(** [add v v'] computes the sum of [v] and [v'] using wraparound semantics, failing when the sizes differ*)
val add : t -> t -> t

(** [sat_add v v'] computes the sum of [v] and [v'] using saturating semantics, failing when the sizes differ*)
val sat_add : t -> t -> t

(** [multiply v v'] computes the product of [v] and [v'] using wraparound semantics, failing when the sizes differ*)
val multiply : t -> t -> t

(** [subtract v v'] computes the difference between [v] and [v'] using wraparound semantics, failing when the sizes differ*)
val subtract : t -> t -> t

(** [sat_subtract v v'] computes the difference between of [v] and [v'] using saturating semantics, failing when the sizes differ*)
val sat_subtract : t -> t -> t

(** [mask v m] computes [v & m], failing when the sizes differ*)
val mask : t -> t -> t

(** [xor v v'] computes [v ^ m], failing when the sizes differ *)
val xor : t -> t -> t

(** [or_ v v'] computes [v | v'], failing when the sizes differ *)
val or_ : t -> t -> t

(** [shl v'] computes [v << m], failing when the sizes differ *)
val shl : t -> t -> t

(** [cast sz v] converts v into a bitvector of size [sz], padding with zeros if
   [sz > size v] and truncating otherwise *)
val cast : int -> t -> t

(** [resize sz v] is an unsafe operation that modifies with size without
   ensuring that the bitvector actually can have that size. See test case
   [ValueTest.resize_is_dangerous] for an example *)
val resize : t -> int -> t

(** [slice hi lo v] returns a value [v[hi:lo]] of size [hi - lo] *)
val slice : int -> int -> t -> t

(** [concat v v'] constructs a value of size [size v + size v'] that contains
 ** all the bits of [v] followed by all the bits of [v']*)
val concat : t -> t -> t

(** [random sz] generates a random [Value.t] of size [sz] *)
val random : int -> t
