(* A value represents a fixed-width bitvector*)
type t [@@deriving sexp, compare]

val make : int * int -> t
(** [make (i,sz)] constructs a bitvector wiht value [i] of size [sz]. **
    Throws an exception when [i > 2^sz -1] or when [i < 0]. ***)

val big_make : Bigint.t * int -> t
(** [big_make] is the same as [make] mutatis mutandi.*)

val str_make : string * int -> t
(** [str_make (s,sz)] converts [s], which is a hex, bin, or dec string, **
    into a bitvector if size [sz] ***)

val unsafe_make : int * int -> t
(** [unsafe_make] forgoes all bounds checking -- used for testing *)

val to_string : t -> string
(** [to_string v] converts a value into a string*)

val to_bmv2_string : t -> string
(** [to_bmv2_string v] converts [v] into a bmv2-parseable string*)

val to_sexp_string : t -> string
(** [to_sexp_string v] converts [v] into an sexp string. *)

val to_smt : t -> Z3.Smtlib.term
(** [to_smt v] is the Z3 term corresponding to [v] *)

val eq : t -> t -> bool
(** [eq v v'] is true when [v] and [v] are the same bitvector. It throws an
    ** exception if [v] and [v'] have different sizes ***)

val equals : t -> t -> bool
(** [equals v v'] is true when [v] and [v] are the same bitvector and when
    they ** have the same size. Does not throw an exception when [v] and [v']
    are ** differently sized, it simply returns false ***)

val big_eq : t -> Bigint.t -> bool
(** [big_eq v i] ignores sizing information when checking that [v] is
    equivalent to [i] *)

val neq : t -> t -> bool
(** [neq v v'] is true when [eq v v'] is false, and throws exceptions in the
    ** same situations. *)

val leq : t -> t -> bool
(** [leq v v'] is true when [v] is less than or equal to [v] and throws an **
    exception when they have different sizes *)

val zero : int -> t
(** [zero sz] is equivalent to [make (0,sz)] *)

val get_bigint : t -> Bigint.t
(** [get_bigint v] is a Bigint.t representation of the bitvector *)

val get_int_exn : t -> int
(** [get_bigint v] is a int representation of the bitvector, throwing an **
    exception when the underlying bitvector is too large *)

val size : t -> int
(** [size v] gets the size of the bitvector*)

val same_size : t -> t -> bool
(** [same_size v v'] is true when [v] and [v'] have the same size*)

val add : t -> t -> t
(** [add v v'] computes the sum of [v] and [v'] using wraparound semantics,
    failing when the sizes differ*)

val sat_add : t -> t -> t
(** [sat_add v v'] computes the sum of [v] and [v'] using saturating
    semantics, failing when the sizes differ*)

val multiply : t -> t -> t
(** [multiply v v'] computes the product of [v] and [v'] using wraparound
    semantics, failing when the sizes differ*)

val subtract : t -> t -> t
(** [subtract v v'] computes the difference between [v] and [v'] using
    wraparound semantics, failing when the sizes differ*)

val sat_subtract : t -> t -> t
(** [sat_subtract v v'] computes the difference between of [v] and [v'] using
    saturating semantics, failing when the sizes differ*)

val mask : t -> t -> t
(** [mask v m] computes [v & m], failing when the sizes differ*)

val xor : t -> t -> t
(** [xor v v'] computes [v ^ m], failing when the sizes differ *)

val or_ : t -> t -> t
(** [or_ v v'] computes [v | v'], failing when the sizes differ *)

val shl : t -> t -> t
(** [shl v'] computes [v << m], failing when the sizes differ *)

val cast : int -> t -> t
(** [cast sz v] converts v into a bitvector of size [sz], padding with zeros
    if [sz > size v] and truncating otherwise *)

val resize : t -> int -> t
(** [resize sz v] is an unsafe operation that modifies with size without
    ensuring that the bitvector actually can have that size. See test case
    [ValueTest.resize_is_dangerous] for an example *)

val slice : int -> int -> t -> t
(** [slice hi lo v] returns a value [v\[hi:lo\]] of size [hi - lo] *)

val concat : t -> t -> t
(** [concat v v'] constructs a vralue of size [size v + size v'] that
    contains ** all the bits of [v] followed by all the bits of [v']*)

val random : ?lo:int -> int -> t
(** [random lo sz] generates a random [Value.t] of size [sz] greater than lo,
    throws an exception if [sz <= 0] or [lo >= 2^sz] *)
