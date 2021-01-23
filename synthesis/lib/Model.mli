open Util

(** A Model is partial valuation of identifiers (strings) *)
type t

val empty : t
(** [empty] is the empty model in which no strings have valuations*)

val set : t -> key:string -> data:Value.t -> t
(** [set m ~key ~data] updates the model with the valuation [key |-> data] *)

val find : t -> string -> Value.t option
(** [find m k] optionally returns the value for [k] indicated by [m], if such
    a value exists. *)

val to_string : t -> string
(** [to_string m] converts [m] into a string representation *)

val equal : t -> t -> bool
(** [equal m1 m2] returns true when [m1] and [m2] represent the same partial
    valuation *)

val of_smt_model : (Z3.Smtlib.identifier * Z3.Smtlib.term) list -> t
(** [of_smt_model lst] constructs a Model from a Z3 association list [lst] *)

val extend_multi_model :
  Value.t list StringMap.t -> t -> Value.t list StringMap.t
(** [extend_multi_model multi m] adds the model [m] to the multimap [multi] *)

val join : t -> t -> t
(** [join m1 m2] computes the disjoint union of m1 & m2. Key overlaps are *
    allowed when the values are identical. An exception is thrown when two
    keys * exist with different values *)

val aggregate : t list -> t
(** [aggregate ms] combines a list of models as in [join] *)

val right_union : t -> t -> t
(** [right_union ml mr] is the disjoint union of [ml ∖ mr] and [mr]*)

val intersect : t -> t -> t
(** [intersect m1 m2] is the intersection of [m1] and [m2], i.e. the
    key-value pair [x ↦ v] is in [intersect m1 m2] iff it is in both [m1]
    and [m2] *)

val perturb : t -> t
(** [perturb m] randomly generates new values for variables in [m]. if
    [x ↦ v] is in [m], then [x ↦ v'] in [perturb m] where
    [Value.veq v v'] is [false] and doesn't throw an exception. *)

val proj_packet_holes : t -> t
(** [proj_packet_holes m] projects a model down to holes that correspond to
    keys & actiondata in tables *)

val fold : t -> init:'a -> f:(key:string -> data:Value.t -> 'a -> 'a) -> 'a
(** [fold m1 ~init ~f] is a specialization of [Core.Map.fold] *)

val iteri : t -> f:(key:string -> data:Value.t -> unit) -> unit
(** [iteri m1 ~init ~f] is a specialization of [Core.Map.iteri] *)

val diff : t -> t -> (Value.t * Value.t) StringMap.t
(** [diff m1 m2] Constructs a mapping from strings to pairs of values such
    that * mapping [k |-> (v1,v2)] occurs in [diff m1 m2] iff * [k |-> v1] in
    [m1] and [k |-> v2] in [m2] *)

val of_alist_exn : (string * Value.t) list -> t
(** [of_alist_exn assoc] constructs a valuation from an association list *
    i.e. [(k,v)] is an element of [assoc[] iff [k |-> v] in [of_alist_exn
    assoc] *)

val to_strmap : t -> Value.t StringMap.t

val of_strmap : Value.t StringMap.t -> t
