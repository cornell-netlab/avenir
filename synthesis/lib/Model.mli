open Util

(** A Model is partial valuation of identifiers (strings) *)
type t

(** [empty] is the empty model in which no strings have valuations*)
val empty : t

(** [set m ~key ~data] updates the model with the valuation [key |-> data]  *)
val set : t -> key:string -> data:Value.t -> t

(** [find m k] optionally returns the value for [k] indicated by [m], if such a value exists. *)
val find : t -> string -> Value.t option

(** [to_string m] converts [m] into a string representation *)
val to_string : t -> string

(** [equal m1 m2] returns true when [m1] and [m2] represent the same partial valuation *)
val equal : t -> t -> bool

(** [of_smt_model lst] constructs a Model from a Z3 association list [lst] *)
val of_smt_model : (Z3.Smtlib.identifier * Z3.Smtlib.term) list -> t

(** [extend_multi_model multi m] adds the model [m] to the multimap [multi]  *)
val extend_multi_model : Value.t list StringMap.t -> t -> Value.t list StringMap.t

(** [join m1 m2] computes the disjoint union of m1 & m2. Key overlaps are
  * allowed when the values are identical. An exception is thrown when two keys
  * exist with different values *)
val join : t -> t -> t

(** [aggregate ms] combines a list of models as in [join]  *)
val aggregate : t list -> t

(** [merge m1 m2 ~f] is a specialization of [Core.Map.merge] *)
val merge : t -> t -> f:(key:string -> [ `Left of Value.t | `Right of Value.t | `Both of Value.t * Value.t ] -> Value.t option) -> t

(** [fold m1 ~init ~f] is a specialization of [Core.Map.fold] *)
val fold : t -> init:'a -> f:(key:string -> data:Value.t -> 'a -> 'a) -> 'a

(** [iteri m1 ~init ~f] is a specialization of [Core.Map.iteri] *)
val iteri : t -> f:(key:string -> data:Value.t -> unit) -> unit

(** [diff m1 m2] Constructs a mapping from strings to pairs of values such that
  * mapping [k |-> (v1,v2)] occurs in [diff m1 m2] iff
  * [k |-> v1] in [m1] and [k |-> v2] in [m2]
  *)
val diff : t -> t -> (Value.t * Value.t) StringMap.t

(** [of_alist_exn assoc] constructs a valuation from an association list
  * i.e. [(k,v)] is an element of [assoc[] iff [k |-> v] in [of_alist_exn assoc] *)
val of_alist_exn : (string * Value.t) list -> t
