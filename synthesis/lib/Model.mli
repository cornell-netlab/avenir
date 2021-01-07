open Util
open Ast

type t

val to_string : t -> string

(** [of_smt_model lst] constructs a Model from a Z3 association list [lst] *)
val of_smt_model : (Z3.Smtlib.identifier * Z3.Smtlib.term) list -> t

val extend_multi_model : value list StringMap.t -> t -> value list StringMap.t

(** [join m1 m2] computes the disjoint union of m1 & m2. Key overlaps are
   allowed when the values are identical. An exception is thrown when two keys
   exist with different values *)
val join : t -> t -> t

(** [aggregate ms] combines a list of models as in [join]  *)
val aggregate : t list -> t

val merge : t -> t -> f:(key:string -> [ `Left of value | `Right of value | `Both of value * value ] -> value option) -> t
val fold : t -> init:'a -> f:(key:string -> data:value -> 'a -> 'a) -> 'a
val iteri : t -> f:(key:string -> data:value -> unit) -> unit
val set : t -> key:string -> data:value -> t
val empty : t

val equal : t -> t -> bool

val diff : t -> t -> (value * value) StringMap.t

val of_alist_exn : (string * value) list -> t

val find : t -> string -> value option
