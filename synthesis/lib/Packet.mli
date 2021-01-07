open Ast

(** A Packet is a partial map from variables (strings) to values *)
type t

(** [empty] defines a packet which defines the values of no variables *)
val empty : t

(** [to_string pkt] produces a string representation of p*)
val to_string : t -> string

(** [equal ~fvs pkt pkt'] is true when [pkt] and [pkt'] are equivalent on the
   fields in [fvs]. If [fvs] us omitted or [None], then [equal pkt pkt'] is true
   when [pkt] and [pkt'] are equivalent maps *)
val equal : ?fvs:((string * int) list option) -> t -> t -> bool


(** [set_field pkt x v] updates [pkt] so that [x] has value [v], i.e. [pkt{x |-> v}]  *)
val set_field : t -> string -> value -> t

(** [set_field pkt x e] updates [pkt] by evaluating [e] using [pkt] as a
   valuation to some value [v] and updating [pkt] to map [x] to [v]. If [e]
   cannot be evaluated (e.g. because of an undefined variable), return None*)
val set_field_of_expr_opt : t -> string -> expr -> t option

(** [get_val_opt pkt s] optionally gets [pkt]'s valuation of [x], returning
   [None] if [pkt] is undefined on [x] *)
val get_val_opt : t -> string -> value option

(** [get_val_opt pkt s] is just like [get_val_opt pkt s] except that it throws
   an exception instead of returning [None] *)
val get_val : t -> string -> value

(** [remake ~fvs pkt] returns [pkt] if [fvs] is omitted or missing. Otherwise it
   restricts the domain of [pkt] to the variables in [fvs], and fills any
   undefined variables (i.e. those in [fvs] but not in the domain of [pkt]) with
   random values. *)
val remake : ?fvs:((string * int) list option) -> t -> t

(** [of_smt_model lst] constructs a packet from a Z3 association list [lst]. *)
val of_smt_model : (Z3.Smtlib.identifier * Z3.Smtlib.term) list -> t

(** [extract_inout_ce pkt] converts a passive packet [pkt] with indexed
   variables (e.g. [x$0],[x$1], etc) into a pair of packets [pktin] and
   [pktout], where [pktin] represents the input variables (e.g. [x$0]) and
   [pktout] represents the output variables (e.g. [x$n] where [n > i] for every
   [x$i] in the domain of [pkt]). *)
val extract_inout_ce : t -> (t * t)

(** [fold] Folds over the packet as a specialization of [Core.Map.fold] *)
val fold : t -> init:'a -> f:(key:string -> data:value -> 'a -> 'a) -> 'a

(** [to_test ~random_fill ~fvs pkt] restricts [pkt] to the domain [fvs] and
   converts it into the corresponding test. If [random_fill] is [true], then
   variables in [fvs] that do not occur in the domain of [pkt] are replaced with
   random values; otherwise, they are simply omitted. *)
val to_test : ?random_fill:bool -> fvs:(string * int) list -> t -> test

(** [to_assignment pkt] converts [pkt] to a command [c] that will produce [pkt]
   when run on [empty]. *)
val to_assignment : t -> cmd

(** [diff_vars pkt pkt'] computes the variables that are different between [pkt]
   and [pkt'], where a variable is different either because it is undefined in
   exactly one of [pkt] or [pkt'], or because the two packets define unequal
   values for the variable. *)
val diff_vars : t -> t -> string list

(** [to_expr_map pkt] produces a map [m] such that if [x |-> v] in [pkt] then [x |-> Value v] in [m]  *)
val to_expr_map : t -> expr Util.StringMap.t
