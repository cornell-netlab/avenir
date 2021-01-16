(** A Packet is a partial map from variables (strings) to values *)
type t

val empty : t
(** [empty] defines a packet which defines the values of no variables *)

val to_string : t -> string
(** [to_string pkt] produces a string representation of p*)

val equal : ?fvs:(string * int) list option -> t -> t -> bool
(** [equal ~fvs pkt pkt'] is true when [pkt] and [pkt'] are equivalent on the
    fields in [fvs]. If [fvs] us omitted or [None], then [equal pkt pkt'] is
    true when [pkt] and [pkt'] are equivalent maps *)

val set_field : t -> string -> Value.t -> t
(** [set_field pkt x v] updates [pkt] so that [x] has value [v], i.e.
    [pkt{x |-> v}] *)

val set_field_of_expr_opt : t -> string -> Expr.t -> t option
(** [set_field pkt x e] updates [pkt] by evaluating [e] using [pkt] as a
    valuation to some value [v] and updating [pkt] to map [x] to [v]. If [e]
    cannot be evaluated (e.g. because of an undefined variable), return None*)

val get_val_opt : t -> string -> Value.t option
(** [get_val_opt pkt s] optionally gets [pkt]'s valuation of [x], returning
    [None] if [pkt] is undefined on [x] *)

val get_val : t -> string -> Value.t
(** [get_val_opt pkt s] is just like [get_val_opt pkt s] except that it
    throws an exception instead of returning [None] *)

val remake : ?fvs:(string * int) list option -> t -> t
(** [remake ~fvs pkt] returns [pkt] if [fvs] is omitted or missing. Otherwise
    it restricts the domain of [pkt] to the variables in [fvs], and fills any
    undefined variables (i.e. those in [fvs] but not in the domain of [pkt])
    with random values. *)

val of_smt_model : (Z3.Smtlib.identifier * Z3.Smtlib.term) list -> t
(** [of_smt_model lst] constructs a packet from a Z3 association list [lst]. *)

val restrict : (string * int) list -> t -> t
(** [restrict fvs pkt] restricts [pkt] to only those fields in [fvs], (c.f.
    [remake]) *)

val extract_inout_ce : t -> t * t
(** [extract_inout_ce fvs pkt] converts a passive packet [pkt] with indexed
    variables (e.g. [x$0],[x$1], etc) into a pair of packets [pktin] and
    [pktout], where [pktin] represents the input variables (e.g. [x$0]) and
    [pktout] represents the output variables (e.g. [x$n] where [n > i] for
    every [x$i] in the domain of [pkt]).*)

val fold : t -> init:'a -> f:(key:string -> data:Value.t -> 'a -> 'a) -> 'a
(** [fold] Folds over the packet as a specialization of [Core.Map.fold] *)

val to_test : ?random_fill:bool -> fvs:(string * int) list -> t -> Test.t
(** [to_test ~random_fill ~fvs pkt] restricts [pkt] to the domain [fvs] and
    converts it into the corresponding test. If [random_fill] is [true], then
    variables in [fvs] that do not occur in the domain of [pkt] are replaced
    with random values; otherwise, they are simply omitted. *)

val to_assignment : t -> Cmd.t
(** [to_assignment pkt] converts [pkt] to a command [c] that will produce
    [pkt] when run on [empty]. *)

val diff_vars : t -> t -> string list
(** [diff_vars pkt pkt'] computes the variables that are different between
    [pkt] and [pkt'], where a variable is different either because it is
    undefined in exactly one of [pkt] or [pkt'], or because the two packets
    define unequal values for the variable. *)

val to_expr_map : t -> Expr.t Util.StringMap.t
(** [to_expr_map pkt] produces a map [m] such that if [x |-> v] in [pkt] then
    [x |-> Value v] in [m] *)

val mk_packet_from_list : (string * Value.t) list -> t
(** [mk_packet_from_list l] constructs a packet from the association list [l] *)
