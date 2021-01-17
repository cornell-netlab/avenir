(** [styp] determines the type of selection statement*)
type styp = Partial | Total | Ordered

val styp_equals : styp -> styp -> bool
(** [stype_equals s s'] is [true] when [s] and [s'] are the same selection
    type*)

val string_of_styp : styp -> string
(** [sexp_string_of_typ s] is an Sexp string for the selection type*)

val sexp_string_of_styp : styp -> string

module Key : sig
  (** The key module represents keys in a table. They can optionally be
      labelled (as part of static analyses) with a value indicating that the
      value of the key is known statically *)
  type t

  val make : string * int -> t
  (** [make (s,i)] is a key with variable name [s] and an integer size [i]*)

  val set_val : t -> Value.t -> t
  (** [set_val k v] Labels the key [k] with the value [v] indicating that its
      value is known statically*)

  val set_val_opt : t -> Value.t option -> t
  (** [set_val k opt] Optionally labels the key [k] a value if [vopt] is
      some. Otherwise it removes any previous label *)

  val has_value : t -> bool
  (** [has_value k] is [true] if the key [k] has a statically known value *)

  (* TODO rename to equal *)
  val equals : t -> t -> bool
  (** [equals k k'] is true if [k] and [k'] are the same key ** TODO ::
      Refactor this to [equal] *)

  val to_string : t -> string
  (** [to_string k] is a string representation of the key [k] *)

  val value : t -> Value.t option
  (** [value k] is the statically known value of the key [k], if it is
      labelled with a value, otherwise is *)

  val to_sized : t -> string * int
  (** [to_sized k] is a pair [(x,i)] where [x] is the name of the key and [i]
      is the size of the variable. Notice that [Fn.compose to_sized make] is
      the identify function *)

  val str_map : f:(string -> string) -> t -> t
  (** [str_map ~f k] applies the function [f] to the variable name without
      changing the size or value label *)

  val var_name : t -> string
  (** [var_name t] is the string variable name. It differs from [to_string]
      in that it neither contains the size information nor the label
      information *)
end

(** [t] is the type of commands. We assume universally that table names are
    unique, in a program though this isn't checked anywhere.*)
type t =
  | Skip
  | Assign of (string * Expr.t)
  | Assume of Test.t
  | Seq of (t * t)
  | Select of (styp * (Test.t * t) list)
  | Apply of
      { name: string
      ; keys: Key.t list
      ; actions: (string * (string * int) list * t) list
      ; default: t }

val to_string : ?depth:int -> t -> string
(** [to_string ?depth c] is a string representation of the command c with
    indentation depth [~depth]. If [~depth] is omitted, it defaults to 0 *)

val to_sexp_string : t -> string
(** [to_sexp_string ?depth c] is an Sex string representation of the command
    c *)

val is_skip : t -> bool
(** [is_skip c] is [true] when [c = Skip] *)

val equals : t -> t -> bool
(** [is_skip c1 c2] is [true] when [c1] and [c2] are the same AST *)

val num_nodes : t -> int
(** [is_skip c] is the number of AST nodes in [c] *)

val seq : t -> t -> t
(** [seq c1 c2] sequentially composes [c1] and [c2]. Equivalent to
    [c1 %:% c2] *)

val ( %:% ) : t -> t -> t

val sequence : t list -> t
(** [sequence cs] sequentially composes a list of [c1::...::cn::\[\]] in
    order. Equivalent to [c1 %:% ... %:% cn %:% Skip] *)

val assign : string -> Expr.t -> t
(** [assign f e] constructs an assignment command, assigning the expression
    [e] to the variable [f]. equivalent to [f %<-% e] *)

val ( %<-% ) : string -> Expr.t -> t

val assume : Test.t -> t
(** [assume t] constructs an assumption command, assuming that t is false*)

val partial : (Test.t * t) list -> t
(** [partial ss] is a DEPRECATED smart constructor for selections of type
    [Partial]. ** A partial selection doesn't stipulate that any of the
    alternatives in [ss] are true. Nondeterministic. *)

val total : (Test.t * t) list -> t
(** [Total ss] is a DEPRECATED smart constructor for selections of type
    [Total]. It asserts that one of the case alternatives in [ss] succeeds.
    This is the standard Dijkstra selection statement. *)

val ordered : (Test.t * t) list -> t
(** [Ordered ss] is a smart constructor for selections of type [Ordered]. It
    tries each alternative in [ss] in sequence. Similar to
    [if-else-if-...-else] statement *)

val select : styp -> (Test.t * t) list -> t
(** [select t ss] is a smart constructor that generalizes over the previous
    three takin the selection type as an argument.*)

val apply :
     string
     * (string * int) list
     * (string * (string * int) list * t) list
     * t
  -> t
(** [apply name ks acts] is a table command with name [name] keys constructed
    from [ks] and actions [acts] *)

val free_keys : Key.t list -> (string * int) list
(** [free_keys ks] returns the sized representation of the valueless keys
    [ks] *)

val vars : t -> (string * int) list
(** [vars c] is a list of sized variables (including keys) corresponding to
    the [Vars] in the command. *)

val holes : t -> (string * int) list
(** [holes c] is a list of holes variables to the [Holes] in the command c. *)

val holify : ?f:(string * int -> string * int) -> string list -> t -> t
(** [holify f vs a] replaces each sized variable [(v,sz)] in [vars c ∩ vs]
    with [f (v,sz)] *)

val get_schema_of_table :
     string
  -> t
  -> (Key.t list * (string * (string * int) list * t) list * t) option
(** [get_schema_of_table name c] extracts the schema [ks, acts, def]
    corresponding to tables in [c] named [name]. [ks] is the list of keys in
    the table [acts] is the list of actions, and [def] is the default action *)

val table_vars :
     ?keys_only:bool
  -> Key.t list * ('b * (string * 'c) list * t) list * t
  -> (string * int) list
(** [table_vars ~keys_only k] extracts the variables from a table schema
    (c.f. [get_schema_of_table]). If [~keys_only] is true then it only
    returns the keys. *)

val get_tables_vars :
  ?keys_only:bool -> t -> (string * (string * int) list) list
(** [get_table_vars ~keys_only c] produces an association list where the key
    of the association list is the table name and the data are the variables
    in that table. If [keys_only] is [true] it returns only the keys of the
    table. (c.f. [get_tables_keys]) *)

val get_tables_actions :
  t -> (string * (string * (string * int) list * t) list) list
(** [get_tables_actions c] produces an association list where the key of the
    association list is the table name and the data are the actions in that
    table. *)

val get_actions : t -> (string * (string * int) list * t) list
(** [get_actions c] produces an list of the actions in any table of [c] *)

val get_tables_actsizes : t -> (string * int) list
(** [get_tables_actsizes c] produces an association list where the keys are
    the table names in [c] and the data is the number of actions in that
    table *)

val get_tables_keys : t -> (string * Key.t list) list
(** [get_tables_keys c] produces an association list of tables names and the
    corresponding keys *)

val tables : t -> string list
(** [tables c] is a list of table names in [c] *)

val num_table_paths : t -> Bigint.t
(** [num_table_paths c] is the number of paths, counting each possible action
    as a different path *)

val num_paths : t -> Bigint.t
(** [num_paths c] is the number of paths, where all paths through a table are
    counted as a single path *)

val multi_vals : t -> Value.t list
(** [multi_vals c] is the list of values that occur in [c]. The [multi_]
    prefix indicates that the list may contain duplicates *)

val assigned_vars : t -> Util.StringSet.t
(** [assigned_vars c] is a set of variables that occur on the lhs of an
    assignment in [c] *)
