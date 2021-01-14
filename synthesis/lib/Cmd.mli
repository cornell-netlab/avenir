type styp = Partial | Total | Ordered

val styp_equals : styp -> styp -> bool

val string_of_styp : styp -> string

val sexp_string_of_styp : styp -> string

module Key : sig
  type t

  val make : string * int -> t

  val set_val : t -> Value.t -> t

  val set_val_opt : t -> Value.t option -> t

  (* TODO rename to equal *)
  val equals : t -> t -> bool

  val to_string : t -> string

  val has_value : t -> bool

  val value : t -> Value.t option

  val to_sized : t -> string * int

  val str_map : f:(string -> string) -> t -> t

  val var_name : t -> string
end

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

val to_sexp_string : t -> string

val is_skip : t -> bool

val equals : t -> t -> bool

val num_nodes : t -> int

val seq : t -> t -> t

val ( %:% ) : t -> t -> t

val assign : string -> Expr.t -> t

val ( %<-% ) : string -> Expr.t -> t

val assume : Test.t -> t

val partial : (Test.t * t) list -> t

val total : (Test.t * t) list -> t

val ordered : (Test.t * t) list -> t

val select : styp -> (Test.t * t) list -> t

val apply :
     string
     * (string * int) list
     * (string * (string * int) list * t) list
     * t
  -> t

val free_keys : Key.t list -> (string * int) list

val holify : ?f:(string * int -> string * int) -> string list -> t -> t

val sequence : t list -> t

val vars : t -> (string * int) list

val holes : t -> (string * int) list

val get_schema_of_table :
     string
  -> t
  -> (Key.t list * (string * (string * int) list * t) list * t) option

val table_vars :
     ?keys_only:bool
  -> Key.t list * ('b * (string * 'c) list * t) list * t
  -> (string * int) list

val get_tables_vars :
  ?keys_only:bool -> t -> (string * (string * int) list) list

val get_tables_actions :
  t -> (string * (string * (string * int) list * t) list) list

val get_actions : t -> (string * (string * int) list * t) list

val get_tables_actsizes : t -> (string * int) list

val get_tables_keys : t -> (string * Key.t list) list

val tables : t -> string list

val num_table_paths : t -> Bigint.t

val num_paths : t -> Bigint.t

val multi_vals : t -> Value.t list

val assigned_vars : t -> Util.StringSet.t
