open Util

(** A total map from table names to lists of rows. If no entry has been
    explicitly added for a given table, it is assumed have no rows. *)
type t

val empty : t
(** [empty] is the empty instance, where every table has 0 rows. *)

val to_string : t -> string
(** [to_string t] is a string representation of an instance---mainly useful
    for debugging *)

val equal : t -> t -> bool
(** [equal i1 i2] is [true] when [i1] and [i2] are identical*)

val size : t -> int
(** [size t] is the sum of the number of rows in each table. *)

val update : Parameters.t -> t -> Edit.t -> t
(** [update params inst e] is an instance constructed by executing edit [e]
    on [inst]. By default, i.e. when [params.above] is false, [e] is added to
    the bottom of the table (i.e. with the lowest priority). If
    [params.above] is true, they are added to the top (i.e with the highest
    priority). *)

val update_list : Parameters.t -> t -> Edit.t list -> t
(** [update_list params inst es] updates, as specified in [update] the
    instance [inst] with the edits [es]. *)

val of_edits : Parameters.t -> Edit.t list -> t
(** [of_edits params es] is a convenience function equivalent to
    [update_list params empty es] *)

val set_rows : t -> table:string -> rows:Row.t list -> t
(** [set_rows inst tbl rows] overwrites any existing rows in [inst] with
    [rows] *)

val get_rows : t -> string -> Row.t list
(** [get_rows inst tbl] returns [rows], a list of rows corresponding to the
    rows that [inst] holds for table [tbl]*)

val get_row : t -> string -> int -> Row.t option
(** [get_rows inst tbl i] returns [Some row], the [i]th row that [inst] holds
    for table [tbl], when [inst] holds at least [i+1] rows for [tbl],
    otherwise it returns [None] *)

val get_row_exn : t -> string -> int -> Row.t
(** [get_row_exn] is just like [get_row], but throws an exception instead of
    returning an optional row. *)

val get_rows_before : t -> string -> int -> Row.t list
(** [get_rows inst tbl i] returns [rows], the [i] rows for [tbl] in [inst]
    that occur above row [i]. If [i] is greater or equal to the number of
    rows, it returns all of the rows in the table.*)

val overwrite : t -> t -> t
(** [overwite i1 i2] overwrite the values in [i1] with their counterparts in
    [i2]. If a key exists in [i1] but not [i2], its value is preserved. If a
    key exists in [i2] but not [i1], its value is thrown away.*)

val negate_rows : t -> string -> Test.t
(** [negate_rows inst tbl] computes a test that is true when packets that
    reach table [tbl] miss every extant row *)

type interp =
  | NoHoles
  | OnlyHoles of Hint.t list
  | WithHoles of (string * int) list * Hint.t list

val interp_equal : interp -> interp -> bool
(** [interp_equal i1 i2] is equal when [i1] and [i2] are syntactically
    equivalent, relying on the equivalence checkers for [List], [Hint], and
    ints. *)

val apply :
     ?no_miss:bool
  -> ?ghost_edits:int list StringMap.t
  -> Parameters.t
  -> interp
  -> [< `Exact | `Mask]
  -> t
  -> Cmd.t
  -> Cmd.t
(** [apply ~no_miss ~ghost_edits params interp typ inst c] applies the
    instance [inst] to program [c]. The output program is a Ast.t program
    [c'] with no tables. If [no_miss] is [true], (default [false]) the
    default actions in tables are omitted. The variables in [ghost_edits] let
    you instrument indexed rows (the data) in specific tables (the keys) with
    "t_hits_row_i" booleans, that you can use in static analyses. The role of
    [interp] is described above. The final configuration argument [typ]
    determines the "match-kind" of the match-holes that are to be inserted
    (if any at all); [`Exact] indicates exact matches, and [`Mask] indicates
    ternary matches *)

val verify_apply : ?no_miss:bool -> Parameters.t -> t -> Cmd.t -> Cmd.t
(** [verify_apply ~no_miss params inst c] applies [inst] to table [c] without
    exposing any of the hole parameters. Comparing with [apply] above,
    [~ghost_edits] is empty, and the tag type is ignored *)

val project : int list StringMap.t -> t -> t
(** [project slice inst] is [inst'] where [inst'] has the rows of [inst]
    identified by [slice]. Treating [slice] as a multimap, If [slice] maps
    [tbl -> i], then the [i]th row for [tbl] in [inst] is kept. Preserves the
    order of [inst] *)

val fold :
  t -> init:'a -> f:(table:string -> rows:Row.t list -> 'a -> 'a) -> 'a
(** [fold] folds over the instance *)
