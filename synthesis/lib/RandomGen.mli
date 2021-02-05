module Obt : sig
  val rand_edits : bool -> int -> int -> int -> int -> Edit.t list
  (** [rand_edits d b k a e] generates [e] random edits for table
      [gen d b k a]. *)

  val gen : bool -> int -> int -> int -> Cmd.t * (string * int) list
  (** [gen d b k a] generates [obt, fvs], where [obt] is a one big table with
      [k] keys and [a] actions each assigning the single argument to a
      different variable (assigned to action data if [d] is true and to a
      constant if [d] is false), and [fvs] is a list of the free variables in
      [obt]. All variables have [b] bits. *)

  val rand_big_edits :
       (string * (string * int) list * Cmd.t) list
    -> int
    -> int
    -> int
    -> Edit.t list
  (** [rand_edits acts b k e] generates [e] random edits for a obt with actions
      [acts] and [k] keys of bitwidth [b] *)

  val gen_big :
       int
    -> int
    -> int
    -> int
    -> int
    -> Cmd.t
       * (string * int) list
       * (string * (string * int) list * Cmd.t) list
  (** [gen b k v d a] generates [obt, fvs, acts], where [obt] is a one big
      table with [k] keys and the [a] actions [acts], with a [v] assigned
      vars, and [d] action data parameters. *)
end

module Pipe : sig
  val gen : bool -> int -> int -> int -> Cmd.t
  (** [gen d b k a] generates a pipeline of tables. The first has [k] keys
      and sets a single metadata field [meta] (defaulting to 0). It then
      generates [a] tables: table [t_i] matches on [meta] and executes a
      single action, which sets ["yi"] to the action data if [d] is true and
      to a constant if [d] is false). All variables have [b] bits *)

  val gen_big : int -> int -> int -> int -> int -> int -> Cmd.t
  (** [gen b k t v d a] generates a pipeline of tables. The first has [k]
      keys and sets a single metadata field [meta] (defaulting to 0). It then
      generates [t] tables, with a total of [a] actions, [v] vars, and [d]
      action data parameters. All variables have [b] bits *)
end
