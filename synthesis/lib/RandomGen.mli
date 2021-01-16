module Obt : sig
  val rand_edits : int -> int -> int -> int -> Edit.t list
  (** [rand_edits b k a e] generates [e] random edits for table [gen b k a]*)

  val gen : int -> int -> int -> Cmd.t * (string * int) list
  (** [gen b k a] generates [obt, fvs], where [obt] is a one big table with
      [k] keys and [a] actions each assigning the single argument to a
      different variable, and [fvs] is a list of the free variables in [obt].
      All variables have [b] bits *)
end

module Pipe : sig
  val gen : int -> int -> int -> Cmd.t
  (** [gen b k a] generates a pipeline of tables. The first has [k] keys and
      sets a single metadata field [meta] (defaulting to 0). It then
      generates [a] tables: table [t_i] matches on [meta] and executes a
      single action, which sets ["yi"] to the argument's value. All variables
      have [b] bits *)
end
