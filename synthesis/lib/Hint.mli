type t

val equal : t -> t -> bool

(* val restriction : [`Mask | `Exact] -> t list -> test *)

val to_string : t -> string

val construct : Cmd.t -> Edit.t -> t list
(** [construct phys e] produces makes heuristic guesses about how phys can
    accomodate the logical insertion of e **)

(* union a disjoint sequence of hints. Conflicts throw an assertion error *)
val list_to_model : [`Vals | `NoVals] -> Cmd.t -> t list -> Model.t
