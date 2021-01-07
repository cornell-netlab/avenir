open Ast

type t

(* val restriction : [`Mask | `Exact] -> t list -> test *)

val to_string : t -> string


(** [construct phys e] produces makes heuristic guesses about how
    phys can accomodate the logical insertion of e **)
val construct : cmd -> Edit.t -> t list


(* union a disjoint sequence of hints. Conflicts throw an assertion error *)
val list_to_model : [`Vals | `NoVals] -> cmd -> t list -> Model.t
