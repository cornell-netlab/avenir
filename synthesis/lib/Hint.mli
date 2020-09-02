open Util
open Ast
open Tables

type t

(* val restriction : [`Mask | `Exact] -> t list -> test *)

val to_string : t -> string


(** [construct log phys e] produces makes heuristic guesses about how
   to accomodate the insertion of e into log in phys **)
val construct : cmd -> cmd -> Edit.t -> t list

val to_model : cmd -> t -> value StringMap.t

(* union a disjoint sequence of hints. Conflicts throw an assertion error *)
val list_to_model : cmd -> t list -> value StringMap.t


val join_models : value StringMap.t -> value StringMap.t -> value StringMap.t
