open Ast


type t

val make : Problem.t -> t
val apply : t -> test -> test
val optimization : Parameters.t -> Problem.t -> test -> test
