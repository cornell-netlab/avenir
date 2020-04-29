open Core
open Util
open Ast
open Tables


type t

val make : Problem.t -> t
val apply : t -> test -> test
val optimization : Parameters.t -> Problem.t -> test -> test
