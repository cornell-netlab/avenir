open Ast
open Util

type t = {
    target_vars : StringSet.t;
    source_keys : StringSet.t
  }

val mkstate : StringSet.t -> StringSet.t -> t

(* val possible_actions : Packet.t -> Packet.t -> cmd -> value StringMap.t list *)

val positive_actions : Parameters.t -> cmd -> (string * size) list -> Packet.t -> Packet.t -> (string * ((string * size) list * int * cmd) list) list * StringSet.t

val traces : Edit.t -> (string * size) list -> cmd -> Packet.t -> Packet.t -> (t * (string * int) list) list


val string_of_traces : (t * (string * int) list) list -> string


val equal_trace_lists : (t * (string * int) list) list -> (t * (string * int) list) list -> bool


val tables_affected_by_keys : cmd -> StringSet.t -> StringSet.t * StringSet.t

val feasible_tables : cmd -> (string * int) list -> Match.t list -> Packet.t -> Packet.t -> (string * StringSet.t * StringSet.t) list

val reach_positive_actions : Parameters.t -> Problem.t -> Packet.t -> Packet.t -> test list
