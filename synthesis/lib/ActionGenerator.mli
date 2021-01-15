open Util

type t = {target_vars: StringSet.t; source_keys: StringSet.t}

val mkstate : StringSet.t -> StringSet.t -> t

(* val possible_actions : Packet.t -> Packet.t -> Cmd.t -> value StringMap.t
   list *)

val positive_actions :
     Parameters.t
  -> Cmd.t
  -> (string * int) list
  -> Packet.t
  -> Packet.t
  -> (string * ((string * int) list * int * Cmd.t) list) list * StringSet.t

val traces :
     Edit.t
  -> (string * int) list
  -> Cmd.t
  -> Packet.t
  -> Packet.t
  -> (t * (string * int) list) list

val string_of_traces : (t * (string * int) list) list -> string

val equal_trace_lists :
  (t * (string * int) list) list -> (t * (string * int) list) list -> bool

val tables_affected_by_keys :
  Cmd.t -> StringSet.t -> StringSet.t * StringSet.t

val feasible_tables :
     Cmd.t
  -> (string * int) list
  -> Match.t list
  -> Packet.t
  -> Packet.t
  -> (string * StringSet.t * StringSet.t) list

val reach_positive_actions :
  Parameters.t -> Problem.t -> Packet.t -> Packet.t -> Test.t list
