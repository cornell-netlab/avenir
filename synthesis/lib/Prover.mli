open Ast
open Util
open Core
open Packet

(*
 * Interface to Z3
 *)

(*
 * Checks a formula .
 * Returns either None or Some model as well as the time it took to check.
 * if the `Sat constructor is passed, the formula is checked as satisfiable and the BITVEC solver is used.
 * if the `Valid constructor is passed, then the formula is negated and the QFBV solver is used.
 * if the `MinSat constructor is passed, then the difference between h_lo and h_hi is minimized
 * Prints debugging and interactive messages according to the flags in the parameters record
*)
val check : Parameters.t -> [> `Valid | `Sat ] -> test -> ((value StringMap.t) option * Time.Span.t)


(*Converts an ast test into an SMT-lib string. Assumes `Sat is intended*)
val toZ3String : test -> string
