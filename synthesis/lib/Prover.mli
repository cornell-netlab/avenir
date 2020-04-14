open Ast
open Util
open Core
open Packet

(*
 * Interface to Z3
 *)

(* Checks a formula as satisfiable. *)
val check_sat : Parameters.t  -> test -> ((value StringMap.t) option * Time.Span.t)

(* Checks SMT Query for validity. *)
val check_valid : Parameters.t -> test -> ((value StringMap.t) option * Time.Span.t)

(* Checks SMT Query as satisfiable, with the difference between h_lo and h_hi minimized. *)
val check_min : Parameters.t -> test -> ((value StringMap.t) option * Time.Span.t)

(*Converts an AST test into an SMT-lib string. Assumes [check_sat] is intended. *)
val toZ3String : test -> string
