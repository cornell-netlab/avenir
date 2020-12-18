open Ast
open Core
open Packet

(*
 * Interface to Z3
 *)

(*
 * Initialize provers using the binary location indicated by the string
 *)
val make_provers : string -> unit


(* Checks whether a formula is satisfiable; returns an optional model
   and timing data if it is *)
val check_sat : Parameters.t  -> test -> ((value StringMap.t) option * Time.Span.t)

(* Is a formula satisfiable? returns a boolean *)
val is_sat : Parameters.t -> test -> bool

(* Checks SMT Query for validity. returning an optional model and timing data *)
val check_valid : ?rv:bool -> Parameters.t -> test -> ((value StringMap.t) option * Time.Span.t)

(* returns true iff the test is valid *)
val is_valid : ?rv:bool -> Parameters.t -> test -> bool

(* Checks SMT Query for validity, using the abstraction cache;
   returns an optional model and timing data if it is *)
val check_valid_cached : Parameters.t -> test -> ((value StringMap.t) option * Time.Span.t)

(* Checks SMT Query for validity, using the abstraction cache; returns
   an optional model and timing data if it is *)
val is_valid_cached : Parameters.t -> test -> bool


(* Checks SMT Query as satisfiable, with the difference between h_lo
   and h_hi minimized. *)
val check_min : Parameters.t -> test -> ((value StringMap.t) option * Time.Span.t)

(*Converts an AST test into an SMT-lib string. Assumes [check_sat] is intended. *)
val toZ3String : test -> string
