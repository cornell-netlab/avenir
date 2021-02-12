open Core

(** Query cache *)
val cache : QAbstr.t ref

(*
 * Interface to Z3
 *)

(*
 * Initialize provers using the binary location indicated by the string
 *)
val make_provers : string -> unit

(* Checks whether a formula is satisfiable; returns an optional model and
   timing data if it is *)
val check_sat : Parameters.t -> Test.t -> Model.t option * Time.Span.t

(* Is a formula satisfiable? returns a boolean *)
val is_sat : Parameters.t -> Test.t -> bool

(* Checks SMT Query for validity. returning an optional packet counterexample
   and timing data *)
val check_valid : Parameters.t -> Test.t -> Packet.t option * Time.Span.t

(* returns true iff the Test.t is valid *)
val is_valid : Parameters.t -> Test.t -> bool

(* Checks SMT Query for validity, using the abstraction cache; returns an
   optional packet counterexample and timing data if it is *)
val check_valid_cached :
  Parameters.t -> Test.t -> Packet.t option * Time.Span.t

(* Checks SMT Query for validity, using the abstraction cache; returns an
   optional model and timing data if it is *)
val is_valid_cached : Parameters.t -> Test.t -> bool

(*Converts an AST Test.t into an SMT-lib string. Assumes [check_sat] is
  intended. *)
val toZ3String : Test.t -> string
