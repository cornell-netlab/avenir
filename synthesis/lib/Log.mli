open Core

val set_level : string list -> unit
(** [set_level strs] parses the command-line interface. Accepts a list of
    string identifiers ["debug"], ["warn"], ["info"], ["z3"], or ["ecache"],
    whose presence indicates that that logging level should be set. Also
    accepts a list containing a single string of the form
    [\[d|D\]?\[w|W\]?\[i|I\]?z?\[e|E\]?] as indicating the first letter
    logging level. The levels are completely independent -- none subsumes any
    other *)

val set_warn : unit -> unit
(** [set_warn] globally enables printing for warning statements *)

val set_info : unit -> unit
(** [set_warn] globally enables printing for info statements *)

val set_debug : unit -> unit
(** [set_debug] globally enables printing for debugging statements *)

val set_z3 : unit -> unit
(** [set_z3] globally enables printing for smt formulae statements *)

val set_ecache : unit -> unit
(** [set_ecache] globally enables printing for cache-related statements *)

val warn : string Lazy.t -> unit
(** [warn s] forces [s] and prints it if the debugging level is set to [warn] *)

val info : string Lazy.t -> unit
(** [info s] forces [s] and prints it if the debugging level is set to [info] *)

val debug : string Lazy.t -> unit
(** [debug s] forces [s] and prints it if the debugging level is set to
    [debug] *)

val z3 : string Lazy.t -> unit
(** [z3 s] forces [s] and prints it if the debugging level is set to [z3] *)

val ecache : string Lazy.t -> unit
(** [ecache s] forces [s] and prints it if the debugging level is set to
    [ecache] *)

val id_print : s:('a -> string) -> p:(string Lazy.t -> unit) -> 'a -> 'a
(** [id_print s p x] lazily converts [x] to a string using [s] and prints it
    using [p]. It returns the original [x]*)

val abs_log_file : string option -> unit
(** [abs_log_file s] initializes the abstract row logger with filepath s*)

val tgt_log_file : string option -> unit
(** [tgt_log_file s] initializes the target row logger with filepath s*)

val log_abs : string -> unit
(** Log the string to the abstract log file, if initialized, or do nothing*)

val log_tgt : string -> unit
(** Log the string to the target log file, if initialized, or do nothing*)
