open Core

val set_level : string list -> unit
(** [set_level strs] parses the command-line interface. Accepts a list of
    string identifiers ["debug"], ["warn"], or ["info"], whose presence
    indicates that that logging level should be set. Also accepts a single
    string of the form [\[d|D\]?\[w|W\]?\[i|I\]?] as shorcuts for [(d)ebug],
    [(w)arn] and [(i)nfo] respectively*)

val set_warn : unit -> unit
(** [set_warn] globally enables printing for warning statements *)

val set_info : unit -> unit
(** [set_warn] globally enables printing for info statements *)

val set_debug : unit -> unit
(** [set_debug] globally enables printing for debugging statements *)

val warn : string Lazy.t -> unit
(** [warn s] forces [s] and prints it if the debugging level is set to [warn] *)

val info : string Lazy.t -> unit
(** [info s] forces [s] and prints it if the debugging level is set to [info] *)

val debug : string Lazy.t -> unit
(** [debug s] forces [s] and prints it if the debugging level is set to
    [debug] *)

val z3 : string Lazy.t -> unit
(** [z3 s] forces [s] and prints it if the debugging level is set to [z3] *)
