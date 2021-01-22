val eval_act : Cmd.t -> Packet.t -> Packet.t
(** [eval_act c pkt] produces a packet corresponding to executing the program
    [c] on input packet [pkt], assumes [c] is action-like, i.e. has no tables*)

val fails_on_some_example :
  Parameters.t -> Problem.t -> (Packet.t * Packet.t) option
(** [fails_on_some_example params problem] checks whether, for
    [Problem.phys_gcl_program problem] there is a counterexample in
    [Problem.cexs problem] that is still unimplemented. *)
