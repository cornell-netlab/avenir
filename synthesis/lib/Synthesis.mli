val implements :
     ?neg:Test.t
  -> Parameters.t
  -> ProfData.t ref
  -> Problem.t
  -> (Packet.t * Packet.t) option

val cegis_math :
  Parameters.t -> ProfData.t ref -> Problem.t -> Edit.t list option
(** [cegis_math params prof prob] is [opt_soln] iff ... *)

val cegis_math_sequence :
     Parameters.t
  -> ProfData.t ref
  -> (unit -> Problem.t)
  -> (Problem.t * Edit.t list) option

val edit_cache : EAbstr.t ref
