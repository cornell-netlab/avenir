val implements :
      ?neg:Ast.test ->
      Parameters.t ->
      ProfData.t ref ->
      Problem.t -> 
      [> `NoAndCE of Packet.t * Packet.t | `Yes ]

(** [cegis_math params prof prob] is [opt_soln] iff ... *)
val cegis_math :
      Parameters.t ->
      ProfData.t ref -> 
      Problem.t -> 
      Tables.Edit.t list option

val cegis_math_sequence:
      Parameters.t ->
      ProfData.t ref -> 
      (unit -> Problem.t) ->
      (Problem.t * Tables.Edit.t list) option

val symb_wp : ?fvs:(string * int) list -> Ast.cmd -> Ast.test

val edit_cache : EAbstr.t ref
                     
