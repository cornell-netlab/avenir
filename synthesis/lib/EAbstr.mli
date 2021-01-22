val make : unit -> unit
(** [make ()] constructs an empty edit cache *)

val clear : unit -> unit
(** [clear ()] empties the cache *)  
   
val infer : Parameters.t -> Cmd.t -> Edit.t -> Edit.t list option
(** [infer params cache phys e] checks whether a logical edit similar to [e] has
   been seen before in the cache, and if so, returns a list of edits [es] on the
   physical program [phys].  *)
  
val update : Edit.t -> Edit.t list -> unit
(** [update cache e es] and the solution [e -> es] to the cache, where [e] is an
   edit to the logical program and [es] is a list of edits on the physical
   programs*)
