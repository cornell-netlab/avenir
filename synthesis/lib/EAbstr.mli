val make : ?filename: string option -> unit -> unit
(** [make ()] constructs an empty edit cache
    [make filename ()] reads in an edit cache from a file *)

val clear : unit -> unit
(** [clear ()] empties the cache *)

type mapping

val mapping_to_yojson : mapping -> Yojson.Safe.t

val mapping_of_yojson : Yojson.Safe.t -> mapping Ppx_deriving_yojson_runtime.error_or

val random_mapping : unit -> mapping

val equal : mapping -> mapping -> bool

val string_of_mapping : mapping -> string

val dump_yojson : string -> unit
(** [dump_yojson filename] writes the current cache to the file at [filename]. *)

val infer : Parameters.t -> Cmd.t -> Edit.t -> Edit.t list option
(** [infer params cache phys e] checks whether a logical edit similar to [e]
    has been seen before in the cache, and if so, returns a list of edits
    [es] on the physical program [phys]. *)

val update : Edit.t -> Edit.t list -> unit
(** [update cache e es] and the solution [e -> es] to the cache, where [e] is
    an edit to the logical program and [es] is a list of edits on the
    physical programs*)
