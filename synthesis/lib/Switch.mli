type t

val make :
  ?drop_spec:Test.t option -> Cmd.t -> Instance.t -> Edit.t list -> t

val pipeline : t -> Cmd.t

val inst : t -> Instance.t

val edits : t -> Edit.t list

val drop_spec : t -> Test.t option

val slice : Parameters.t -> t -> t

val edited_instance : Parameters.t -> t -> Instance.t

val to_gcl : Parameters.t -> (string * int) list -> t -> Cmd.t

val to_gcl_holes :
  Parameters.t -> t -> Instance.interp -> [`Exact | `Mask] -> Cmd.t

val to_string : Parameters.t -> t -> string

val clear_cache : t -> t

val replace_edits : t -> Edit.t list -> t

val append_edits : t -> Edit.t list -> t

val update_inst : Parameters.t -> t -> Edit.t list -> t

val replace_inst : t -> Instance.t -> t

val commit_edits : Parameters.t -> t -> t

val replace_pipeline : t -> Cmd.t -> t
