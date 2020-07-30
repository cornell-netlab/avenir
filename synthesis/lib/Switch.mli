open Ast
open Tables

type t

val make : ?drop_spec:(test option) -> cmd -> Instance.t ->  Edit.t list -> t

val pipeline : t -> cmd
val inst : t -> Instance.t
val edits : t -> Edit.t list
val drop_spec : t -> test option

val edited_instance : Parameters.t -> t -> Instance.t
val to_gcl : Parameters.t -> t -> cmd
val to_gcl_holes : Parameters.t -> t -> Instance.interp -> [ `Exact | `Mask] -> cmd

val to_string : Parameters.t -> t -> string

val clear_cache : t -> t

val replace_edits : t -> Edit.t list -> t
val append_edits : t -> Edit.t list -> t

val update_inst : Parameters.t -> t -> Edit.t list -> t
val replace_inst : t -> Instance.t -> t

val commit_edits : Parameters.t -> t -> t

val replace_pipeline : t -> cmd -> t
