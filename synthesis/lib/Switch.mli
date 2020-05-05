open Core
open Util
open Ast
open Tables



type t

val make : cmd -> Instance.t ->  Edit.t list -> t

val pipeline : t -> cmd
val inst : t -> Instance.t
val edits : t -> Edit.t list

val edited_instance : t -> Instance.t
val to_gcl : t -> cmd
val to_gcl_holes : t -> Instance.interp -> [ `Exact | `Mask] -> cmd

val to_string : t -> string

val clear_cache : t -> t

val replace_edits : t -> Edit.t list -> t
val append_edits : t -> Edit.t list -> t

val update_inst : t -> Edit.t list -> t
val replace_inst : t -> Instance.t -> t

val commit_edits : t -> t

val replace_pipeline : t -> cmd -> t
