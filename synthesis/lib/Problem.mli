open Core
open Util
open Ast
open Tables

type t

val make :
  ?phys_drop_spec:test option ->
  log:cmd -> phys:cmd -> fvs:(string * int) list
  -> log_inst:Instance.t -> phys_inst:Instance.t
  -> log_edits:Edit.t list -> unit -> t

val empty : t

val to_string : Parameters.t -> t -> string

val fvs : t -> (string * int) list
val cexs : t -> (Packet.t * Packet.t) list
val model_space : t -> test
val attempts : t -> value StringMap.t list
val add_cex : t  -> (Packet.t * Packet.t) -> t

val log : t -> cmd
val log_inst : t -> Instance.t
val log_edits : t -> Edit.t list
val log_edited_instance : Parameters.t -> t -> Instance.t
val log_gcl_program : Parameters.t -> t -> cmd

val phys : t -> cmd
val phys_inst : t -> Instance.t
val phys_edits : t -> Edit.t list
val phys_gcl_program : Parameters.t -> t -> cmd
val phys_edited_instance : Parameters.t -> t -> Instance.t
val phys_gcl_holes : Parameters.t -> t -> Instance.interp -> [`Exact | `Mask] -> cmd
val phys_drop_spec : t -> test option


val slice : Parameters.t -> t -> t
val update_log : t -> cmd -> t
val update_phys : t -> cmd -> t
val append_log_edits : t -> Edit.t list -> t
val append_phys_edits : t -> Edit.t list -> t
val replace_log_edits : t -> Edit.t list -> t
val replace_phys_edits : t -> Edit.t list -> t
val delete_phys_edits : t -> t
val apply_edits_to_log : Parameters.t -> t -> Edit.t list -> t
val apply_edits_to_phys : Parameters.t -> t -> Edit.t list -> t
val commit_edits_log : Parameters.t -> t -> t
val commit_edits_phys : Parameters.t -> t -> t

val reset_attempts : t -> t
val add_attempt : t -> value StringMap.t -> t
val attempts_to_string : t -> string
val num_attempts : t -> int
val seen_attempt : t -> value StringMap.t -> bool


val add_cex : t -> Packet.t * Packet.t -> t

val reset_model_space : t -> t
val refine_model_space : t -> test -> t
val set_model_space : t -> test -> t

