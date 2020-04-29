open Core
open Util
open Ast
open Tables

type t

val make : log:cmd -> phys:cmd -> fvs:(string * int) list
           -> log_inst:Instance.t -> phys_inst:Instance.t
           -> log_edits:Edit.t list -> t

val to_string : t -> string

val fvs : t -> (string * int) list
val cexs : t -> (Packet.t * Packet.t) list
val model_space : t -> test
val attempts : t -> value StringMap.t list

val log : t -> cmd
val log_inst : t -> Instance.t
val log_edits : t -> Edit.t list
val log_edited_instance : t -> Instance.t
val log_gcl_program : t -> cmd

val phys : t -> cmd
val phys_inst : t -> Instance.t
val phys_edits : t -> Edit.t list
val phys_gcl_program : t -> cmd
val phys_edited_instance : t -> Instance.t
val phys_gcl_holes: t -> [`NoHoles | `OnlyHoles of Hint.t list | `WithHoles of (string * int) list * Hint.t list] -> [< `Exact | `Range] -> cmd


val slice : t -> t
val update_phys : t -> cmd -> t
val append_phys_edits : t -> Edit.t list -> t
val replace_log_edits : t -> Edit.t list -> t
val replace_phys_edits : t -> Edit.t list -> t
val delete_phys_edits : t -> t
val apply_edits_to_log : t -> Edit.t list -> t
val apply_edits_to_phys : t -> Edit.t list -> t

val reset_attempts : t -> t
val add_attempt : t -> value StringMap.t -> t
val attempts_to_string : t -> string
val num_attempts : t -> int

val add_cex : t -> Packet.t * Packet.t -> t

val reset_model_space : t -> t
val refine_model_space : t -> test -> t
val set_model_space : t -> test -> t

