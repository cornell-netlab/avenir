type t

val make :
     ?phys_drop_spec:Test.t option
  -> log:Cmd.t
  -> phys:Cmd.t
  -> fvs:(string * int) list
  -> log_inst:Instance.t
  -> phys_inst:Instance.t
  -> log_edits:Edit.t list
  -> unit
  -> t

val empty : t

val to_string : Parameters.t -> t -> string

val fvs : t -> (string * int) list

val cexs : t -> (Packet.t * Packet.t) list

val model_space : t -> Test.t

val attempts : t -> Model.t list

val add_cex : t -> Packet.t * Packet.t -> t

val log : t -> Cmd.t

val log_inst : t -> Instance.t

val log_edits : t -> Edit.t list

val log_edited_instance : Parameters.t -> t -> Instance.t

val log_gcl_program : Parameters.t -> t -> Cmd.t

val phys : t -> Cmd.t

val phys_inst : t -> Instance.t

val phys_edits : t -> Edit.t list

val phys_gcl_program : Parameters.t -> t -> Cmd.t

val phys_edited_instance : Parameters.t -> t -> Instance.t

val phys_gcl_holes :
  Parameters.t -> t -> Instance.interp -> [`Exact | `Mask] -> Cmd.t

val phys_drop_spec : t -> Test.t option

val slice : Parameters.t -> t -> t

val update_log : t -> Cmd.t -> t

val update_phys : t -> Cmd.t -> t

val empty_log_edits : t -> bool

val empty_phys_edits : t -> bool

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

val add_attempt : t -> Model.t -> t

val attempts_to_string : t -> string

val num_attempts : t -> int

val seen_attempt : t -> Model.t -> bool

val reset_model_space : t -> t

val refine_model_space : t -> Test.t -> t

val slice_conclusive : Parameters.t -> ProfData.t ref -> t -> bool

val step_search_state : t -> Edit.t list -> t
(** [step_search_state p es] commits the physical edits [es] and resets
    models and attempts *)

val negate_model_in_model_space : t -> Model.t -> Edit.t list -> t
