(** Represents the search state -- the schedule of optimizations and the
    queries to try from them*)
type t

val make_searcher : Parameters.t -> t
(** [make_searcher p] is search state with a full optimization schedule
    determined by [p], but no queries yet *)

val search :
  Parameters.t -> ProfData.t ref -> Problem.t -> t -> (Model.t * t) option
(** [search params data problem space] searches through the [space] until
    model is found, when it returns a pair of model and the the new state. If
    the search space is exhausted before a model is found, return [None] *)

(** Represents the optimizations used on any given query. EXFILTRATED FOR
    TESTING, DO NOT USE*)
type opts =
  { injection: bool
  ; hints: bool
  ; hint_type: [`Vals | `NoVals]
  ; paths: bool (* DEPRECATED *)
  ; only_holes: bool
  ; mask: bool
  ; restrict_mask: bool
  ; nlp: bool
  ; annot: bool
  ; single: bool
  ; domain: bool
  ; no_defaults: bool
  ; no_deletes: bool
  ; double: bool
  ; reachable_adds: bool
  ; restr_acts: bool }

val no_opts : opts
(** [no_opts] represents all optimizations being disabled. EXFILTRATED FOR
    TESTING, DO NOT USE. *)

val construct_model_query :
     opts
  -> (string * int) list
  -> (Packet.t * Packet.t) list
  -> Packet.t
  -> Cmd.t
  -> Packet.t
  -> Test.t
(** [construct_model_query opts fvs cexs in_pkt phys out_pkt] constructs a
    verification condition on an instrumented program [phys] to ensure that
    [phys(in_pkt|_fvs)|_fvs = outpkt|_fvs]. if [opts.double] is true, then it
    conjoins a disjunction of similar queries for all of the counterexamples
    we've seen so far. EXFILTRATED FOR TESTING, DO NOT USE *)
