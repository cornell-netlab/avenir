(*** [extract_reached_edits params data problem m] computes [es_opt] by first
  extracting the edist from [m]. Then if [params.reach_filter] is true, it
  removes the edits that are not reachable. If there are no reachable edits
  it returns [None]. In other words, [es_opt] is [None] or [Some es] and [es]
  is nonempty *)
val extract_reached_edits :
     Parameters.t
  -> ProfData.t ref
  -> Problem.t
  -> Model.t
  -> Edit.t list option
