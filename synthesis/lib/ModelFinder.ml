open Core
open Manip
open VCGen
open Util
open Prover

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

type t = {schedule: opts list; search_space: (Test.t * Model.t) list}

(* TODO (EHC) Will need for logging *)
(* let condcat b app s = if b then Printf.sprintf "%s %s" s app else s
 * 
 * let string_of_opts opts : string =
 *   condcat opts.injection "injection" " "
 *   |> condcat opts.hints "hints"
 *   |> condcat opts.paths "paths"
 *   |> condcat opts.only_holes "only_holes"
 *   |> condcat opts.mask "mask"
 *   |> condcat opts.restrict_mask "restrict_mask"
 *   |> condcat opts.nlp "NLP"
 *   |> condcat opts.annot "annotations"
 *   |> condcat opts.single "single"
 *   |> condcat opts.domain "domain"
 *   |> condcat opts.no_defaults "no_defaults"
 *   |> condcat opts.double "double"
 *   |> condcat opts.reachable_adds "reachable_adds "
 *   |> condcat opts.no_deletes "no_deletes" *)

let no_opts =
  { injection= false
  ; hints= false
  ; hint_type= `Vals
  ; paths= false
  ; only_holes= false
  ; mask= false
  ; restrict_mask= false
  ; nlp= false
  ; annot= false
  ; single= false
  ; domain= false
  ; no_defaults= false
  ; no_deletes= false
  ; double= false
  ; reachable_adds= false
  ; restr_acts= false }

(* None > Mask > Paths > Injection > Hints > Only_Holes *)
let rec make_schedule opt =
  opt
  ::
  ( if opt.double then
    let opt' = {opt with double= false} in
    make_schedule opt'
  else if opt.restr_acts then make_schedule {opt with restr_acts= false}
  else if
  opt.injection || opt.hints || opt.paths || opt.only_holes || opt.nlp
  || opt.domain
then
    let opt' =
      { opt with
        injection= false
      ; hints= false
      ; paths= false
      ; only_holes= false
      ; nlp= false
      ; domain= false }
    in
    make_schedule opt'
  else if opt.no_defaults then
    let opt' = {opt with no_defaults= false} in
    make_schedule opt'
  else [] )

let extract_hint_type (params : Parameters.t) =
  if String.(params.hint_type = "mask") then `NoVals
  else if String.(params.hint_type = "exact") then `Vals
  else
    Printf.sprintf
      "Unrecognized hint type %s, expected \"mask\" or \"exact\""
      params.hint_type
    |> failwith

let make_searcher (params : Parameters.t) : t =
  let schedule =
    make_schedule
      { injection= params.injection
      ; hints= params.hints
      ; hint_type= extract_hint_type params
      ; paths= params.monotonic
      ; only_holes= params.only_holes
      ; mask= params.widening
      ; restrict_mask= params.restrict_mask
      ; annot= params.allow_annotations
      ; nlp= params.nlp
      ; single= params.unique_edits
      ; domain= params.domain
      ; no_defaults= params.no_defaults
      ; no_deletes= params.no_deletes
      ; double= params.use_all_cexs
      ; reachable_adds= params.reach_restrict
      ; restr_acts= params.restr_acts }
  in
  {schedule; search_space= []}

(** [reindex_for_dels problem tbl i] adjusts a deletion index for row [i] of
    table [tbl] in [Problem.phys_inst problem], adjusting for any deletion
    operations in [Problem.phys_edits problem]. If the deletion indices
    conflict, returns [None] .*)
let reindex_for_dels problem tbl i =
  Problem.phys_edits problem
  |> List.fold ~init:(Some i) ~f:(fun cnt edit ->
         match (cnt, edit) with
         | Some n, Del (t, j) when String.(t = tbl) ->
             if i = j then None else Some (n - 1)
         | _ -> cnt)

(** [get_deletes opts problem] constructs a list of [(x,idx)], where [x] is a
    table name and [i] is a row to delete. If [opts.no_deletes] is [true],
    returns [\[\]]. *)
let get_deletes opts (problem : Problem.t) =
  if opts.no_deletes then []
  else
    let phys_inst = Problem.phys_inst problem in
    Instance.fold phys_inst ~init:[] ~f:(fun ~table ~rows dels ->
        dels
        @ List.filter_mapi rows ~f:(fun i _ ->
              match reindex_for_dels problem table i with
              | None -> None
              | Some i' -> Some (table, i')))

(** [get_hints opts problem] constructs a list of [Hint.t]s to apply to the
    problem. Returns the empty list if [opts.hints] is [false]*)
let get_hints opts problem =
  if opts.hints then
    let open Problem in
    (* Log.print_edits (log_edits problem); *)
    Hint.construct (phys problem) (log_edits problem |> List.hd_exn)
  else []

(** [get_hole_protocol opts dels hints] constructs the hole-instrumentation
    protocol identifier (c.f. [Instance.interp]) *)
let get_hole_protocol opts deletions hints =
  if opts.only_holes then Instance.OnlyHoles hints
  else Instance.WithHoles (deletions, hints)

(** [get_hole_type opts] extracts the type of holes from [opts]*)
let get_hole_type opts = if opts.mask then `Mask else `Exact

(* TODO Commented out because slowed things down, why? *)
(* let well_formed_adds (params : Parameters.t) problem encode_tag =
 *   let open Test in
 *   let phys_inst = Problem.phys_edited_instance params problem in
 *   let phys = Problem.phys problem in
 *   Cmd.tables phys
 *   |> concatMap ~c:and_ ~init:(Some True) ~f:(fun t ->
 *          let t_rows = Instance.get_rows phys_inst t in
 *          Hole.add_row_hole t
 *          %=% Expr.value (1, 1)
 *          %=>% concatMap t_rows ~init:(Some True) ~c:( %&% )
 *                 ~f:(fun (ms, _, _) ->
 *                   !%(concatMap ms ~init:(Some False) ~c:( %&% )
 *                        ~f:(Match.to_valuation_test t encode_tag)))) *)

(** [adds_are_reachable params problem opts fvs encode_tag] computes a
    condition that ensures that all synthesized insertions into any table are
    reachable by some packet, if [opts.reachable_adds] is [true]. Otherwise
    it returns [Test.True] *)
let adds_are_reachable params (problem : Problem.t) (opts : opts) fvs
    encode_tag =
  let open Test in
  if not opts.reachable_adds then True
  else
    let phys = Problem.phys problem in
    let in_pkt = Problem.cexs problem |> List.hd_exn |> fst in
    Cmd.get_tables_keys phys
    |> List.fold ~init:True ~f:(fun acc (tbl_name, keys) ->
           and_ acc
           @@ impl (Hole.add_row_hole tbl_name %=% Expr.value (1, 1))
           @@ FastCX.is_reachable encode_tag params problem fvs in_pkt
                tbl_name keys)

(** [non_empty_adds problem] computes a condition ensuring that at least one
    table in [Problem.phys problem] is added to *)
let non_empty_adds (problem : Problem.t) =
  let open Test in
  Problem.phys problem |> Cmd.tables
  |> List.fold ~init:None ~f:(fun acc tbl ->
         match acc with
         | None -> Some (Hole.add_row_hole tbl %=% Expr.value (1, 1))
         | Some acc ->
             Some (acc %+% (Hole.add_row_hole tbl %=% Expr.value (1, 1))))
  |> Option.value ~default:True

(** [single problem opts query_holes] computes, if [opts.single] is [true], a
    condition that only one row may be added to a table in
    [Problem.phys problem], including already-added rows in
    [Problem.phys_edits problem], otherwise, if [opts.single] is [false], is
    [Test.true] *)
let single problem (opts : opts) query_holes =
  let open Test in
  if not opts.single then True
  else
    List.fold query_holes ~init:True ~f:(fun acc (h, sz) ->
        and_ acc
          ( if
            Hole.is_add_row_hole h
            && List.exists (Problem.phys_edits problem) ~f:(fun e ->
                   String.(
                     Edit.table e
                     = String.chop_prefix_exn h ~prefix:Hole.add_row_prefix))
          then Hole (h, sz) %=% Expr.value (0, sz)
          else acc ))

(** [restrict_mask opts query_holes] computes, if [opts.restrict_mask] is
    [true], a condition enforcing that every mask hole in [query_holes] is
    [0] or all [1]s *)
let restrict_mask (opts : opts) query_holes =
  let open Test in
  if not opts.restrict_mask then True
  else
    List.fold query_holes ~init:True ~f:(fun acc (h, sz) ->
        and_ acc
        @@
        if String.is_suffix h ~suffix:"_mask" then
          let h_value = String.chop_suffix_exn h ~suffix:"_mask" in
          let all_1s = max_int sz in
          and_ acc
          @@ bigor
               [ Hole (h, sz) %=% Value (Value.big_make (all_1s, sz))
               ; bigand
                   [ Hole (h_value, sz) %=% Expr.value (0, sz)
                   ; Hole (h, sz) %=% Expr.value (0, sz) ] ]
        else True)

(** [active_domain_restrict params problem opts query_holes] computes, if
    [opts.restrict_mask] is [true], a condition enforcing that every data
    hole in [query_holes] can be found as an equally-sized value in
    [Problem.log_gcl_program program] or [Problem.phys_gcl_program program],
    or is [0] or [1], Otherwise, if [opts.restrict_mask] is false, it returns
    [true]*)
let active_domain_restrict params problem opts query_holes : Test.t =
  let open Test in
  if not opts.domain then True
  else
    let ints =
      Cmd.multi_vals (Problem.log_gcl_program params problem)
      @ Cmd.multi_vals (Problem.phys_gcl_program params problem)
      |> List.dedup_and_sort ~compare:Stdlib.compare
      |> List.filter ~f:(fun v ->
             Bigint.(Value.get_bigint v <> zero && Value.get_bigint v <> one))
    in
    List.fold query_holes ~init:True ~f:(fun acc (h, sz) ->
        let restr =
          List.fold ints ~init:False ~f:(fun acci v ->
              let szi = Value.size v in
              let i = Value.get_bigint v in
              or_ acci
              @@
              if
                sz = szi
                && (not (String.is_suffix h ~suffix:"_mask"))
                && (not (Hole.is_add_row_hole h))
                && (not (Hole.is_delete_hole h))
                && not (Hole.is_which_act_hole h)
              then
                Hole (h, sz)
                %=% Value (Value.big_make (i, szi))
                %+% (Hole (h, sz) %=% Expr.value (0, szi))
                %+% (Hole (h, sz) %=% Expr.value (1, szi))
              else False)
        in
        if Test.equals restr False then acc else acc %&% restr)

(** [no_defaults problem opts fvs phys] computes a condition that is [True]
    if [opt.no_defaults] is [False], and otherwise ensures that no data hole
    in [phys] (i.e. key or action data) is [0] *)
let no_defaults (_ : Parameters.t) opts fvs phys =
  let open Test in
  if not opts.no_defaults then True
  else
    List.filter (Cmd.holes phys) ~f:(fun (v, _) ->
        List.for_all fvs ~f:(fun (v', _) ->
            List.exists
              [ v'
              ; Hole.add_row_prefix
              ; Hole.delete_row_prefix
              ; Hole.which_act_prefix ] ~f:(fun substring ->
                String.is_substring v ~substring)
            |> not))
    |> List.fold ~init:True ~f:(fun acc (v, sz) ->
           acc %&% (Hole (v, sz) %<>% Expr.value (0, sz)))

let flows_to (phys : Cmd.t) (tgts : StringSet.t) (src : string) =
  let open StringSet in
  Log.debug @@ lazy "[flows_to]" ;
  Log.debug @@ lazy (Printf.sprintf "Targeting %s" (string_of_strset tgts)) ;
  let flows = StaticSlicing.flows_to (StringSet.singleton src) phys in
  Log.debug @@ lazy (Printf.sprintf "%s -> %s" src (string_of_strset flows)) ;
  exists tgts ~f:(mem flows)

(** [unlikely_actions opts fvs phys cex] disables actions that do not
    progress towards solving the cex. Specifically, those that make no
    assignments to the changed variables in the cex. *)
let unlikely_actions opts fvs phys (inpkt, outpkt) =
  let open Test in
  if not opts.restr_acts then True
  else
    let () = Log.debug @@ lazy (Printf.sprintf "[unlikely_actions]") in
    let raw_diffs = Packet.diff_vars inpkt outpkt |> StringSet.of_list in
    let diffs = StringSet.(of_list fvs |> inter raw_diffs) in
    let ta_map = Cmd.get_tables_actions phys in
    List.fold ta_map ~init:True ~f:(fun acc (table, acts) ->
        let phys = StaticSlicing.truncate ~dir:`Fwd phys table in
        Log.debug @@ lazy (Cmd.to_string phys) ;
        let actSize = max (log2 (List.length acts)) 1 in
        Log.debug @@ lazy (Printf.sprintf "checking table %s" table) ;
        List.foldi acts ~init:acc ~f:(fun i acc (_, _, act) ->
            if
              StringSet.exists (Cmd.assigned_vars act)
                ~f:(flows_to phys diffs)
            then
              let () =
                lazy (Printf.sprintf "ok to add table %s act %d" table i)
                |> Log.debug
              in
              acc
            else
              let () =
                lazy (Printf.sprintf "dont add to table %s act %d" table i)
                |> Log.debug
              in
              !%(bigand
                   [ Hole.add_row_hole table %=% Expr.value (1, 1)
                   ; Hole.which_act_hole table actSize
                     %=% Expr.value (i, actSize) ])
              |> and_ acc))

(** [test_of_cexs fvs cex] aggregates an input-output list of
    counterexamples, into a pair [(intest, outest)], where [intest] is the
    disjunction of the input packets in [cexs] and [outest] is the
    disjunction of the output packets *)
let tests_of_cexs fvs cexs =
  let open Test in
  List.fold cexs ~init:(False, False) ~f:(fun (ins, outs) (inp, outp) ->
      (ins %+% Packet.to_test ~fvs inp, outs %+% Packet.to_test ~fvs outp))

(** [no_defaults problem fvs sub good cexs] is [True] when [opts.double] is
    [false] and otherwise is a computes a hoare triple on the passive program
    [good], using the varible indexer [sub] such that leverages every
    counterexample packet, restricted to the variables [fvs]. *)
let use_past_cexs opts fvs sub good cexs =
  if opts.double then
    let in_test, out_test = tests_of_cexs fvs cexs in
    hoare_triple_passified sub in_test good out_test
  else True

(** [apply_heurs params problem opts phys query_test partial_model]
    conditionally applies the heuristics (as determined by [opts]), on the
    test [query_test] constructed from the instrumented program [phys],
    applying [partial_model] to each optimization *)
let apply_heurs params problem opts phys query_test partial_model =
  let open Test in
  let hole_type = get_hole_type opts in
  let fvs = Problem.fvs problem in
  let cex = Problem.cexs problem |> List.hd_exn in
  let query_holes =
    Test.holes query_test |> List.dedup_and_sort ~compare:Stdlib.compare
  in
  (* TODO (ehc) :: can we just call fixup_test partial_model after?*)
  bigand
    [ query_test |> fixup_test partial_model
    ; adds_are_reachable params problem opts fvs hole_type
      |> fixup_test partial_model
    ; restrict_mask opts query_holes |> fixup_test partial_model
    ; no_defaults params opts fvs phys |> fixup_test partial_model
    ; single problem opts query_holes |> fixup_test partial_model
    ; active_domain_restrict params problem opts query_holes
      |> fixup_test partial_model
    ; unlikely_actions opts (fsts fvs) (Problem.phys problem) cex
      |> fixup_test partial_model
    ; non_empty_adds problem |> fixup_test partial_model ]

(** [action_hole_valid tbl num_acts] ensures that the hole index for table
    [tb] doesn't surpass [num_acts] *)
let action_hole_valid tbl num_acts =
  let open Test in
  Hole.which_act_hole tbl (max (log2 num_acts) 1)
  %<=% Expr.value (max (num_acts - 1) 0, max (log2 num_acts) 1)

(** [well_formed_actions problem] ensures that the hole index for each table
    in [Problem.phys problem] doesn't surpass the number of actions defined
    for the table*)
let well_formed_actions problem =
  let open Test in
  Problem.phys problem |> Cmd.get_tables_actsizes
  |> List.fold ~init:True ~f:(fun acc (tbl, num_acts) ->
         acc %&% action_hole_valid tbl num_acts)

let construct_model_query opts fvs cexs in_pkt phys out_pkt =
  let open Test in
  let sub, passive_phys = passify fvs phys in
  let phys' = CompilerOpts.optimize fvs passive_phys in
  let good = good_wp phys' in
  let past = use_past_cexs opts fvs sub good cexs in
  let intest, outest = pair_map ~f:(Packet.to_test ~fvs) (in_pkt, out_pkt) in
  let curr = hoare_triple_passified sub intest good outest in
  past %&% curr

(** [instr_phys params problem opts] is [hints, iphys] where [iphys] is the
    [Problem.phys_gcl_program problem] instrumented with holes according to
    the strategy specified by [opts], and [hints] are the hints used in the
    instrumenation.*)
let instr_phys (params : Parameters.t) problem opts =
  let hints = get_hints opts problem in
  let deletes = get_deletes opts problem in
  let hole_protocol = get_hole_protocol opts deletes hints in
  let hole_type = get_hole_type opts in
  (* TODO (EHC) Why is this here? *)
  let params = {params with no_defaults= opts.no_defaults} in
  (hints, Problem.phys_gcl_holes params problem hole_protocol hole_type)

(** [compute_vs params data problem opts] computes the verification condition
    for the (most-recent) counterexample(s) in [Problem.cexs params]
    according to *)
let compute_vc (params : Parameters.t) (_ : ProfData.t ref)
    (problem : Problem.t) (opts : opts) =
  let in_pkt, out_pkt = Problem.cexs problem |> List.hd_exn in
  let hints, phys = instr_phys params problem opts in
  let fvs = inter (Cmd.vars phys) (Problem.fvs problem) ~f:Stdlib.( = ) in
  let query =
    Test.bigand
      [ construct_model_query opts fvs (Problem.cexs problem) in_pkt phys
          out_pkt ]
  in
  (query, phys, hints)

(** [with_opts params problems opts (query,phys,hints)] applies the
    optimizations indicated by [opts], and the [hints] to the query [query],
    such that [query] and [hints] are derived from [phys] *)
let with_opts (params : Parameters.t) (problem : Problem.t) (opts : opts)
    (query, phys, hints) : (Test.t * Model.t) list =
  let open Test in
  if Test.equals query False || not (Test.has_hole query) then []
  else
    let wf_acts = well_formed_actions problem in
    let query_test =
      Problem.model_space problem
      |> and_ query
      |> Injection.optimization
           {params with injection= opts.injection}
           problem
      |> and_ wf_acts
    in
    let partial_model =
      Hint.list_to_model opts.hint_type (Problem.phys problem) hints
    in
    let out_test =
      apply_heurs params problem opts phys query_test partial_model
    in
    [(out_test, partial_model)]

(** [compute_queryd params data problems opts] computes the a list of
    optimized (accdg to [opts]) queries & the partial models corresponding to
    the hints in optimizing the query *)
let compute_query (params : Parameters.t) (data : ProfData.t ref)
    (problem : Problem.t) (opts : opts) : (Test.t * Model.t) list =
  compute_vc params data problem opts |> with_opts params problem opts

let rec search (params : Parameters.t) data problem t =
  if Timeout.timed_out params.timeout then None
  else
    match (t.search_space, t.schedule) with
    | [], [] -> None
    | [], opts :: schedule ->
        let search_space = compute_query params data problem opts in
        search params data problem {schedule; search_space}
    | (test, partial_model) :: search_space, schedule -> (
        let model_opt, _ =
          Problem.model_space problem
          |> Test.and_ test |> fixup_test partial_model |> check_sat params
        in
        match model_opt with
        | Some raw_model -> Some (Model.join partial_model raw_model, t)
        | _ -> search params data problem {schedule; search_space} )
