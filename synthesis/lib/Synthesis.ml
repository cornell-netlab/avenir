open Core
open Semantics
open Prover
open Util
open VCGen

let edit_cache = ref @@ EAbstr.make ()

let check_equivalence neg (params : Parameters.t) data problem =
  (* ensure default actions are used in constructing the equivalence condition *)
  let params = {params with no_defaults = false} in
  let log = Problem.log_gcl_program params problem in
  let phys = Problem.phys_gcl_program params problem in
  let condition = equivalent ~neg data Problem.(fvs problem) log phys in
  if params.vcache then
    check_valid_cached params condition |> fst
  else
    check_valid params condition |> fst

let implements ?neg:(neg = Test.True) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t)
    : (Packet.t * Packet.t) option =
  try_in_sequence [
      (fun _ ->
        fails_on_some_example params problem
      );
      (fun _ ->
        let open Option in
        check_equivalence neg params data problem
        >>| Packet.extract_inout_ce
      )
    ]

let handle_fast_cex neg (params : Parameters.t) data problem = function
  | `Yes ->
     Log.log params.debug "New rule is not reachable\n%!";
     None
  | `NotFound _ ->
     Log.log params.debug "No cex to be found rapidly, check full equivalence\n%!";
     implements ~neg params data problem
  | `NoAndCE counter ->
     Log.log params.debug "Found CX\n%!";
     Some counter

let slice_ok problem = not (Problem.empty_phys_edits problem)

let handle_sliced_equivalence neg problem params data = function
  | Some counter ->
     Some counter
  | None ->
     if Problem.slice_conclusive params data problem then
       None
     else
       implements ~neg params data problem

let get_cex ?neg:(neg=Test.True) (params : Parameters.t) (data :  ProfData.t ref) (problem : Problem.t) : (Packet.t * Packet.t) option =
  if params.fastcx then
    FastCX.get_cex ~neg params data problem
    |> handle_fast_cex neg params data problem
  else if params.do_slice && slice_ok problem then
    Problem.slice params problem
    |> implements ~neg params data
    |> handle_sliced_equivalence neg problem params data
  else
    implements ~neg params data problem

let rec minimize_edits params data problem certain uncertain =
  match uncertain with
  | [] -> certain
  | e::es ->
     match implements params data (Problem.replace_phys_edits problem (certain @ es)) with
     | None -> minimize_edits params data problem certain es
     | Some _ -> minimize_edits params data problem (certain@[e]) es

let minimize_solution (params : Parameters.t) data problem =
  if params.minimize then
    let () = Log.log params.debug "\tminimizing\n" in
    Problem.phys_edits problem
    |> minimize_edits params data problem []
    |> Problem.replace_phys_edits problem
  else
    problem

(* TODO Edit cache should abstract the fact that its a reference *)
let update_edit_cache (params : Parameters.t) problem : unit =
  if Option.is_none params.ecache then
    edit_cache :=
      EAbstr.update !edit_cache
        (Problem.log_edits problem |> List.hd_exn)
        (Problem.phys_edits problem)

let rec cegis_math (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) : (Edit.t list option) =
  Log.log params.debug "entering cegis\n%!";
  if Timeout.timed_out params.timeout then
    None
  else if Option.is_some params.ecache then
    let () = Log.log params.debug "\ttrying cache \n%!"in
    solve_math 1 params data problem
  else
    match get_cex params data problem with
    | None ->
       (* optionally minimize the solution*)
       let problem = minimize_solution params data problem in
       (* maintain the edit cache *)
       update_edit_cache params problem;
       Some (Problem.phys_edits problem)

    | Some (in_pkt, out_pkt) ->
       let params = {params with fastcx = false; ecache = None} in
       let problem = Problem.add_cex problem (in_pkt, out_pkt) in
       solve_math params.search_width params data problem

and solve_math (i : int) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  Log.log params.debug "solve_math\n%!";
  if Timeout.timed_out params.timeout
     || i = 0
     || List.length(Problem.phys_edits problem) > params.edits_depth
  then
    None
  else if Option.is_some params.ecache then
    try_cache params data problem
  else
    ModelFinder.make_searcher params data problem
    |> drive_search params.search_width params data problem

and drive_search (i : int) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) searcher =
  Log.log params.debug "loop\n%!";
  if Timeout.timed_out params.timeout
     || i = 0
  then
    None
  else
    let open Option.Let_syntax in
    let%bind (model,searcher) = ModelFinder.search params data problem searcher in
    let%bind es = EditSimpl.extract_reached_edits params data problem model in
    let problem' = Problem.step_search_state problem es in (* step down *)
    let problem  = Problem.negate_model_in_model_space problem model es in (*step side*)
    try_in_sequence [
        (fun _ -> cegis_math params data problem'); (* step down *)
        (fun _ -> drive_search (i - 1) params data problem searcher); (* try new heuristic*)
        (fun _ -> solve_math (i - 1) params data problem) (* backtrack *)
      ]

and try_cache params data problem =
  match EAbstr.infer params !edit_cache (Problem.phys problem) (Problem.log_edits problem |> List.hd_exn) with
  | None ->
     Log.edit_cache_miss params.debug;
     let params =
       {params with ecache = None;    (* caching failed so disable it *)
                    do_slice = false  (* dont slice.. I don't remember why not *)
       } in
     cegis_math params data problem

  | Some ps ->
     Log.edit_cache_hit params (Problem.phys problem) ps;
     (* fastCX's preconditions may be violated, so make sure its turned off*)
     let params_nofastcx = {params with fastcx = false} in
     (* add guessed edits to problem*)
     let problem_with_cache_guess = Problem.replace_phys_edits problem ps in
     (* try and get a CX to see if the problem works *)
     let did_cache_work = get_cex params_nofastcx data problem_with_cache_guess in
     match did_cache_work with
     | None ->
        Interactive.pause params.interactive ~prompt:"Caching succeeded";
        Some ps
     | Some (in_pkt,out_pkt) ->
        Log.cexs params problem in_pkt out_pkt;
        Interactive.pause params.interactive ~prompt:"Caching failed";
        let params = {params with ecache = None} in (* caching failed so disable it *)
        cegis_math params data problem

(* TODO -- this needs to be incorporated into [cegis_math], or does it? In some
   sense this can be managed at the command line by the user.*)
let manage_outer_heurs (params : Parameters.t) data problem = function
  | None ->
     if params.no_deletes then
       cegis_math {params with no_deletes = false} data problem
     else
       None
  | Some phys_edits ->
     Some phys_edits

let cegis_math_sequence_once (params : Parameters.t) data (problem : Problem.t) =
  let log_edit_sequence = Problem.log_edits problem in
  let problem = Problem.replace_log_edits problem [] in
  List.fold log_edit_sequence ~init:(Some(problem,[]))
    ~f:(fun acc ledit ->
      let open Option.Let_syntax in
      let%bind (problem, pedits) = acc in
      let problem = Problem.replace_log_edits problem [ledit] in
      let%bind phys_edits = cegis_math params data problem
                            |> manage_outer_heurs params data problem in
      Log.print_edits ~tab:false params (Problem.phys problem) phys_edits;
      Some (Problem.replace_phys_edits problem phys_edits
            |> Problem.commit_edits_log params
            |> Problem.commit_edits_phys params,
            pedits @ phys_edits)
    )

let cegis_math_sequence (params : Parameters.t) data (get_problem : unit -> Problem.t) =
  if params.hot_start then
    let open Option.Let_syntax in
    let st = Time.now () in
    let%bind _ = cegis_math_sequence_once params data (get_problem ()) in
    let () = Printf.eprintf "%f\n%!" Time.(now () |> Fn.flip diff st |> Span.to_ms) in
    cegis_math_sequence_once {params with hot_start = false} data (get_problem ())
  else
    (get_problem ())
    |> cegis_math_sequence_once params data
