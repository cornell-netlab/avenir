open Core
open Semantics
open Prover
open Util
open VCGen

let check_equivalence neg (params : Parameters.t) data problem =
  (* ensure default actions are used in constructing the equivalence
     condition *)
  let params = {params with no_defaults= false} in
  let log = Problem.log_gcl_program params problem in
  let phys = Problem.phys_gcl_program params problem in
  let condition = equivalent ~neg data Problem.(fvs problem) log phys in
  if params.vcache then check_valid_cached params condition |> fst
  else check_valid params condition |> fst

let implements ?(neg = Test.True) (params : Parameters.t)
    (data : ProfData.t ref) (problem : Problem.t) :
    (Packet.t * Packet.t) option =
  try_in_sequence
    [ (fun _ -> fails_on_some_example params problem)
    ; (fun _ ->
        let open Option in
        check_equivalence neg params data problem >>| Packet.extract_inout_ce)
    ]

let handle_fast_cex neg (params : Parameters.t) data problem = function
  | `Yes ->
      Log.info (lazy "New rule is not reachable\n%!") ;
      None
  | `NotFound _ -> implements ~neg params data problem
  | `NoAndCE counter -> Some counter

let slice_ok problem = not (Problem.empty_phys_edits problem)

let handle_sliced_equivalence neg problem params data = function
  | Some counter -> Some counter
  | None ->
      if Problem.slice_conclusive params data problem then None
      else implements ~neg params data problem

let log_cex_str inpkt outpkt =
  Printf.sprintf "log :%s\n" (Packet.cex_to_string (inpkt, outpkt))

let phys_cex_str params problem inpkt =
  let phys = Problem.phys_gcl_program params problem in
  Printf.sprintf "phys :%s\n"
    (Packet.cex_to_string (inpkt, eval_act phys inpkt))

let normalize_cex params (problem : Problem.t) cex =
  let inpkt, outpkt =
    Util.pair_map ~f:(Packet.restrict (Problem.fvs problem)) cex
  in
  Log.debug @@ lazy (log_cex_str inpkt outpkt) ;
  Log.debug @@ lazy (phys_cex_str params problem inpkt) ;
  (inpkt, outpkt)

let eq_str params problem =
  Printf.sprintf
    "-------------------------------------------\n\
     %s \n\
     ???====?=====????\n\
    \ %s\n\
     -------------------------------------\n\
     %!"
    (Cmd.to_string (Problem.log_gcl_program params problem))
    (Cmd.to_string (Problem.phys_gcl_program params problem))

let get_cex ?(neg = Test.True) (params : Parameters.t)
    (data : ProfData.t ref) (problem : Problem.t) :
    (Packet.t * Packet.t) option =
  Log.debug @@ lazy (eq_str params problem) ;
  let open Option in
  if params.fastcx then
    FastCX.get_cex ~neg params data problem
    |> handle_fast_cex neg params data problem
    >>| normalize_cex params problem
  else if params.do_slice && slice_ok problem then
    Problem.slice params problem
    |> implements ~neg params data
    |> handle_sliced_equivalence neg problem params data
    >>| normalize_cex params problem
  else implements ~neg params data problem >>| normalize_cex params problem

let rec minimize_edits params data problem certain uncertain =
  match uncertain with
  | [] -> certain
  | e :: es -> (
    match
      implements params data
        (Problem.replace_phys_edits problem (certain @ es))
    with
    | None -> minimize_edits params data problem certain es
    | Some _ -> minimize_edits params data problem (certain @ [e]) es )

let minimize_solution (params : Parameters.t) data problem =
  if params.minimize then (
    Log.debug @@ lazy "minimizing" ;
    Problem.phys_edits problem
    |> minimize_edits params data problem []
    |> Problem.replace_phys_edits problem )
  else problem

(* TODO Edit cache should abstract the fact that its a reference *)
let update_edit_cache (params : Parameters.t) problem : unit =
  if Option.is_none params.ecache then
    EAbstr.update
      (Problem.log_edits problem |> List.hd_exn)
      (Problem.phys_edits problem)

let rec cegis_math (params : Parameters.t) (data : ProfData.t ref)
    (problem : Problem.t) : Edit.t list option =
  Log.info @@ lazy "entering cegis\n" ;
  if Timeout.timed_out params.timeout then None
  else if Option.is_some params.ecache then solve_math 1 params data problem
  else
    match get_cex params data problem with
    | None ->
        (* optionally minimize the solution*)
        let problem = minimize_solution params data problem in
        (* maintain the edit cache *)
        update_edit_cache params problem ;
        Some (Problem.phys_edits problem)
    | Some (in_pkt, out_pkt) ->
        let params = {params with fastcx= false; ecache= None} in
        let problem = Problem.add_cex problem (in_pkt, out_pkt) in
        solve_math params.search_width params data problem

and solve_math (i : int) (params : Parameters.t) (data : ProfData.t ref)
    (problem : Problem.t) =
  Log.info @@ lazy "solve_math" ;
  if
    Timeout.timed_out params.timeout
    || i = 0
    || List.length (Problem.phys_edits problem) > params.edits_depth
  then None
  else if Option.is_some params.ecache then try_cache params data problem
  else
    ModelFinder.make_searcher params
    |> drive_search params.search_width params data problem

and drive_search (i : int) (params : Parameters.t) (data : ProfData.t ref)
    (problem : Problem.t) searcher =
  Log.info @@ lazy "search" ;
  if Timeout.timed_out params.timeout || i = 0 then None
  else
    let open Option.Let_syntax in
    let%bind model, searcher =
      ModelFinder.search params data problem searcher
    in
    let%bind es =
      EditSimpl.extract_reached_edits params data problem model
    in
    let problem' = Problem.step_search_state problem es in
    (* step down *)
    let problem = Problem.negate_model_in_model_space problem model es in
    (*step side*)
    try_in_sequence
      [ (fun _ -> cegis_math params data problem')
      ; (* step down *)
        (fun _ -> drive_search (i - 1) params data problem searcher)
      ; (* try new heuristic*)
        (fun _ -> solve_math (i - 1) params data problem)
        (* backtrack *) ]

and try_cache params data problem =
  Log.info @@ lazy "try_cache" ;
  match
    EAbstr.infer params (Problem.phys problem)
      (Problem.log_edits problem |> List.hd_exn)
  with
  | None ->
      Log.info @@ lazy "cache_missed" ;
      let params =
        { params with
          ecache= None (* caching failed so disable it *)
        ; do_slice= false (* dont slice.. I don't remember why not *) }
      in
      cegis_math params data problem
  | Some ps -> (
      Log.info @@ lazy "cache hit" ;
      Log.debug @@ lazy (Edit.list_to_string ps) ;
      (* fastCX's preconditions may be violated, so make sure its turned off*)
      let params_nofastcx = {params with fastcx= false} in
      (* add guessed edits to problem*)
      let problem_with_cache_guess = Problem.replace_phys_edits problem ps in
      (* try and get a CX to see if the problem works *)
      let did_cache_work =
        get_cex params_nofastcx data problem_with_cache_guess
      in
      match did_cache_work with
      | None ->
          Log.info @@ lazy "Cache succeeded" ;
          Some ps
      | Some cex ->
          Log.info @@ lazy "Cache failed" ;
          Log.debug @@ lazy (Packet.cex_to_string cex) ;
          Log.debug @@ lazy "discarding cex and backtracking" ;
          let params = {params with ecache= None} in
          cegis_math params data problem )

(* TODO -- this needs to be incorporated into [cegis_math], or does it? In
   some sense this can be managed at the command line by the user.*)
let manage_outer_heurs (params : Parameters.t) data problem = function
  | None ->
      if params.no_deletes then
        cegis_math {params with no_deletes= false} data problem
      else None
  | Some phys_edits -> Some phys_edits

let cegis_math_sequence_once (params : Parameters.t) data
    (problem : Problem.t) =
  let log_edit_sequence = Problem.log_edits problem in
  let problem = Problem.replace_log_edits problem [] in
  List.fold log_edit_sequence
    ~init:(Some (problem, []))
    ~f:(fun acc ledit ->
      let open Option.Let_syntax in
      let%bind problem, pedits = acc in
      let problem = Problem.replace_log_edits problem [ledit] in
      let%map phys_edits =
        cegis_math params data problem
        |> manage_outer_heurs params data problem
      in
      ( Problem.replace_phys_edits problem phys_edits
        |> Problem.commit_edits_log params
        |> Problem.commit_edits_phys params
      , pedits @ phys_edits ))

let cegis_math_sequence (params : Parameters.t) data
    (get_problem : unit -> Problem.t) =
  if params.hot_start then
    let open Option.Let_syntax in
    let st = Time.now () in
    let%bind _ = cegis_math_sequence_once params data (get_problem ()) in
    let () =
      Printf.eprintf "%f\n%!" Time.(now () |> Fn.flip diff st |> Span.to_ms)
    in
    cegis_math_sequence_once
      {params with hot_start= false}
      data (get_problem ())
  else get_problem () |> cegis_math_sequence_once params data
