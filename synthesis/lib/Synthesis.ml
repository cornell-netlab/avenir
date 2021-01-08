open Core
open Ast
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

let implements ?neg:(neg = True) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t)
    : (Packet.t * Packet.t) option =
  try_in_sequence [
      (fun _ -> fails_on_some_example params problem);
      (fun _ -> let open Option in
                check_equivalence neg params data problem
                >>| Packet.extract_inout_ce)
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


let get_cex ?neg:(neg=True) (params : Parameters.t) (data :  ProfData.t ref) (problem : Problem.t) : (Packet.t * Packet.t) option =
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
     (* Printf.printf "\t%s" (Edit.to_string e); *)
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

let rec cegis_math (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) : (Edit.t list option) =
  Log.log params.debug "cegis_math\n%!";
  Log.print_problem params problem;
  (* Printf.printf "%s\n%!" (List.hd_exn (Problem.log_edits problem) |> Edit.to_string); *)
  if Timeout.timed_out params.timeout then
    None
  else
    if Option.is_some params.ecache then
      let () = Log.log params.debug "\ttrying cache \n%!"in
      solve_math 1 params data problem
    else
      match get_cex params data problem with
      | None ->
         (* optionally minimize the solution*)
         let problem = minimize_solution params data problem in

         (* if we didn't use the edit cache in the solution, update it *)
         if Option.is_none params.ecache then
           edit_cache := EAbstr.update !edit_cache
                           (Problem.log_edits problem |> List.hd_exn)
                           (Problem.phys_edits problem);

         (*Pause execution and wait for operator's ok to continue*)
         Interactive.pause params.interactive ~prompt:("correct!");

         Some (Problem.phys_edits problem)

      | Some (in_pkt, _) ->
         let log_out_pkt = Semantics.eval_act (Problem.log_gcl_program params problem) in_pkt in
         let params = {params with fastcx = false; ecache = None} in
         let problem = Problem.add_cex problem (in_pkt, log_out_pkt) in
         Log.cexs params problem log_out_pkt in_pkt;
         solve_math params.search_width params data problem

and solve_math (i : int) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  Log.log params.debug "solve_math\n%!";
  if Timeout.timed_out params.timeout || i = 0 then
    None
  else
    if Option.is_some params.ecache then
      try_cache params data problem
    else
      List.length(Problem.phys_edits problem) <= params.edits_depth
      |=> fun _ ->
          ModelFinder.make_searcher params data problem
          |> drive_search params.search_width params data problem

and drive_search (i : int) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) searcher =
  Log.log params.debug"loop\n%!";
  let open Option in
  if Timeout.timed_out params.timeout then begin
      Printf.printf "Timeout\n%!";
      None
    end
  else if i = 0 then begin
      Log.log params.debug "Out of gas\n%!";
      None
    end
  else
    let st = Time.now () in
    let model_opt = ModelFinder.search params data problem searcher in
    ProfData.update_time !data.model_search_time st;
    model_opt >>= fun (model, searcher) ->
    let es = EditSimpl.extract_reached_edits params data problem model in

    Log.print_search_state params problem es model;
    Interactive.pause ~prompt:"\n" params.interactive;

    let problem' = Problem.(append_phys_edits problem es
                            |> reset_model_space
                            |> reset_attempts) in

    let problem = Problem.add_attempt problem model in
    let restriction = Edit.negate (Problem.phys problem) model es in
    let problem = Problem.refine_model_space problem restriction in

    Log.check_attempts params.debug problem;

    not(List.is_empty es)
    |=> fun _ ->
        try_in_sequence [
            (fun _ -> cegis_math params data problem');
            (fun _ -> Log.log params.debug "Those edits were wrong, backtracking \n%!";
                      Interactive.pause params.interactive;
                      drive_search (i - 1) params data problem searcher);
            (fun _ -> ProfData.incr !data.num_backtracks;
                      solve_math (i - 1) params data problem)
          ]

and try_cache params data problem =
  match EAbstr.infer params !edit_cache (Problem.phys problem) (Problem.log_edits problem |> List.hd_exn) with
  | None ->
     Log.edit_cache_miss params.debug;
     let params =
       {params with ecache = None;   (* caching failed so disable it *)
                    do_slice = false  (* dont slice.. I don't remember why not *)
       } in
     cegis_math params data problem
  | Some ps ->
     Log.edit_cache_hit params (Problem.phys problem) ps;

     (* fastCX's preconditions may be violated, so make sure its turned off*)
     let params_nofastcx_with_slicing = {params with fastcx = false} in

     (* add guessed edits to problem*)
     let problem_with_cache_guess = Problem.replace_phys_edits problem ps in

     (* try and get a CX to see if the problem works *)
     let did_cache_work = get_cex params_nofastcx_with_slicing data problem_with_cache_guess in

     match did_cache_work with
     | None ->
        Interactive.pause params.interactive ~prompt:"Caching succeeded";
        Some ps
     | Some (in_pkt,out_pkt) ->
        Log.cexs params problem in_pkt out_pkt;
        Interactive.pause params.interactive ~prompt:"Caching failed";
        let params = {params with ecache = None} in (* caching failed so disable it *)
        cegis_math params data problem


let rec cegis_math_sequence (params : Parameters.t) data get_problem =
  let t = Time.now() in
  let initial_problem = get_problem () in
  let log_edit_sequence = Problem.log_edits initial_problem in
  let problem = Problem.replace_log_edits initial_problem [] in
  List.fold log_edit_sequence ~init:(Some(problem,[]))
    ~f:(fun acc ledit ->
      match acc with
      | None ->
         Log.log true "Rule failed";
         None
      | Some (problem, pedits) ->
         let problem = Problem.replace_log_edits problem [ledit] in
         Printf.sprintf "\n\n\n%s\n\n\n" (Problem.to_string params problem)
         |> Log.log params.debug;
         let phys_edits = match cegis_math params data problem with
           | None ->
              if params.no_deletes then
                cegis_math {params with no_deletes = false} data problem
              else
                None
           | Some phys_edits ->
              Some phys_edits
         in
         match phys_edits with
         | None ->
            Printf.printf "Couldn't find solution for:\n%!";
            Log.print_edits params (Problem.log problem) [ledit];
            None
         | Some phys_edits ->
            Log.print_edits ~tab:false params (Problem.phys problem) phys_edits;
            Some (Problem.replace_phys_edits problem phys_edits
                  |> Problem.commit_edits_log params
                  |> Problem.commit_edits_phys params,
                  pedits @ phys_edits)
    )
  |> Option.bind ~f:(fun p ->
         if params.hot_start then
           let () = Printf.eprintf "%f\n%!" Time.(now () |> Fn.flip diff t |> Span.to_ms) in
           cegis_math_sequence {params with hot_start = false} data get_problem
         else
           Some p
       )
