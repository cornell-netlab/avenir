open Core
open Ast
open Packet
open Semantics
open Prover
open Manip
open Util
open VCGen
open Tables

let symbolic_pkt fvs =
  List.fold fvs ~init:True
    ~f:(fun acc_test (var,sz) ->
        if String.get var 0 |> Char.is_uppercase
        || String.substr_index var ~pattern:("NEW") |> Option.is_some
        then acc_test
        else
          Var (var,sz) %=% Var (symbolize var, sz)
          %&% acc_test)

let symb_wp ?fvs:(fvs=[]) cmd =
  List.dedup_and_sort ~compare:Stdlib.compare (free_vars_of_cmd cmd @ fvs)
  |> symbolic_pkt
  |> wp `Negs cmd

let implements ?neg:(neg = True) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t)
    : [> `NoAndCE of Packet.t * Packet.t | `Yes] =
  let params = {params with no_defaults = false} in
  let st = Time.now () in
  Log.log params.debug "slicing logical\n";
  let log = Problem.log_gcl_program params problem in
  Log.log params.debug @@ Printf.sprintf "\t it has %d nodes\n" (num_nodes_in_cmd log);
  Log.log params.debug @@ Printf.sprintf "%s\n%!" (string_of_cmd log);
  Log.log params.debug "slicing physical\n";
  let phys = Problem.phys_gcl_program params problem in
  Log.log params.debug @@ Printf.sprintf "\t it has %d nodes\n" (num_nodes_in_cmd phys);
  Log.log params.debug @@ Printf.sprintf "%s\n%!" (string_of_cmd phys);
  Interactive.pause params.interactive;
  (* assert (fails_on_some_example log (Problem.fvs problem) (Problem.cexs problem) |> Option.is_none); *)
  ProfData.update_time !data.slicing_time st;

  match fails_on_some_example phys (Problem.fvs problem) (Problem.cexs problem) with
  | Some (in_pkt, out_pkt) -> `NoAndCE (in_pkt,out_pkt)
  | None ->
     Log.log false @@
       Printf.sprintf "-------------------------------------------\n%s \n???====?=====????\n %s\n-------------------------------------\n%!"
         (string_of_cmd log) (string_of_cmd phys);
     Interactive.pause params.interactive;
     let st_mk_cond = Time.now () in
     let condition = equivalent ~neg data Problem.(fvs problem) log phys in
     ProfData.update_time !data.make_vc_time st_mk_cond;
     let cv_st = Time.now () in
     let model_opt, z3time = if params.vcache then check_valid_cached params condition else check_valid params condition in
     ProfData.update_time !data.check_valid_time cv_st;
     ProfData.update_time_val !data.eq_time z3time;
     !data.tree_sizes :=  num_nodes_in_test condition :: !(!data.tree_sizes);
     ProfData.incr !data.eq_num_z3_calls;
     let st = Time.now () in
     let pkt_opt = match model_opt with
       | None  ->
          Log.log params.debug "++++++++++valid+++++++++++++\n%!";
          Interactive.pause params.interactive;
          `Yes
       | Some x ->
          let in_pkt, out_pkt = Packet.extract_inout_ce x in
          let remake = Packet.make ~fvs:(Problem.fvs problem |> Some) in
          let in_pkt' = if params.widening then in_pkt else remake in_pkt in
          let out_pkt' = if params.widening then out_pkt
                         else eval_act (Problem.log_gcl_program params problem) in_pkt in
          if params.debug then assert (Packet.subseteq ~fvs:(Some(Problem.fvs problem)) out_pkt' (eval_act log in_pkt'));
       if params.debug || params.interactive then
         Printf.printf "----------invalid----------------\n%! CE_in = %s\n log_out  = %s\n phys_out = %s\n%!"
           (Packet.string__packet in_pkt')
           (Packet.string__packet out_pkt')
           (Packet.string__packet @@ eval_act (Problem.phys_gcl_program params problem) in_pkt)
       ; `NoAndCE (in_pkt', out_pkt')
     in
     ProfData.update_time !data.normalize_packet_time st;
     pkt_opt


let unique_in_table params (_ (*prog*) : cmd) inst edits e =
  let open Edit in
  match e with
  | Del _ -> false
  | Add (tbl, (ms, _,_)) ->
     let index_of_e = List.findi edits ~f:(fun _ e' -> e = e') |> Option.value_exn |> fst in
     let earlier_edits = List.filteri edits ~f:(fun i _ -> i < index_of_e) in
     let inst' = Instance.update_list params inst earlier_edits in
     let earlier_rows = Instance.get_rows inst' tbl in
     List.for_all earlier_rows ~f:(fun (ms', _, _) ->
         not (Match.has_inter_l ms ms')
       )


let exists_in_table params (_ (*prog*) : cmd) inst edits e =
  let open Edit in
  match e with
  | Del _ -> false
  | Add (tbl, (ms, ad, aid)) ->
     let index_of_e = List.findi edits ~f:(fun _ e' -> e = e') |> Option.value_exn |> fst in
     let earlier_edits = List.filteri edits ~f:(fun i _ -> i < index_of_e) in
     let inst' = Instance.update_list params inst earlier_edits in
     let earlier_rows = Instance.get_rows inst' tbl in
     List.exists earlier_rows ~f:(fun (ms', ad', aid') ->
         List.equal (fun m m' -> Match.equal m m') ms ms'
         && aid = aid'
         && List.equal (fun v v' -> Stdlib.(v = v')) ad ad'
       )



(* The truth of a slice implies the truth of the full programs when
 * the inserted rules are disjoint with every previous rule (i.e. no overlaps or deletions)
 * Here we only check that the rules are exact, which implies this property given the assumption that every insertion is reachable
*)
let slice_conclusive (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  let st = Time.now () in
  let log_edit = Problem.log_edits problem |> List.hd_exn in
  let res =
    let open Problem in
      unique_in_table params (log problem) (log_inst problem) (log_edits problem) log_edit
      &&
        List.for_all (phys_edits problem) ~f:(fun e ->
            unique_in_table params (phys problem) (phys_inst problem) (phys_edits problem) e
            || exists_in_table params (phys problem) (phys_inst problem) (phys_edits problem) e
          )
    (* then
     *   (\* let () = Log.log params.debug @@ Printf.sprintf "\nquick sliceable check succeeded in %fms!\n%!" Time.(Span.(diff(now()) st |> to_ms))  in *\)
     *   true
     * else
     *   false
     *   (\* let () = Printf.printf "\nquick sliceable check FAILED!\n%!" in
     *    * let log_eqs  = FastCX.hits_list_pred params data (log problem) (log_inst problem) (log_edits problem) in
     *    * let phys_eqs = FastCX.hits_list_pred params data (phys problem) (phys_inst problem) (phys_edits problem) in
     *    * if log_eqs = phys_eqs
     *    * then
     *    *   true
     *    * else
     *    *   check_valid params (List.reduce_exn ~f:mkOr log_eqs %<=>% List.reduce_exn ~f:mkOr phys_eqs)
     *    *   |> fst
     *    *   |> Option.is_none *\) *)
  in
  ProfData.update_time !data.check_sliceable_time st;
  (* Printf.printf "\tSlice is %s\n%!" (if res then "conclusive" else "inconclusive"); *)
  res

let negate_model problem (model : value StringMap.t) (es : Edit.t list) : test =
  !%(if List.is_empty es
     then StringMap.fold model
            ~init:True
            ~f:(fun ~key ~data:(Int(i,sz)) acc ->
              mkAnd acc @@
                Hole(key, sz) %=% Value(Int(i,sz)))
     else
       Edit.test_of_list (Problem.phys problem) es
    )



let remove_missed_edits params data problem es =
  List.fold es ~init:[] ~f:(fun acc e ->
      let cex = List.hd_exn (Problem.cexs problem) in
      let check = Packet.to_test ~fvs:(Problem.fvs problem) (fst cex)
                  %=>% FastCX.hits_pred params data (Problem.phys problem) (Problem.phys_inst problem) (Problem.phys_edits problem) e in
      match check_valid params check with
      | None,_ ->
         (* Printf.printf "Checked \n%s\n" (string_of_test check); *)
         acc @ [e]
      | Some _,_ -> acc)

let extract_reached_edits (params : Parameters.t) data problem model =
  Edit.extract (Problem.phys problem) model
  |> if params.reach_filter then
       remove_missed_edits params data problem
     else Fn.id



let edit_cache = ref @@ EAbstr.make ()

let get_cex ?neg:(neg=True) (params : Parameters.t) (data :  ProfData.t ref) (problem : Problem.t)
    : [> `NoAndCE of Packet.t * Packet.t | `Yes] =
  if params.do_slice then Interactive.pause params.interactive ~prompt:"slicing begins!";
  if params.fastcx then begin
      let st = Time.now () in
      let cex = FastCX.get_cex ~neg params data problem in
      ProfData.update_time !data.fast_cex_time st;
      match cex with
      | `Yes ->
         Log.log params.debug "New rule is not reachable\n%!";
         `Yes
      | `NotFound _ ->
         Log.log params.debug "No cex to be found rapidly, check full equivalence\n%!";
         let st = Time.now () in
         let res = implements ~neg params data problem in
         ProfData.update_time !data.impl_time st;
         res
      | `NoAndCE counter ->
         Log.log params.debug "BACKTRACKING\n%!";
         `NoAndCE counter
    end
  else
    if params.do_slice && not( List.is_empty (Problem.phys_edits problem)) then
      let () = Log.log params.debug "\tSLICING\n%!" in
      let st = Time.now () in
      let res = implements ~neg params data (Problem.slice params problem) in
      let () = Log.log params.debug "\tslice checked\n%!" in
      ProfData.update_time !data.impl_time st;
      match res with
      | `NoAndCE counter ->
         Log.log params.debug "BACKTRACKING (SLICED)\n!%";
         `NoAndCE counter
      | `Yes when slice_conclusive params data problem ->
         Log.log params.debug "\tconclusively\n%!";
         `Yes
      | `Yes ->
         Log.log params.debug "\tinconclusively\n%!";
         implements ~neg params data problem
    else
      (* let () = Printf.printf "\tNormal Eq Check %d edits \n%!" (Problem.phys_edits problem |> List.length)in *)
      let st = Time.now () in
      let res = implements ~neg params data problem in
      ProfData.update_time !data.impl_time st;
      res



let rec minimize_edits params data problem certain uncertain =
  match uncertain with
  | [] -> certain
  | e::es ->
     (* Printf.printf "\t%s" (Edit.to_string e); *)
     match implements params data (Problem.replace_phys_edits problem (certain @ es)) with
     | `Yes -> minimize_edits params data problem certain es
     | `NoAndCE _ -> minimize_edits params data problem (certain@[e]) es

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
      | `Yes ->
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

      | `NoAndCE (in_pkt, _) ->
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
(* end *)
(* else begin
       *     Printf.printf "Exhausted the Space\n%!";
       *     match get_new_cex params data problem with
       *     | Some p -> solve_math i params data p
       *     | None ->
       *        Printf.printf "we really have exhausted the space\n";
       *        None
       *   end *)

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
    let es = extract_reached_edits params data problem model in

    Log.print_search_state params problem es model;
    Interactive.pause ~prompt:"\n" params.interactive;

    let problem' = Problem.(append_phys_edits problem es
                            |> reset_model_space
                            |> reset_attempts) in

    let problem = Problem.add_attempt problem model in
    let restriction = negate_model problem model es in
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
     Log.log params.debug "trying to slice\n";
     let did_cache_work = get_cex params_nofastcx_with_slicing data problem_with_cache_guess in

     match did_cache_work with
     | `Yes ->
        Interactive.pause params.interactive ~prompt:"Caching succeeded";
        Some ps
     | `NoAndCE (in_pkt,out_pkt) ->
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
