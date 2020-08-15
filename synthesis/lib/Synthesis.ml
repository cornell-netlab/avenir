open Core
open Ast
open Packet
open Semantics
open Prover
open Manip
open Util
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
  let st_mk_cond = Time.now () in
  (* let slice = StaticSlicing.static_slice (Problem.fvs problem) in *)
  let log = Problem.log_gcl_program params problem(* |> slice*) in
  let phys = Problem.phys_gcl_program params problem (* |> slice*) in
  assert (fails_on_some_example log (Problem.fvs problem) (Problem.cexs problem) |> Option.is_none);

  match fails_on_some_example phys (Problem.fvs problem) (Problem.cexs problem) with
  | Some (in_pkt, out_pkt) -> `NoAndCE (in_pkt,out_pkt)
  | None ->
     if params.debug then
       Printf.printf "-------------------------------------------\n%s \n???====?=====????\n %s\n-------------------------------------\n%!"
         (string_of_cmd log) (string_of_cmd phys);
     ProfData.update_time !data.make_vc_time st_mk_cond;
     let condition = equivalent ~neg Problem.(fvs problem) log phys in
     let cv_st = Time.now () in
     let model_opt, z3time = if params.vcache then check_valid_cached params condition else check_valid params condition in
     ProfData.update_time !data.check_valid_time cv_st;
     ProfData.update_time_val !data.eq_time z3time;
     !data.tree_sizes :=  num_nodes_in_test condition :: !(!data.tree_sizes);
     ProfData.incr !data.eq_num_z3_calls;
     let st = Time.now () in
     let pkt_opt = match model_opt with
       | None  -> if params.debug then Printf.printf "++++++++++valid+++++++++++++\n%!";
                  `Yes
       | Some x ->
          let in_pkt, out_pkt = Packet.extract_inout_ce x in
          let remake = Packet.make ~fvs:(Problem.fvs problem |> Some) in
          let in_pkt' = if params.widening then in_pkt else remake in_pkt in
          let out_pkt' = if params.widening then out_pkt
                         else eval_act (Problem.log_gcl_program params problem) in_pkt in
          assert (Packet.subseteq ~fvs:(Some(Problem.fvs problem)) out_pkt' (eval_act log in_pkt'));
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
  | Add (tbl, (ms, _, _)) ->
     let index_of_e = List.findi edits ~f:(fun _ e' -> e = e') |> Option.value_exn |> fst in
     let earlier_edits = List.filteri edits ~f:(fun i _ -> i < index_of_e) in
     let inst' = Instance.update_list params inst earlier_edits in
     let earlier_rows = Instance.get_rows inst' tbl in
     List.exists earlier_rows ~f:(fun (ms', _, _) ->
         List.equal (fun m m' -> Match.equal m m') ms ms'
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
    if
      unique_in_table params (Problem.log problem) (Problem.log_inst problem) (Problem.log_edits problem) log_edit
      &&
        List.for_all (Problem.phys_edits problem) ~f:(fun e ->
            unique_in_table params (Problem.phys problem) (Problem.phys_inst problem) (Problem.phys_edits problem) e
            || exists_in_table params (Problem.phys problem) (Problem.phys_inst problem) (Problem.phys_edits problem) e
          )
    then
      (* let () = Printf.printf "\nquick sliceable check succeeded in %fms!\n%!" Time.(Span.(diff(now()) st |> to_ms))  in *)
      true
    else
      (* let () = Printf.printf "\nquick sliceable check FAILED!\n%!" in *)
      let log_eqs  = FastCX.hits_list_pred params data (log problem) (log_inst problem) (log_edits problem) in
      let phys_eqs = FastCX.hits_list_pred params data (phys problem) (phys_inst problem) (phys_edits problem) in
      if log_eqs = phys_eqs
      then true
      else
        check_valid params (List.reduce_exn ~f:mkOr log_eqs %<=>% List.reduce_exn ~f:mkOr phys_eqs)
        |> fst
        |> Option.is_none
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

let extract_reached_edits params data problem model =
  Edit.extract (Problem.phys problem) model
  |> remove_missed_edits params data problem



let edit_cache = ref @@ EAbstr.make ()




let get_cex ?neg:(neg=True) (params : Parameters.t) (data :  ProfData.t ref) (problem : Problem.t)
        : [> `NoAndCE of Packet.t * Packet.t | `Yes] =
  if params.fastcx then begin
      let st = Time.now () in
      let cex = FastCX.get_cex ~neg params data problem in
      ProfData.update_time !data.fast_cex_time st;
      match cex with
      | `Yes ->
         if params.debug then
           Printf.printf "New rule is not reachable\n%!";
         `Yes
      | `NotFound _ ->
         (* Printf.printf "\t     but it failed\n%!"; *)
         if params.debug then
           Printf.printf "No cex to be found rapidly, check full equivalence\n%!";
         let st = Time.now () in
         let res = implements ~neg params data problem in
         ProfData.update_time !data.impl_time st;
         res
      | `NoAndCE counter ->
         Printf.printf "BACKTRACKING\n%!";
         `NoAndCE counter
    end
  else
    if params.do_slice && not( List.is_empty (Problem.phys_edits problem)) then
      (* let () = Printf.printf "\tSLICING\n%!" in *)
      let st = Time.now () in
      let res = implements ~neg params data (Problem.slice params problem) in
      ProfData.update_time !data.impl_time st;
      match res with
      | `NoAndCE counter ->
         Printf.printf "BACKTRACKING (SLICED)\n%!";
         `NoAndCE counter
      | `Yes when slice_conclusive params data problem -> `Yes
      | `Yes -> implements ~neg params data problem
    else
      (* let () = Printf.printf "\tNormal Eq Check %d edits \n%!" (Problem.phys_edits problem |> List.length)in *)
      let st = Time.now () in
      let res = implements ~neg params data problem in
      ProfData.update_time !data.impl_time st;
      res
      (* let hits_phys_edits =
       *   FastCX.hits_list_pred
       *     data
       *     (Problem.phys problem)
       *     (Problem.phys_inst problem)
       *     (Problem.phys_edits problem)
       * in
       * match
       *   Problem.update_phys problem (Assume hits_phys_edits %:% (Problem.phys problem))
       *   |> implements params data
       * with
       * | `NoAndCE counter -> `NoAndCE counter
       * | `Yes ->
       *    Problem.update_phys problem (Assume !%(hits_phys_edits) %:% Problem.phys problem)
       *    |> implements params data *)

let get_new_cex params data problem =
  let open Problem in
  let ctest = List.fold (cexs problem)
                ~init:True
                ~f:(fun acc (in_pkt,_) -> acc %&% !%(Packet.to_test  ~fvs:(fvs problem) in_pkt)) in
  match get_cex params data problem ~neg:ctest with
  | `Yes -> None
  | `NoAndCE ((inp,outp) as counter)->
     if List.exists ~f:(fun (i,o) -> Packet.equal i inp && Packet.equal o outp ) (cexs problem)
     then failwith "repeated counterexample"
     else add_cex problem counter |> Some

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
    let () = Printf.printf "\tminimizing\n" in
    Problem.phys_edits problem
    |> minimize_edits params data problem []
    |> Problem.replace_phys_edits problem
  else
    problem



let rec cegis_math (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) : (Edit.t list option) =
  Printf.printf "cegis_math\n%!";
  (* Printf.printf "%s\n%!" (List.hd_exn (Problem.log_edits problem) |> Edit.to_string); *)
  if timed_out params.timeout then None else
    if params.ecache then
      (* let () = Printf.printf "\ttrying cache \n%!"in *)
      solve_math 1 params data problem
    else
      match get_cex params data problem with
      | `Yes ->
         (* optionally minimize the solution*)
         let problem = minimize_solution params data problem in

         (* if we didn't use the edit cache in the solution, update it *)
         if not params.ecache then
           edit_cache := EAbstr.update !edit_cache
                           (Problem.log_edits problem |> List.hd_exn)
                           (Problem.phys_edits problem);

         (*Pause execution and wait for operator's ok to continue*)
         Interactive.pause params.interactive
           ~prompt:(string_of_cmd @@ Problem.phys_gcl_program params problem);

         Some (Problem.phys_edits problem)

      | `NoAndCE (in_pkt, _) ->
         let log_out_pkt = Semantics.eval_act (Problem.log_gcl_program params problem) in_pkt in
         let params = {params with fastcx = false; ecache = false} in
         let problem = Problem.add_cex problem (in_pkt, log_out_pkt) in
         Log.cexs ~log_out_pkt:(Some log_out_pkt) params (Problem.phys_gcl_program params problem) in_pkt;
         solve_math params.search_width params data problem

and solve_math (i : int) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  Printf.printf "solve_math\n%!";
  if timed_out params.timeout || i = 0 then None else
    if params.ecache then
      try_cache params data problem
    else
      if is_sat params (Problem.model_space problem)
      then begin
          List.length(Problem.phys_edits problem) <= params.edits_depth
          |=> fun _ ->
              ModelFinder.make_searcher params data problem
              |> drive_search params.search_width params data problem
        end
      else begin
          Printf.printf "Exhausted the Space\n%!";
          match get_new_cex params data problem with
          | Some p -> solve_math i params data p
          | None ->
             Printf.printf "we really have exhausted the space\n";
             None
        end

and drive_search (i : int) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) searcher =
  Printf.printf "loop\n%!";
  let open Option in
  if timed_out params.timeout then begin
      Printf.printf "Timeout\n%!";
      None
    end
  else if i = 0 then begin
      Printf.printf "Out of gas\n%!";
      None
    end
  else
    let st = Time.now () in
    let model_opt = ModelFinder.search params data problem searcher in
    ProfData.update_time !data.model_search_time st;
    model_opt >>= fun (model, searcher) ->
    let es = extract_reached_edits params data problem model in

    Log.print_search_state true problem es model;
    Interactive.pause ~prompt:"\n" params.interactive;

    let problem' = Problem.(append_phys_edits problem es
                            |> reset_model_space
                            |> reset_attempts) in
    Log.print_problem params problem';

    let problem = Problem.add_attempt problem model in
    let problem = negate_model problem model es
                  |> Problem.refine_model_space problem in

    not(List.is_empty es)
    |=> fun _ ->
        try_in_sequence [
            (fun _ -> cegis_math params data problem');
            (fun _ -> Log.backtracking params;
                      Printf.printf "the model_space is \n %s\n---\n%!"
                        (string_of_test @@ Problem.model_space problem);
                      Interactive.pause params.interactive;
                      drive_search (i - 1) params data problem searcher);
            (fun _ -> ProfData.incr !data.num_backtracks;
                      solve_math (i - 1) params data problem)
          ]

and try_cache params data problem =
  match EAbstr.infer !edit_cache (Problem.log_edits problem |> List.hd_exn) with
  | None ->
     let params =
       {params with ecache = false;   (* caching failed so disable it *)
                    do_slice = false  (* dont slice.. I don't remember why not *)
       } in
     cegis_math params data problem
  | Some ps ->

     (* fastCX's preconditions may be violated, so make sure its turned off*)
     let params_nofastcx = {params with fastcx = false} in

     (* add guessed edits to problem*)
     let problem_with_cache_guess = Problem.replace_phys_edits problem ps in

     (* try and get a CX to see if the problem works *)
     let did_cache_work = get_cex params_nofastcx data problem_with_cache_guess in

     match did_cache_work with
     | `Yes -> Some ps
     | `NoAndCE _ ->
        let params = {params with ecache = false} in (* caching failed so disable it *)
        cegis_math params data problem


let cegis_math_sequence (params : Parameters.t) data problem =
  let log_edit_sequence = Problem.log_edits problem in
  let problem = Problem.replace_log_edits problem [] in
  match List.fold log_edit_sequence ~init:(Some(problem,[]))
    ~f:(fun acc ledit ->
      match acc with
      | None -> None
      | Some (problem, pedits) ->
          let problem = Problem.replace_log_edits problem [ledit] in
          Printf.printf "\n\n\n%s\n\n\n" (Problem.to_string params problem);
          match cegis_math params data problem with
          | None -> None
          | Some phys_edits ->
             Some
               (Problem.replace_phys_edits problem phys_edits
                |> Problem.commit_edits_log params
                |> Problem.commit_edits_phys params,
                pedits @ phys_edits)
    )
  with
  | None -> None
  | Some (problem,edits) ->
     match implements params data problem with
     | `Yes -> Some (problem, edits)
     | `NoAndCE _ -> assert false;
