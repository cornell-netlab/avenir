open Core
open Ast
open Packet
open Semantics
open Prover
open Manip
open Util
open Tables


(* let symbolize x = x ^ "_SYMBOLIC" *)
(* let unsymbolize = String.chop_suffix_exn ~suffix:"_SYMBOLIC" *)
let is_symbolic = String.is_suffix ~suffix:"_SYMBOLIC"

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
  |> wp cmd

let implements ?neg:(neg = True) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t)
    : [> `NoAndCE of Packet.t * Packet.t | `Yes] =
  let st_mk_cond = Time.now () in
  let log = Problem.log_gcl_program params problem in
  let phys = Problem.phys_gcl_program params problem in
  (* Printf.printf "\timplements\n%!"; *)
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
       if params.debug || params.interactive then
         Printf.printf "----------invalid----------------\n%! CE_in = %s\n log_out  = %s\n phys_out = %s\n%!"
           (Packet.string__packet in_pkt')
           (Packet.string__packet out_pkt')
           (Packet.string__packet @@ eval_act (Problem.phys_gcl_program params problem) in_pkt)
       ; `NoAndCE (in_pkt', out_pkt')
  in
  ProfData.update_time !data.normalize_packet_time st;
  pkt_opt


let unique_in_table params prog inst edits e =
  let open Edit in
  match e with
  | Del _ -> false
  | Add (tbl, (ms, ds, a)) ->
     let index_of_e = List.findi edits ~f:(fun _ e' -> e = e') |> Option.value_exn |> fst in
     let earlier_edits = List.filteri edits ~f:(fun i _ -> i < index_of_e) in
     let inst' = Instance.update_list params inst earlier_edits in
     let earlier_rows = Instance.get_rows inst' tbl in
     List.for_all earlier_rows ~f:(fun (ms', _, _) ->
         not (Match.has_inter_l ms ms')
       )

let exists_in_table params prog inst edits e =
  let open Edit in
  match e with
  | Del _ -> false
  | Add (tbl, (ms, ds, a)) ->
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



(* A Reimplementation of the core algorithm using the math from the paper *)



let negate_model (model : value StringMap.t) : test =
  !%( StringMap.fold model
        ~init:True
        ~f:(fun ~key ~data:(Int(i,sz)) acc ->
          acc %&%
            (Hole(key, sz) %=% Value(Int(i,sz))))
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




let edit_cache = ref @@ EAbstr.make ()




let rec get_cex ?neg:(neg=True) (params : Parameters.t) (data :  ProfData.t ref) (problem : Problem.t)
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
         `NoAndCE counter
    end
  else
    if params.do_slice && not( List.is_empty (Problem.phys_edits problem)) then
      (* let () = Printf.printf "\tSLICING\n%!" in *)
      let st = Time.now () in
      let res = implements ~neg params data (Problem.slice params problem) in
      ProfData.update_time !data.impl_time st;
      match res with
      | `NoAndCE counter -> `NoAndCE counter
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



(*a model is suspicious if it contains a value that's not related to
   anything we've seen so far. Not sure how to analyze masks, so we
   just allow them to have any value*)
let suspect_model params problem es model =
  let ints_in_problem =
    multi_ints_of_cmd (Problem.log_gcl_program params problem)
    @ multi_ints_of_cmd (Problem.phys_gcl_program params problem)
    |> List.map ~f:fst
    |> List.dedup_and_sort ~compare:Bigint.compare
  in
  let open Bigint in
  StringMap.fold model ~init:false
    ~f:(fun ~key ~data acc ->
         let d = get_int data in
         if d = zero then acc
         else if  d = one then acc
         else if not (List.exists (Problem.fvs problem) ~f:(fun (v,_) -> String.is_substring key ~substring:v)) then
           let () = Printf.printf "%s is excluded\n" key in acc
         else if String.is_substring key ~substring:"_mask" then acc
         else if List.exists ints_in_problem ~f:(Bigint.(=) d) then acc
         else true
    )


let rec minimize_edits params data problem certain uncertain =
  match uncertain with
  | [] -> certain
  | e::es ->
     (* Printf.printf "\t%s" (Edit.to_string e); *)
     match implements params data (Problem.replace_phys_edits problem (certain @ es)) with
     | `Yes -> minimize_edits params data problem certain es
     | `NoAndCE _ -> minimize_edits params data problem (certain@[e]) es



let minimize_solution params data problem =
  let es = Printf.printf "\tminimizing\n";
           Problem.phys_edits problem
           |> minimize_edits params data problem [] in
  (* List.iter es
   *   ~f:(fun e ->
   *     Printf.printf "\t%s\n%!" (Edit.to_string e)
   *   ); *)
  Problem.replace_phys_edits problem es


let rec cegis_math (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) : (Edit.t list option) =
  (* Printf.printf "\tcegis_math\n%!"; *)
  (* Printf.printf "%s\n%!" (List.hd_exn (Problem.log_edits problem) |> Edit.to_string); *)
  match params.timeout with
  | Some (st,dur) when Time.(Span.(dur < diff(now()) st))  -> None
  | _ ->
  if params.ecache then
    (* let () = Printf.printf "\ttrying cache \n%!"in *)
    solve_math 1 params data problem
  else
    (* let st = Time.now () in *)
    let cex = get_cex params data problem in
    (* ProfData.update_time !data.impl_time st; *)
    match cex with
    | `Yes ->
       (* (\*if params.debug then*\) Printf.printf "\tNo CEX to be found -- programs are equiv\n%!"; *)
       let problem =
         if params.minimize
         then minimize_solution params data problem
         else problem
       in
       if not params.ecache then
         edit_cache := EAbstr.update !edit_cache (Problem.log_edits problem |> List.hd_exn) (Problem.phys_edits problem);
       if params.interactive then begin
           Printf.printf "%s\n%!" (Problem.phys_gcl_program params problem |> string_of_cmd);
           ignore(Stdio.In_channel.(input_char stdin) : char option)
         end;
       Problem.phys_edits problem |> Some
    | `NoAndCE counter ->
       (* Printf.printf "\tCEX found \n%!"; *)
       (* (Time.(Span.(diff (now()) st |> to_ms))); *)
       let problem = Problem.add_cex problem counter in
       let counter = (
           fst counter
         , Semantics.eval_act (Problem.log_gcl_program params problem) (fst counter))
       in
       if params.debug then
         Printf.printf "Counterexample found!\nin: %s\nlog:  %s\nphys:  %s\n\n%!"
           (Packet.string__packet @@ fst counter)
           (Packet.string__packet @@ snd counter)
           (Packet.string__packet @@ Semantics.eval_act (Problem.phys_gcl_program params problem) (fst counter))
       ;
         let params = {params with fastcx = false; ecache = false} in
         (* let f = liftPair ~f:Packet.equal ~combine:(&&) counter in *)
         (* if List.exists ~f (Problem.cexs problem) then begin
          *     if params.debug then
          *       Printf.eprintf "Duplicated counter example. IN: %s -------> OUT: %s\n%!"
          *         (fst counter |> Packet.string__packet)
          *         (snd counter |> Packet.string__packet);
          *     None
          *   end
          * else *)
         solve_math params.search_width params data
           (Problem.add_cex problem counter)



and solve_math (i : int) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  (* if params.debug then
   *   Printf.printf "+Model Space+\n%!"; *)
  (* Printf.printf "\tSolving for %d more iters\n%!" i; *)
  match params.timeout with
  | Some (st,dur) when Time.(Span.(dur < diff(now()) st)) ->
     Printf.printf "Timeout\n%!";
     None
  | _ ->
  if i = 0 then
    (* let () = Printf.printf "The jig is up\n%!" in *)
    None
  else
    if params.ecache then
      match EAbstr.infer !edit_cache (Problem.log_edits problem |> List.hd_exn) with
      | None ->
         (* Printf.printf "\nEdit Cache failed\n%!"; *)
         cegis_math {params with ecache = false; do_slice = false} data problem
      | Some ps ->
         (* Printf.printf "\nEdit Cache Succeeded\n%! Guessing\n";
          * List.iter ps ~f:(fun e -> Printf.printf "\t%s\n%!" (Edit.to_string e)); *)
         match get_cex {params with fastcx = false} data (Problem.replace_phys_edits problem ps) with
         | `Yes ->
            (* Printf.printf "Successful%!\n"; *)
            Some ps
         | `NoAndCE _ ->
            (* Printf.printf "Failed%!\n"; *)
            cegis_math {params with ecache = false} data problem
    else
      if Problem.model_space problem = True
         || (check_sat params (Problem.model_space problem) |> fst |> Option.is_some)
      then begin
          if (Problem.phys_edits problem |> List.length > params.edits_depth)
          then (* let () = Printf.printf "too many edits\n%!" in *)
               None
          else
            let st = Time.now () in
            let rec loop i problem searcher =
              match params.timeout with
              | Some (st,dur) when Time.(Span.(dur < diff(now()) st))  ->  Printf.printf "Timeout\n%!";None
              | _ ->
              if i = 0 then
                (* let () = Printf.printf "The jig is up\n%!" in *)
                None else
                (* let () = Printf.printf "\tlooping\n%!" in *)
                let model_opt = ModelFinder.search params data problem searcher in
                (*get_model params datproblem in*)
                ProfData.update_time !data.model_search_time st;
                match model_opt with
                | None ->
                   (* if params.debug || params.interactive then *)
                   (* Printf.printf "No model could be found\n%!"; *)
                   if params.interactive then
                     ignore(Stdio.In_channel.(input_char stdin) : char option);
                   if params.debug then None
                   else None (* begin match ModelFinder.search {params with debug = true} data problem searcher with
                              * | Some _ -> Printf.printf "found a model when retrying%!\n"; failwith ""
                              * | None -> Printf.printf "couldn't find a model when retrying"; None
                              * end *)
                | Some (model, searcher) ->
                   (* Printf.printf "\nfound model\n%s\n\n%!" (string_of_map model); *)
                   if Problem.seen_attempt problem model
                   then begin
                       Printf.printf "ALREADY EXPLORED\n %s \n\n %s \n%!"
                         (Problem.model_space problem |> string_of_test)
                         (string_of_map model);
                       let res = fixup_test model (Problem.model_space problem) in
                       Printf.printf "applied \n\n\n %s\n\n\n" (string_of_test res);
                       failwith ""
                     end
                   else
                     let problem = Problem.add_attempt problem model in
                     (* assert (Problem.num_attempts problem <= 1); *)
                     let es = Edit.extract (Problem.phys problem) model in
                     (* if List.exists es ~f:(fun e -> List.exists (Problem.phys_edits problem) ~f:(fun e' -> e = e') ) then
                      *   None
                      * else *)
                     let es = remove_missed_edits params data problem es in
                     let () = if params.debug then begin
                         (* Printf.printf "\tCEX in %s\n" (Packet.string__packet @@ fst @@ List.hd_exn @@ Problem.cexs problem); *)
                         Printf.printf "\n\t***Edits***\n%!";
                         Problem.phys_edits problem |>
                           List.iter ~f:(fun e -> Printf.printf "\t %s\n%!" (Edit.to_string e));
                         Printf.printf "\t*** New ***\n%!";
                         List.iter es ~f:(fun e -> Printf.printf "\t %s\n%!" (Edit.to_string e));
                         Printf.printf "\t***     ***\n";
                         (* ignore(Stdio.In_channel.(input_char stdin) : char option); *)
                       end in
                     (* let es =
                      *   if params.del_pushdown then
                      *     match Edit.get_deletes es with
                      *     | [] -> es
                      *     | ds -> es @ List.map ds ~f:(fun (table,idx) ->
                      *                      Edit.Add(table, Instance.get_row_exn (Problem.phys_inst problem) table idx)
                      *                    )
                      *   else es
                      * in *)


                     (* let (dels, _) = Edit.split @@ Problem.phys_edits problem @ es in
                      * if List.length dels > 0
                      * then
                      *   let () = Printf.printf "\t Committing\n%!" in
                      *   cegis_math params data Problem.(append_phys_edits problem es |> reset_model_space |> reset_attempts)
                      * else *)
                     (* let problem = Problem.apply_edits_to_phys params problem dels in *)
                     (* let model = List.fold (StringMap.keys model) ~init:model
                      *               ~f:(fun m k ->
                      *                 if String.is_prefix k ~prefix:(Hole.delete_row_prefix)
                      *                 then StringMap.remove m k
                      *                 else m
                      *               ) in *)
                     if List.length es = 0 then None else
                     let problem' = Problem.(append_phys_edits problem es
                                             |> reset_model_space
                                             |> reset_attempts) in
                     let continue () =
                       (* Printf.printf "\tmodel didnt work\n"; *)
                       let negated_edits = !%(Edit.test_of_list (Problem.phys problem) es) in
                       (* Printf.printf "%s\n%!" (string_of_test negated_edits); *)
                       let model_space = Problem.model_space problem %&% negated_edits in
                       let problem = Problem.set_model_space problem model_space in
                       match loop (i - 1) problem searcher with
                       | Some es -> Some es
                       | None ->
                          (* Printf.printf "\tBacktracking\n%!"; *)
                          (* if params.interactive then
                           *   ignore(Stdio.In_channel.(input_char stdin) : char option); *)
                          ProfData.incr !data.num_backtracks;
                          (* match (1, get_new_cex params data problem) with
                           * | 0, Some problem ->
                           *    Printf.printf "\tnew cex\n%!";
                           *    solve_math (i - 1) params data problem
                           * | _, _ -> *)
                          solve_math (i - 1) params data problem

                     in
                     if params.debug then begin
                         Printf.printf "\n%s\n%!" (Problem.to_string params problem');
                       end;
                     match cegis_math params data problem' with
                     | None -> continue ()
                     | Some es -> Some es in
            ModelFinder.make_searcher params data problem
            |> loop params.search_width problem
        end
      else begin
          Printf.printf "Exhausted the Space\n%!";
          match get_new_cex params data problem with
          | Some p -> solve_math i params data p
          | None ->
             Printf.printf "we really have exhausted the space\n";
             None
        end


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
