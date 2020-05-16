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

(** [complete] A completion takes a cmd to which a substitution has
    already been applied and replaces the remaining holes with integers
    that are not in the "active domain" of the program. This is a kind
    of an "educated un-guess" i.e. we're guessing values that are
    almost certainly wrong so that on the next run of the CEGIS loop Z3
    will notice and produce a counter example that will take this
    path. The optional [~falsify] flag will replace any [Eq] or [Lt]
    test containing a hole with [False] **)
let complete_inner ~falsify (cmd : cmd) =
  let domain = multi_ints_of_cmd cmd |> dedup in
  let rec complete_aux_test ~falsify t =
    let hole_replace x sz comp =
      if falsify
      then False
      else let i = random_int_nin (List.map ~f:(fun x -> fst x |> Bigint.to_int_exn) domain) in
        comp x (Value (Int (Bigint.of_int_exn i,sz)))
    in
    match t with
    | True | False -> t
    | Neg b -> !%(complete_aux_test ~falsify b)
    | And (a, b) -> complete_aux_test ~falsify a %&% complete_aux_test ~falsify b
    | Or (a, b) -> complete_aux_test ~falsify a %+% complete_aux_test ~falsify b
    | Impl (a, b) -> complete_aux_test ~falsify a %=>% complete_aux_test ~falsify b
    | Iff (a, b) -> complete_aux_test ~falsify a %<=>% complete_aux_test ~falsify b
    | Eq (Hole (_,sz), x) | Eq (x, Hole (_,sz)) -> hole_replace x sz (%=%)
    | Le (Hole (_,sz), x) | Le (x, Hole (_,sz)) -> hole_replace x sz (%<=%)
    | Eq _ | Le _ -> t
  and complete_aux ~falsify cmd =
    match cmd with
    | Skip -> cmd
    | Assign (f, v) ->
      begin
        match v with
        | Hole _ ->
          let i = random_int_nin (List.map ~f:(fun x -> fst x |> Bigint.to_int_exn) domain) |> Bigint.of_int_exn in
          let sz = Bigint.((one + one) ** i |> to_int_exn) in
          f %<-% Value (Int (i,sz))
        | _ -> cmd
      end
    | Assert b -> Assert (complete_aux_test ~falsify b)
    | Assume b -> Assume (complete_aux_test ~falsify b)
    | Seq (c, c') -> complete_aux ~falsify c %:% complete_aux ~falsify c'
    | While (b, c) -> While (complete_aux_test ~falsify b, complete_aux ~falsify c)
    | Select (styp, ss) ->
      Select(styp,
             List.map ss
               ~f:(fun (b, c) ->
                   complete_aux_test ~falsify b , complete_aux ~falsify c )
            )
    | Apply t
      -> Apply {t with
               actions = List.map t.actions ~f:(fun (data, a) -> (data, complete_aux a ~falsify));
               default = complete_aux ~falsify t.default}
  in
  complete_aux ~falsify cmd

let complete cmd = complete_inner ~falsify:true cmd

let get_one_model_edit
    (pkt : Packet.t)
    (data : ProfData.t ref)
    (params : Parameters.t)
    (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
    (problem : Problem.t)
  =
  let linst_edited = Problem.log_edited_instance params problem in
  let pinst_edited = Problem.phys_edited_instance params problem in
  let (pkt',_), wide, trace, actions = trace_eval_inst ~wide:StringMap.empty (Problem.log problem) linst_edited (pkt,None) in
  let deletions = [] in
  let st = Time.now () in
  let cands = CandidateMap.apply_hints params (Instance.WithHoles (deletions, [])) `Mask hints actions (Problem.phys problem) pinst_edited in
  let log_wp = wp trace True in
  let wp_phys_paths =
    List.fold cands ~init:[] ~f:(fun acc (path, acts) ->
        if params.debug then
          Printf.printf "Candidate:\n%s \n" (string_of_cmd path);
        let precs = if Option.is_none hints
          then
            [wp path (Packet.to_test ~fvs:(Problem.fvs problem) pkt')]
          else [wp path True]
        in
        acc @ List.map precs ~f:(inj_l acts))
    (* if prec = False then None else Some(prec, acts)) *)
  in
  let () = if params.debug then
      Printf.printf "The logical trace is: %s \n%!" (string_of_cmd trace) in
  ProfData.update_time !data.search_wp_time st;
  let model =
    List.find_map wp_phys_paths ~f:(fun (wp_phys, acts) ->
        if wp_phys = False then None else
          let () = if params.debug then Printf.printf "LOGWP %s\n => PHYSWP %s\n%!" (string_of_test log_wp) (string_of_test wp_phys) in
          if holes_of_test wp_phys = [] then
            (if params.debug then Printf.printf "no holes, so skipping\n%!";
             None)
          else
            let (res, time) = check_min params (log_wp %=>% wp_phys) in
            ProfData.update_time_val (!data.model_z3_time) time;
            ProfData.incr !data.model_z3_calls;
            match res with
            | None -> None
            | Some model -> Some (model, acts)
      )
  in
  model

let get_one_model_edit_no_widening
    (pkt : Packet.t)
    (data : ProfData.t ref)
    (params : Parameters.t)
    (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
    (problem : Problem.t)
  =
  (* print_instance "Logical" (apply_edit linst ledit);
   * print_instance "Physical" pinst; *)
  let interp_st = Time.now () in
  let linst_edited =  Problem.log_edited_instance params problem in
  let phys_edited = Problem.phys_edited_instance params problem in
  let (pkt',_), _, trace, actions = trace_eval_inst ~wide:StringMap.empty
      (Problem.log problem) linst_edited (pkt,None) in
  let deletions = [] in
    if params.debug then
    begin
      Printf.printf "The following rules are deletable";
      List.iter deletions
        ~f:(fun (t,i) -> Printf.printf " %s[%d]" t i);
      Printf.printf "\n--\n%!"
    end;
  ProfData.update_time !data.interp_time interp_st;
  (* let () = if params.debug || params.interactive then
   *     Printf.printf "CE input: %s \n%!CE TRACE: %s\nCE output: %s\n%!"
   *       (Packet.string__packet pkt)
   *       (string_of_cmd trace)
   *       (Packet.string__packet pkt');
   *   if params.interactive then
   *     (Printf.printf "Press enter to solve for CE input-output pair\n";
   *      ignore (In_channel.(input_char stdin) : char option))
   * in *)
  if params.debug then Printf.printf "Computing candidates\n%!";
  let cst = Time.now () in
  let cands = CandidateMap.apply_hints params (Instance.WithHoles (deletions,[])) `Exact hints actions (Problem.phys problem) phys_edited in
  ProfData.update_time !data.cand_time cst;
  let () =
    if params.debug then begin
        Printf.printf "Candidates:\n";
        List.iter cands
          ~f:(fun (cmd,_) -> Printf.printf "%s\n" (string_of_cmd cmd));
        Printf.printf "-------------\n"
      end
  in
  let wp_st = Time.now () in
  let wp_phys_paths =
    List.fold cands ~init:[] ~f:(fun acc (path, acts) ->
        let precs = if Option.is_none hints
                    then
                      if params.monotonic then
                        Packet.to_test ~fvs:(Problem.fvs problem) pkt'
                        |> wp_paths ~no_negations:false path
                        |> List.filter_map ~f:(fun (_, cond) ->
                               if has_hole_test cond then Some cond else None)
                      else
                        [wp ~no_negations:false path (Packet.to_test ~fvs:(Problem.fvs problem) pkt')]
                    else [wp path True]
        in
        acc @ List.map precs ~f:(inj_l acts))
  in
  ProfData.update_time !data.search_wp_time wp_st;
  if params.debug then
    Printf.printf "There are %d physical paths\n%!" (List.length wp_phys_paths);
  let model =
    List.find_map wp_phys_paths ~f:(fun (wp_phys, acts) ->
        if wp_phys = False then
          begin
            if params.debug then Printf.printf "Skipping because statically false\n%!";
            None
          end
        else
          let cdst = Time.now () in
          let condition = (Packet.to_test ~fvs:(Problem.fvs problem) ~random_fill:true pkt %=>% wp_phys) in
          let c_dur = Time.diff (Time.now ()) cdst in
          if params.debug then
            Printf.printf "Checking \n%s  \n=> \n%s\n%!"
              (Packet.to_test ~fvs:(Problem.fvs problem) ~random_fill:false pkt
               |> string_of_test)
              (string_of_test wp_phys);
          let h_st = Time.now() in
          let h_dur =  Time.diff (Time.now ()) h_st in
          let (res, dur) = check_sat params condition in
          ProfData.update_time_val !data.model_holes_time h_dur;
          ProfData.update_time_val !data.model_z3_time dur;
          ProfData.incr !data.model_z3_calls;
          ProfData.update_time_val !data.model_cond_time c_dur;
          begin match res with
          | None -> if params.debug then Printf.printf "no model\n%!";None
          | Some model -> Some (model, acts)
          end
      )
  in
  model

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

let implements (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t)
  : [> `NoAndCE of Packet.t * Packet.t | `Yes] =
  let st_mk_cond = Time.now () in
  let log = Problem.log_gcl_program params problem in
  let phys = Problem.phys_gcl_program params problem in
  (* Printf.printf "\timplements\n%!"; *)
  if params.debug then
    Printf.printf "-------------------------------------------\n%s \n???====?=====????\n %s\n-------------------------------------\n%!"
      (string_of_cmd log) (string_of_cmd phys);
  ProfData.update_time !data.make_vc_time st_mk_cond;
  let condition = equivalent Problem.(fvs problem) log phys in
  let cv_st = Time.now () in
  let model_opt, z3time = if params.do_slice then check_valid_cached params condition else check_valid params condition in
  ProfData.update_time !data.check_valid_time cv_st;
  ProfData.update_time_val !data.eq_time z3time;
  !data.tree_sizes :=  num_nodes_in_test condition :: !(!data.tree_sizes);
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


(** solves the inner loop **)
let rec solve_concrete
    ?packet:(packet=None)
    (data : ProfData.t ref)
    (params : Parameters.t)
    (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
    (problem : Problem.t)
  : (Edit.t list) =
  let values = multi_ints_of_cmd (Problem.log problem) |> List.map ~f:(fun x -> Int x) in
  let pkt = packet |> Option.value ~default:(Packet.generate (Problem.fvs problem) ~values) in
  let model_finder = if params.widening
    then get_one_model_edit
    else get_one_model_edit_no_widening in
  let st = Time.now () in
  match model_finder pkt data params hints problem with
  | None -> Printf.sprintf "Couldnt find a model" |> failwith
  | Some (model, action_map) ->
    ProfData.update_time !data.model_search_time st;
    Edit.extract (Problem.phys problem) model

(* The truth of a slice implies the truth of the full programs when
 * the inserted rules are disjoint with every previous rule (i.e. no overlaps or deletions)
 * Here we only check that the rules are exact, which implies this property given the assumption that every insertion is reachable
*)
let slice_conclusive (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  let st = Time.now () in
  let res = true in
  (*   let open Problem in
   *   let log_eqs  = FastCX.hits_list_pred params data (log problem) (log_inst problem) (log_edits problem) in
   *   let phys_eqs = FastCX.hits_list_pred params data (phys problem) (phys_inst problem) (phys_edits problem) in
   *   if log_eqs = phys_eqs then true else
   *   check_valid params (List.reduce_exn ~f:mkOr log_eqs %<=>% List.reduce_exn ~f:mkOr phys_eqs)
   *   |> fst
   *   |> Option.is_none
   * in *)
  ProfData.update_time !data.check_sliceable_time st;
  (* Printf.printf "\tSlice is %s\n%!" (if res then "conclusive" else "inconclusive"); *)
  res

let cegis ~iter
    (params : Parameters.t)
    (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
    (data : ProfData.t ref)
    (problem : Problem.t)
  : (Edit.t list) option
  =
  let rec loop (params : Parameters.t) (problem : Problem.t) : (Edit.t list) option =
    if params.interactive then
      (Printf.printf "Press enter to continue\n%!";
       ignore(Stdio.In_channel.(input_char stdin) : char option));
    if params.debug || params.interactive then
      Printf.printf "======================= LOOP (%d, %d) =======================\n%!%s\n%!" (iter) (params.gas) (Problem.to_string params problem);
    let imp_st = Time.now () in (* update data after setting time start  *)
    let res = ((* if params.fastcx
                * then
                *   ( if params.debug then Printf.printf "getting fast cx\n";
                *     List.hd_exn problem.log_edits
                *     |> get_cex params data problem.log problem.log_inst problem.phys problem.phys_inst)
                * else *) (if params.do_slice
                     then implements params data (Problem.slice params problem)
                     else implements params data problem)) in
    let params = {params with fastcx = false} in
    ProfData.update_time !data.impl_time imp_st;
    let do_cex counter =
      (* if params.interactive then
       *   (Printf.printf "Press enter to resolve counterexample\n%!";
       *    ignore(Stdio.In_channel.(input_char stdin) : char option)); *)
      if params.gas = 0 then failwith "RAN OUT OF GAS" else
        let pedits = solve_concrete ~packet:(Some counter) data params hints problem in
        if List.length pedits = 0
        then failwith ("Could not make progress on edits ")
        else loop
            { params with gas = params.gas - 1 }
            (Problem.append_phys_edits problem pedits)
    in
    match res with
    | `Yes ->
       if params.do_slice && not (slice_conclusive params data problem)
       then
         match implements params data problem with
         | `Yes -> Problem.phys_edits problem |> Some
         | `NoAndCE counter -> do_cex (fst counter)
       else Problem.phys_edits problem |> Some
    | `NoAndCE counter -> do_cex (fst counter)
  in
  loop params problem

let synthesize ~iter (params : Parameters.t) (hints : (CandidateMap.trace -> CandidateMap.trace list) option) (data : ProfData.t ref)  (problem : Problem.t) =
  let start = Time.now () in
  let pedits = cegis ~iter params hints data problem in
  let pedits_out = Option.value ~default:[] pedits in
  !data.time := Time.(diff (now ()) start);
  if params.debug || params.interactive then
    Printf.printf "\nSynthesized Program (%d edits made):\n%s\n\n%!"
      (List.length pedits_out)
      (Problem.replace_phys_edits problem pedits_out
       |> Problem.phys_gcl_program params
       |> string_of_cmd);
  !data.log_inst_size := Instance.size (Problem.log_edited_instance params problem);
  !data.phys_inst_size := Instance.size (Problem.phys_inst problem);
  pedits_out



(* A Reimplementation of the core algorithm using the math from the paper *)
let negate_model (model : value StringMap.t) : test =
  !%( StringMap.fold model
        ~init:True
        ~f:(fun ~key ~data:(Int(i,sz)) acc ->
          acc %&%
            (Hole(key, sz) %=% Value(Int(i,sz))))
    )


let complete_model (holes : (string * size) list) (model : value StringMap.t) : value StringMap.t =
  model
  (* let bound_vars = StringMap.keys model in
   * let unbound_vars = List.filter holes
   *                      ~f:(fun (x,_) ->
   *                        List.for_all bound_vars ~f:((<>) x)) in
   * List.fold unbound_vars ~init:model
   *   ~f:(fun acc (key,sz) ->
   *     let data =
   *       Int (Bigint.random Bigint.(pow (of_int 2) (of_int sz)), sz)
   *     in
   *     StringMap.set acc ~key ~data
   *   ) *)

(* let get_model (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) : (value StringMap.t option * Hint.t list) =
 *   let (in_pkt, out_pkt) = Problem.cexs problem |> List.hd_exn in
 *   let st = Time.now () in
 *   let deletions = [] in
 *   let hints = Hint.construct (Problem.log problem) (Problem.phys problem) (Problem.log_edits problem |> List.hd_exn) in
 *   let hole_protocol = Instance.WithHoles (deletions, hints) in
 *   let hole_type =  if params.widening then `Mask else `Exact in
 *   let phys = Problem.phys_gcl_holes problem hole_protocol hole_type  in
 *   ProfData.update_time !data.model_holes_time st;
 *   let st = Time.now () in
 *   let fvs = List.(free_vars_of_cmd phys
 *                   |> filter ~f:(fun x -> exists (Problem.fvs problem) ~f:(Stdlib.(=) x))) in
 *   (\* let fvs = problem.fvs in *\)
 *   let in_pkt_form, out_pkt_form = Packet.to_test in_pkt ~fvs, Packet.to_test out_pkt ~fvs in
 *   let wp_list =
 *     (if params.monotonic then
 *       wp_paths ~no_negations:true phys out_pkt_form
 *     else
 *       []) @  [Skip, wp phys out_pkt_form]
 *   in
 *   ProfData.update_time !data.search_wp_time st;
 *   let model =
 *     List.find_map wp_list
 *       ~f:(fun (_,in_pkt_wp) ->
 *         if in_pkt_wp = False || not (has_hole_test in_pkt_wp) then
 *           let () = if params.debug then Printf.printf "WP was False or holeless\n%!" in
 *           None
 *         else
 *         let st = Time.now() in
 *         (\* Printf.printf "\n\nWP: %s\n\n%!" (string_of_test in_pkt_wp); *\)
 *         let spec = in_pkt_form %=>% in_pkt_wp in
 *         let wf_holes = List.fold (Problem.phys problem |> get_tables_actsizes) ~init:True
 *                          ~f:(fun acc (tbl,num_acts) ->
 *                            acc %&%
 *                              (Hole.which_act_hole tbl (max (log2 num_acts) 1)
 *                               %<=% mkVInt(num_acts-1,max (log2 num_acts) 1))) in
 *         let widening_constraint =
 *           if params.widening then
 *             List.fold (holes_of_test in_pkt_wp)  ~init:True
 *               ~f:(fun acc (hole, sz) ->
 *                 match String.chop_suffix hole ~suffix:"_mask" with
 *                 | Some hole_val ->
 *                    Hole(hole_val,sz) %=% (mkMask (Hole(hole_val,sz)) (Hole(hole, sz)))
 *                    %&% ((Hole(hole,sz) %=% mkVInt(0, sz))
 *                         %+% (Hole(hole,sz) %=% Value(Int(Bigint.((pow (of_int 2) (of_int sz)) - one), sz))))
 *                 | None -> acc
 *
 *               )
 *           else True
 *         in
 *         let pre_condition =
 *           (Problem.model_space problem) %&% wf_holes %&% spec
 *           |> Injection.optimization params problem
 *         in
 *         let wf_condition = wf_holes %&% widening_constraint %&% pre_condition in
 *         ProfData.update_time !data.model_cond_time st;
 *         if params.debug || params.interactive then
 *                     Printf.printf "phys to check\n--\nInput:%s\n--\n%s\n--\nOutput:%s\n--\nWP:\n%s--\n%!"
 *                       (Packet.string__packet in_pkt)
 *                       (string_of_cmd phys)
 *                       (Packet.string__packet out_pkt)
 *                       (string_of_test spec);
 *         let model, dur = check_sat params wf_condition in
 *         ProfData.update_time_val !data.model_z3_time dur;
 *         ProfData.incr !data.model_z3_calls;
 *         !data.tree_sizes := num_nodes_in_test wf_condition :: !(!data.tree_sizes) ;
 *         Option.(model >>| complete_model (holes_of_test wf_condition))
 *       )
 *   in
 *   if params.interactive then begin
 *       ignore(Stdio.In_channel.(input_char stdin) : char option)
 *     end;
 *   begin match model with
 *   | None -> Printf.printf "All Attempts Failed\n%!"
 *   | Some _ -> ()
 *   end;
 *   (model,hints) *)


let edit_cache = ref @@ EAbstr.make ()

let rec get_cex (params : Parameters.t) (data :  ProfData.t ref) (problem : Problem.t)
    : [> `NoAndCE of Packet.t * Packet.t | `Yes] =
  if params.fastcx then begin
      let st = Time.now () in
      let cex = FastCX.get_cex params data problem in
      ProfData.update_time !data.fast_cex_time st;
      match cex with
      | `Yes ->
         if params.debug then
           Printf.printf "New rule is not reachable\n%!";
         `Yes
      | `NotFound _ ->
         Printf.printf "\t     but it failed\n%!";
         if params.debug then
           Printf.printf "No cex to be found rapidly, check full equivalence\n%!";
         let st = Time.now () in
         let res = implements params data problem in
         ProfData.update_time !data.impl_time st;
         res
      | `NoAndCE counter ->
         `NoAndCE counter
    end
  else
    if params.do_slice && not( List.is_empty (Problem.phys_edits problem)) then
      (* let () = Printf.printf "\tSLICING\n%!" in *)
      let st = Time.now () in
      let res = implements params data (Problem.slice params problem) in
      ProfData.update_time !data.impl_time st;
      match res with
      | `NoAndCE counter -> `NoAndCE counter
      | `Yes when slice_conclusive params data problem -> `Yes
      | `Yes -> implements params data problem
    else
      let () = Printf.printf "\tNotSlicing\n%!" in
      let st = Time.now () in
      let res = implements params data problem in
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

let rec cegis_math (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) : (Edit.t list option) =
  (* Printf.printf "\tcegis_math\n%!"; *)
  if params.cache then solve_math 1 params data problem else
    let st = Time.now () in
    let cex = get_cex params data problem in
    (* ProfData.update_time !data.impl_time st; *)
    match cex with
    | `Yes ->
       (* (\*if params.debug then*\) Printf.printf "\tNo CEX to be found -- programs are equiv\n%!"; *)
       edit_cache := EAbstr.update !edit_cache (Problem.log_edits problem |> List.hd_exn) (Problem.phys_edits problem);
       if params.interactive then begin
           Printf.printf "%s\n%!" (Problem.phys_gcl_program params problem |> string_of_cmd);
           ignore(Stdio.In_channel.(input_char stdin) : char option)
         end;
       Problem.phys_edits problem |> Some
    | `NoAndCE counter ->
       (* Printf.printf "\tCEX found in %fms\n%!" *)
       (* (Time.(Span.(diff (now()) st |> to_ms))); *)
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
         let params = {params with fastcx = false; cache = false} in
         (* let f = liftPair ~f:Packet.equal ~combine:(&&) counter in *)
         (* if List.exists ~f (Problem.cexs problem) then begin
          *     if params.debug then
          *       Printf.eprintf "Duplicated counter example. IN: %s -------> OUT: %s\n%!"
          *         (fst counter |> Packet.string__packet)
          *         (snd counter |> Packet.string__packet);
          *     None
          *   end
          * else *)
         solve_math 100 params data
           (Problem.add_cex problem counter)

and solve_math (i : int) (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  (* if params.debug then
   *   Printf.printf "+Model Space+\n%!"; *)
  (* Printf.printf "\tSolving\n%!"; *)
  if i = 0 then None else
    if params.cache then
      match EAbstr.infer !edit_cache (Problem.log_edits problem |> List.hd_exn) with
      | None ->
         Printf.printf "\tedit cache miss\n";
         cegis_math {params with cache = false} data problem
      | Some ps ->
         match cegis_math {params with fastcx = false; cache = false} data (Problem.replace_phys_edits problem ps) with
         | Some ps -> Some ps
         | None -> cegis_math {params with cache = false} data problem
    else
      if Problem.model_space problem = True
         || (check_sat params (Problem.model_space problem) |> fst |> Option.is_some)
      then begin
          if (Problem.phys_edits problem |> List.length > 5)
          then None
          else
            let st = Time.now () in
            let rec loop i problem searcher =
              if i = 0 then None else
                (* Printf.printf "\tlooping\n%!"; *)
                let model_opt = ModelFinder.search params data problem searcher in
                (*get_model params datproblem in*)
                ProfData.update_time !data.model_search_time st;
                match model_opt with
                | None ->
                   (* if params.debug || params.interactive then *)
                   Printf.printf "No model could be found\n%!";
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
                     if true then begin
                         Printf.printf "\t***Edits***\n%!";
                         List.iter es ~f:(fun e -> Printf.printf "\t %s\n%!" (Edit.to_string e));
                         Printf.printf "\t***     ***\n"
                       end;
                     (* let es =
                      *   if params.del_pushdown then
                      *     match Edit.get_deletes es with
                      *     | [] -> es
                      *     | ds -> es @ List.map ds ~f:(fun (table,idx) ->
                      *                      Edit.Add(table, Instance.get_row_exn (Problem.phys_inst problem) table idx)
                      *                    )
                      *   else es
                      * in *)


                     (* let (dels, es) = Edit.split es in
                      * let problem = Problem.apply_edits_to_phys params problem dels in
                      * let model = List.fold (StringMap.keys model) ~init:model
                      *               ~f:(fun m k ->
                      *                 if String.is_prefix k ~prefix:(Hole.delete_row_prefix)
                      *                 then StringMap.remove m k
                      *                 else m
                      *               ) in *)
                     let problem' = Problem.(append_phys_edits problem es
                                             |> reset_model_space
                                             |> reset_attempts) in
                     let continue () =
                       Printf.printf "\tmodel didnt work\n";
                       let model_space = Problem.model_space problem %&% negate_model model in
                       let problem = Problem.set_model_space problem model_space in
                       match loop (i - 1) problem searcher with
                       | None ->
                          Printf.printf "\tBacktracking\n%!";
                          (* if params.interactive then
                           *   ignore(Stdio.In_channel.(input_char stdin) : char option); *)
                          ProfData.incr !data.num_backtracks;
                          solve_math (i - 1) params data problem
                       | Some es -> Some es
                     in
                     if params.debug then begin
                         Printf.printf "\n%s\n%!" (Problem.to_string params problem');
                       end;
                     match cegis_math params data problem' with
                     | None when List.length (Problem.phys_edits problem) > 10
                       -> None
                     | None -> continue ()
                     | Some es -> Some es in
            ModelFinder.make_searcher params data problem
            |> loop i problem
        end
      else begin
          Printf.printf "Exhausted the Space\n%!";
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
          (* Printf.printf "\n\n\n%s\n\n\n" (Problem.to_string problem); *)
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
