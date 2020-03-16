open Core
open Ast
open Packet
open Semantics
open Prover
open Manip
open Util
open Tables
open FastCX


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
    | Apply (name, keys, acts, dflt)
      -> Apply (name
               , keys
               , List.map acts ~f:(fun (data, a) -> (data, complete_aux a ~falsify))
               , complete_aux ~falsify dflt)
  in
  complete_aux ~falsify cmd

let complete cmd = complete_inner ~falsify:true cmd

let compute_deletions pkt (problem : Problem.t) = get_nd_hits problem.log problem.log_inst pkt

let get_one_model_edit
    (pkt : Packet.t)
    (data : ProfData.t ref)
    (params : Parameters.t)
    (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
    (problem : Problem.t)
  =
  let linst_edited = Instance.update_list problem.log_inst problem.log_edits in
  let pinst_edited = Instance.update_list problem.phys_inst problem.phys_edits in
  let (pkt',_), wide, trace, actions = trace_eval_inst ~wide:StringMap.empty problem.log linst_edited (pkt,None) in
  let deletions = compute_deletions pkt problem in
  let st = Time.now () in
  let cands = CandidateMap.apply_hints (`WithHoles deletions) `Range hints actions problem.phys pinst_edited in
  let log_wp = wp trace True in
  let wp_phys_paths =
    List.fold cands ~init:[] ~f:(fun acc (path, acts) ->
        if params.debug then
          Printf.printf "Candidate:\n%s \n" (string_of_cmd path);
        let precs = if Option.is_none hints
          then
            [wp path (Packet.to_test ~fvs:problem.fvs pkt')]
          else [wp path True]
        in
        acc @ List.map precs ~f:(inj_l acts))
    (* if prec = False then None else Some(prec, acts)) *)
  in
  let () = if params.debug then
      Printf.printf "The logical trace is: %s \n%!" (string_of_cmd trace) in
  let wp_time = Time.diff (Time.now ()) st in
  let model =
    List.find_map wp_phys_paths ~f:(fun (wp_phys, acts) ->
        if wp_phys = False then None else
          let () = if params.debug then Printf.printf "LOGWP %s\n => PHYSWP %s\n%!" (string_of_test log_wp) (string_of_test wp_phys) in
          if holes_of_test wp_phys = [] then
            (if params.debug then Printf.printf "no holes, so skipping\n%!";
             None)
          else
            let (res, time) = check params `MinSat (log_wp %=>% wp_phys) in
            data := {!data with
                     model_z3_time = Time.Span.(!data.model_z3_time + time);
                     model_z3_calls = !data.model_z3_calls + 1};
            match res with
            | None -> None
            | Some model -> Some (model, acts)
      )
  in
  data := {!data with search_wp_time = Time.Span.(!data.search_wp_time + wp_time)};
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
  let linst_edited =  Instance.update_list problem.log_inst problem.log_edits in
  let phys_edited = Instance.update_list problem.phys_inst problem.phys_edits in
  let (pkt',_), _, trace, actions = trace_eval_inst ~wide:StringMap.empty
      problem.log linst_edited (pkt,None) in
  let deletions = compute_deletions pkt problem in
  assert (List.length deletions = 0);
  data := {!data with interp_time = Time.Span.(!data.interp_time + (Time.diff (Time.now ()) interp_st)) };

  let () = if params.debug || params.interactive then
      Printf.printf "CE input: %s \n%!CE TRACE: %s\nCE output: %s\n%!"
        (Packet.string__packet pkt)
        (string_of_cmd trace)
        (Packet.string__packet pkt');
    if params.interactive then
      (Printf.printf "Press enter to solve for CE input-output pair\n";
       ignore (In_channel.(input_char stdin) : char option))
  in
  let cst = Time.now () in
  let cands = CandidateMap.apply_hints (`WithHoles deletions) `Exact hints actions problem.phys phys_edited in
  data := {!data with cand_time = Time.Span.(!data.cand_time +  Time.diff (Time.now ()) cst) };
  let wp_st = Time.now () in
  let wp_phys_paths =
    List.fold cands ~init:[] ~f:(fun acc (path, acts) ->
        let precs = if Option.is_none hints
                    then
                      (* [wp ~no_negations:false path (Packet.to_test ~fvs:problem.fvs pkt')] *)
                      wp_paths ~no_negations:false path (Packet.to_test ~fvs:problem.fvs pkt')
                      |> List.map ~f:snd
                    else [wp path True]
        in
        acc @ List.map precs ~f:(inj_l acts))
  in
  data := {!data with search_wp_time = Time.Span.(!data.search_wp_time + Time.diff (Time.now ()) wp_st)};
  let model =
    List.find_map wp_phys_paths ~f:(fun (wp_phys, acts) ->
        if wp_phys = False then
          None
        else
          let cdst = Time.now () in
          let condition = (Packet.to_test ~fvs:problem.fvs ~random_fill:false pkt %=>% wp_phys) in
          let c_dur = Time.diff (Time.now ()) cdst in
          if params.debug then
            Printf.printf "Checking \n%s  \n=> \n%s\n%!"
              (Packet.to_test ~fvs:problem.fvs ~random_fill:false pkt
               |> string_of_test)
              (string_of_test wp_phys);
          let h_st = Time.now() in
          if condition |> has_hole_test then
            let h_dur =  Time.diff (Time.now ()) h_st in
            let (res, dur) = check params `Sat condition in
            data := {!data with
                     model_holes_time = Time.Span.(!data.model_holes_time + h_dur);
                     model_z3_time = Time.Span.(!data.model_z3_time + dur);
                     model_z3_calls = !data.model_z3_calls + 1;
                     model_cond_time = Time.Span.(!data.model_cond_time + c_dur)
                    };
            begin match res with
              | None -> if params.debug then Printf.printf "no model\n%!";None
              | Some model -> Some (model, acts)
            end
          else None
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

let implements (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  (* let _ = Printf.printf "IMPLEMENTS on\n%!    ";
   *         List.iter fvs ~f:(fun (x,_) -> Printf.printf " %s" x);
   *         Printf.printf "\n%!" *)
  (* in *)
  let u_log,_ = problem.log |> (Instance.update_list problem.log_inst problem.log_edits
                                |> Instance.apply ~no_miss:params.do_slice `NoHoles `Exact)
  in
  let u_rea,_ = problem.phys |> Instance.apply `NoHoles `Exact (Instance.update_list problem.phys_inst problem.phys_edits) in
  if params.debug then
    Printf.printf "-------------------------------------------\n%s \n???====?=====????\n %s\n-------------------------------------\n%!"
      (string_of_cmd u_log) (string_of_cmd u_rea);
  let st_mk_cond = Time.now () in
  let condition = equivalent problem.fvs u_log u_rea in
  let nd_mk_cond = Time.now () in
  let mk_cond_time = Time.diff nd_mk_cond st_mk_cond in
  let cv_st = Time.now () in
  let model_opt, z3time = check_valid params condition in
  let cv_nd = Time.now () in
  data := {!data with check_valid_time = Time.Span.(!data.check_valid_time + Time.diff cv_nd cv_st)};
  let pkt_opt = match model_opt with
    | None  -> if params.debug then Printf.printf "++++++++++valid+++++++++++++\n%!";
      `Yes
    | Some x ->
      let pce = Packet.from_CE x |> Packet.un_SSA in
      if params.debug || params.interactive then
        Printf.printf "----------invalid----------------\n%! CE = %s\n%!" (Packet.string__packet pce)
    ; `NoAndCE pce
  in
  data := {!data with
           eq_time = Time.Span.(!data.eq_time + z3time);
           eq_num_z3_calls = !data.eq_num_z3_calls + 1;
           make_vc_time = Time.Span.(!data.make_vc_time + mk_cond_time);
           tree_sizes = num_nodes_in_test condition :: !data.tree_sizes
          };
  pkt_opt


(** solves the inner loop **)
let rec solve_concrete
    ?packet:(packet=None)
    (data : ProfData.t ref)
    (params : Parameters.t)
    (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
    (problem : Problem.t)
  : (Edit.t list) =
  let values = multi_ints_of_cmd problem.log |> List.map ~f:(fun x -> Int x) in
  let pkt = packet |> Option.value ~default:(Packet.generate problem.fvs ~values) in
  let model_finder = if params.widening
    then get_one_model_edit
    else get_one_model_edit_no_widening in
  let st = Time.now () in
  match model_finder pkt data params hints problem with
  | None -> Printf.sprintf "Couldnt find a model" |> failwith
  | Some (model, action_map) ->
    data := {!data with model_search_time = Time.Span.(!data.model_search_time + Time.diff (Time.now()) st) };
    Edit.extract problem.phys model


(* The truth of a slice implies the truth of the full programs when
 * the inserted rules are disjoint with every previous rule (i.e. no overlaps or deletions)
 * Here we only check that the rules are exact, which implies this property given the assumption that every insertion is reachable
*)
let slice_conclusive _ _ (problem : Problem.t) =
  List.length problem.phys_edits > 0 &&
  List.for_all (problem.log_edits @ problem.phys_edits)
    ~f:(function | Add (_, (ms, _,_)) ->
        List.for_all ms ~f:(function | Exact _ -> true | _ -> false) (* need to check disjointness with previous rows here *)
                 | Del _ -> false)

let slice (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  if params.debug then
    Printf.printf "///////////////////////////SLICE (%d) (%d)///////////////////////////////\n"
      (List.length problem.log_edits) (List.length problem.phys_edits);
  let log_inst_slice = Instance.update_list Instance.empty problem.log_edits in
  let phys_inst_slice = Instance.update_list Instance.empty problem.phys_edits in
  let log_inst = Instance.overwrite problem.log_inst log_inst_slice in
  let phys_inst = Instance.overwrite problem.phys_inst phys_inst_slice in
  {problem with log_inst; phys_inst;
                log_edits = []; phys_edits = [] }

let cegis ~iter
    (params : Parameters.t)
    (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
    (data : ProfData.t ref)
    (problem : Problem.t)
  : (Edit.t list) option
  =
  let rec loop (params : Parameters.t) (problem : Problem.t) : (Edit.t list) option =
    if params.interactive then
      (Printf.printf "Press enter to loop again\n%!";
       ignore(Stdio.In_channel.(input_char stdin) : char option));
    if params.debug || params.interactive then
      Printf.printf "======================= LOOP (%d, %d) =======================\n%!%s\n%!" (iter) (params.gas) (Problem.to_string problem);
    let imp_st = Time.now () in (* update data after setting time start  *)
    let res = (if params.fastcx
               then
                 ( if params.debug then Printf.printf "getting fast cx\n";
                   List.hd_exn problem.log_edits
                   |> get_cex params data problem.log problem.log_inst problem.phys problem.phys_inst)
               else (if params.do_slice
                     then implements params data (slice params data problem)
                     else implements params data problem)) in
    let params = {params with fastcx = false} in
    let imp_dur = Time.(diff (now()) imp_st) in
    data := {!data with impl_time = Time.Span.(!data.impl_time + imp_dur)};
    let do_cex counter =
      if params.interactive then
        (Printf.printf "Press enter to resolve counterexample\n%!";
         ignore(Stdio.In_channel.(input_char stdin) : char option));
      if params.gas = 0 then failwith "RAN OUT OF GAS" else
        let pedits = solve_concrete ~packet:(Some counter) data params hints problem in
        if List.length pedits = 0
        then failwith ("Could not make progress on edits ")
        else loop
            { params with gas = params.gas - 1 }
            { problem with phys_edits = pedits }
    in
    match res with
    | `Yes ->
      if params.do_slice && not (slice_conclusive params data problem)
      then
        match implements params data problem with
        | `Yes -> Some problem.phys_edits
        | `NoAndCE counter -> do_cex counter
      else Some problem.phys_edits
    | `NoAndCE counter -> do_cex counter
  in
  loop params problem

let synthesize ~iter (params : Parameters.t) (hints : (CandidateMap.trace -> CandidateMap.trace list) option) (data : ProfData.t ref)  (problem : Problem.t) =
  let start = Time.now () in
  let pedits = cegis ~iter params hints data problem in
  let pedits_out = Option.value ~default:[] pedits in
  let stop = Time.now() in
  if params.debug || params.interactive then
    Printf.printf "\nSynthesized Program (%d edits made):\n%s\n\n%!"
      (List.length pedits_out)
      (Instance.apply `NoHoles `Exact (Instance.update_list problem.phys_inst pedits_out) problem.phys |> fst |> string_of_cmd);
  data := {!data with
           log_inst_size = List.length problem.log_edits + (Instance.size problem.log_inst);
           phys_inst_size = Instance.size problem.phys_inst;
           time = Time.diff stop start};
  pedits_out
