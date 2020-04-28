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
    | Apply (name, keys, acts, dflt)
      -> Apply (name
               , keys
               , List.map acts ~f:(fun (data, a) -> (data, complete_aux a ~falsify))
               , complete_aux ~falsify dflt)
  in
  complete_aux ~falsify cmd

let complete cmd = complete_inner ~falsify:true cmd

let compute_deletions pkt (problem : Problem.t) =
  let open Problem in
  get_nd_hits (phys problem) (phys_inst problem) pkt

let get_one_model_edit
    (pkt : Packet.t)
    (data : ProfData.t ref)
    (params : Parameters.t)
    (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
    (problem : Problem.t)
  =
  let linst_edited = Problem.log_edited_instance problem in
  let pinst_edited = Problem.phys_edited_instance problem in
  let (pkt',_), wide, trace, actions = trace_eval_inst ~wide:StringMap.empty (Problem.log problem) linst_edited (pkt,None) in
  let deletions = compute_deletions pkt problem in
 let st = Time.now () in
  let cands = CandidateMap.apply_hints (`WithHoles deletions) `Range hints actions (Problem.phys problem) pinst_edited in
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
  let linst_edited =  Problem.log_edited_instance problem in
  let phys_edited = Problem.phys_edited_instance problem in
  let (pkt',_), _, trace, actions = trace_eval_inst ~wide:StringMap.empty
      (Problem.log problem) linst_edited (pkt,None) in
  let deletions = compute_deletions pkt problem in
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
  let cands = CandidateMap.apply_hints (`WithHoles deletions) `Exact hints actions (Problem.phys problem) phys_edited in
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
                        |> wp_paths ~no_negations:false params path
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
  let log = Problem.log_gcl_program problem in
  let phys = Problem.phys_gcl_program problem in
  if params.debug then
    Printf.printf "-------------------------------------------\n%s \n???====?=====????\n %s\n-------------------------------------\n%!"
      (string_of_cmd log) (string_of_cmd phys);
  let st_mk_cond = Time.now () in
  let condition = equivalent Problem.(fvs problem) log phys in
  ProfData.update_time !data.make_vc_time st_mk_cond;
  let cv_st = Time.now () in
  let model_opt, z3time = check_valid params condition in
  ProfData.update_time !data.check_valid_time cv_st;
  ProfData.update_time_val !data.eq_time z3time;
  !data.tree_sizes :=  num_nodes_in_test condition :: !(!data.tree_sizes);
  let pkt_opt = match model_opt with
    | None  -> if params.debug then Printf.printf "++++++++++valid+++++++++++++\n%!";
      `Yes
    | Some x ->
       let in_pkt, out_pkt = Packet.extract_inout_ce x in
       let remake = Packet.make ~fvs:(Problem.fvs problem |> Some) in
       let in_pkt' = if params.widening then in_pkt else remake in_pkt in
       let out_pkt' = if params.widening then out_pkt
                      else eval_act (Problem.log_gcl_program problem) in_pkt in
       if params.debug || params.interactive then
         Printf.printf "----------invalid----------------\n%! CE_in = %s\n CE_out = %s\n%!"
           (Packet.string__packet in_pkt')
           (Packet.string__packet out_pkt')
       ; `NoAndCE (in_pkt', out_pkt')
  in
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
  let res =
    FastCX.(Problem.(
              hits_list_pred data (log problem) (log_inst problem) (log_edits problem)
            %<=>% hits_list_pred data (phys problem) (phys_inst problem) (phys_edits problem)))
    |> check_valid params
    |> fst
    |> Option.is_none
  in
  ProfData.update_time !data.check_sliceable_time st;
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
      Printf.printf "======================= LOOP (%d, %d) =======================\n%!%s\n%!" (iter) (params.gas) (Problem.to_string problem);
    let imp_st = Time.now () in (* update data after setting time start  *)
    let res = ((* if params.fastcx
                * then
                *   ( if params.debug then Printf.printf "getting fast cx\n";
                *     List.hd_exn problem.log_edits
                *     |> get_cex params data problem.log problem.log_inst problem.phys problem.phys_inst)
                * else *) (if params.do_slice
                     then implements params data (Problem.slice problem)
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
       |> Problem.phys_gcl_program
       |> string_of_cmd);
  !data.log_inst_size := Instance.size (Problem.log_edited_instance problem);
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


let edit_domain (prog : cmd) (edit : Edit.t) =
  get_schema_of_table (Edit.table edit) prog
  |> Option.value_exn
  |> table_vars

let edits_domain (prog : cmd) (edits : Edit.t list) : (string * size) list =
  let open List in
  edits >>= edit_domain prog


let get_model (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) : (value StringMap.t option) =
  let (in_pkt, out_pkt) = Problem.cexs problem |> List.hd_exn in
  let st = Time.now () in
  let deletions = [] in
  let phys = Problem.phys_gcl_holes problem (`WithHoles deletions) (if params.widening then `Range else `Exact) in
  ProfData.update_time !data.model_holes_time st;
  let st = Time.now () in
  let fvs = List.(free_vars_of_cmd phys
                  |> filter ~f:(fun x -> exists (Problem.fvs problem) ~f:(Stdlib.(=) x))) in
  (* let fvs = problem.fvs in *)
  let in_pkt_form, out_pkt_form = Packet.to_test in_pkt ~fvs, Packet.to_test out_pkt ~fvs in
  let wp_list =
    if params.monotonic then
      wp_paths ~no_negations:true {params with monotonic = false} phys out_pkt_form |> List.map ~f:snd
    else
      [wp phys out_pkt_form]
  in
  ProfData.update_time !data.search_wp_time st;
  let model =
    List.find_map wp_list
      ~f:(fun in_pkt_wp ->
        if in_pkt_wp = False then None else
        let st = Time.now() in
        let spec = in_pkt_form %=>% in_pkt_wp in
        let wf_holes = List.fold (Problem.phys problem |> get_tables_actsizes) ~init:True
                         ~f:(fun acc (tbl,num_acts) ->
                           acc %&%
                             (Hole("?ActIn"^tbl,max (log2 num_acts) 1)
                              %<=% mkVInt(num_acts-1,max (log2 num_acts) 1)))
                       %&%
                         ( if params.widening then
                             List.fold (holes_of_test in_pkt_wp)  ~init:True
                               ~f:(fun acc (hole, sz) ->
                                 match String.chop_suffix hole ~suffix:"_mask" with
                                 | Some hole_val ->
                                    Hole(hole_val,sz) %=% (mkMask (Hole(hole_val,sz)) (Hole(hole, sz)))
                                    %&% ((Hole(hole,sz) %=% mkVInt(0, sz))
                                         %+% (Hole(hole,sz) %=% Value(Int(Bigint.((pow (of_int 2) (of_int sz)) - one), sz))))
                                | None -> acc

                               )
                           else True
                         )

        in
        let edit_domain = Problem.(edits_domain (log problem) (log_edits problem)) in
        if params.debug then
          Printf.printf "%s vars in log edits domain\n%!" (List.map edit_domain ~f:fst |> List.reduce_exn ~f:(Printf.sprintf "%s %s"));
        let injection_restriction =
          if params.injection then
            List.fold (Problem.phys problem |> get_tables_vars) ~init:StringMap.empty
              ~f:(fun acc (tbl,vars) ->
                if params.debug then
                  Printf.printf "%s vars in table %s\n%!" (List.map vars ~f:fst |> List.reduce_exn ~f:(Printf.sprintf "%s %s")) tbl;
                  if List.exists vars ~f:(fun (v,_) -> List.exists edit_domain ~f:(fun (v',_) -> v = v'))
                  then
                    acc
                  else
                    StringMap.set acc ~key:("?AddRowTo"^tbl) ~data:(mkInt(0,1))
              )
          else
            StringMap.empty
        in
        let condition =
          fixup_test injection_restriction ((wf_holes %&% Problem.model_space problem) %&% spec)
        in
        ProfData.update_time !data.model_cond_time st;
        if params.debug || params.interactive then
                    Printf.printf "phys to check\n--\nInput:%s\n--\n%s\n--\nOutput:%s\n--\nWP:\n%s--\n%!"
                      (Packet.string__packet in_pkt)
                      (string_of_cmd phys)
                      (Packet.string__packet out_pkt)
                      ("OMITTED" (*string_of_test spec*));
        if condition = False
        then None
        else
          let model, dur = check_sat params condition in
          ProfData.update_time_val !data.model_z3_time dur;
          ProfData.incr !data.model_z3_calls;
          !data.tree_sizes := num_nodes_in_test condition :: !(!data.tree_sizes) ;
          Option.(model >>| complete_model (holes_of_test condition))
      )
  in
  if params.interactive then begin
      ignore(Stdio.In_channel.(input_char stdin) : char option)
    end;
  model


let holes_for_table table phys =
  match get_schema_of_table table phys with
  | None -> failwith ""
  | Some (ks, acts, _) ->
     List.map ks ~f:(fun (k,_) -> "?" ^ k )
     @ List.(acts >>= fun (params,_) ->
             params >>| fst )

let minimize_model (model : value StringMap.t) (phys : cmd) : value StringMap.t =
  StringMap.keys model
  |> List.filter ~f:(String.is_prefix ~prefix:"?AddRowTo")
  |> List.fold ~init:model
       ~f:(fun model key ->
         match StringMap.find model key with
         | Some Int(v,_) when v = Bigint.zero ->
            let table = String.chop_prefix_exn key ~prefix:"?AddRowTo" in
            holes_for_table table phys
            |> List.fold ~init:model ~f:(StringMap.remove)
         | _ -> model
       )



let get_cex (params : Parameters.t) (data :  ProfData.t ref) (problem : Problem.t)
    : [> `NoAndCE of Packet.t * Packet.t | `Yes] =
  if params.fastcx then begin
      match FastCX.get_cex params data problem with
      | `Yes ->
         if params.debug then
           Printf.printf "New rule is not reachable\n%!";
         `Yes
      | `NotFound ->
         if params.debug then
           Printf.printf "No cex to be found rapidly, check full equivalence\n%!";
         implements params data problem
      | `NoAndCE counter ->
         `NoAndCE counter
    end
  else
    if params.do_slice && not( List.is_empty (Problem.phys_edits problem)) then
      match implements params data (Problem.slice problem) with
      | `NoAndCE counter -> `NoAndCE counter
      | `Yes when slice_conclusive params data problem -> `Yes
      | `Yes -> implements params data problem
    else
      implements params data problem
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


let rec cegis_math params (data : ProfData.t ref) (problem : Problem.t) =
  let st = Time.now () in
  let cex = get_cex params data problem in
  ProfData.update_time !data.impl_time st;
  match cex with
  | `Yes ->
     if params.interactive then begin
       Printf.printf "%s\n%!" (Problem.phys_gcl_program problem |> string_of_cmd);
       ignore(Stdio.In_channel.(input_char stdin) : char option)
       end;
     Problem.phys_edits problem |> Some
  | `NoAndCE counter ->
     let params = {params with fastcx = false} in
     let f = liftPair ~f:Packet.equal ~combine:(&&) counter in
     if List.exists ~f (Problem.cexs problem) then begin
         if params.debug then
           Printf.eprintf "Duplicated counter example. IN: %s -------> OUT: %s\n%!"
           (fst counter |> Packet.string__packet)
           (snd counter |> Packet.string__packet);
         None
       end
     else
       solve_math params data
         (Problem.add_cex problem counter)

and solve_math (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  (* if params.debug then
   *   Printf.printf "+Model Space+\n%!"; *)
  if Problem.model_space problem = True
     || (check_sat params (Problem.model_space problem) |> fst |> Option.is_some)
  then begin
      let st = Time.now () in
      let model_opt = get_model params data problem in
      ProfData.update_time !data.model_search_time st;
      match model_opt with
      | None ->
         if params.debug || params.interactive then
           Printf.printf "No model could be found\n%!";
         if params.interactive then
           ignore(Stdio.In_channel.(input_char stdin) : char option);
         if params.injection then
           solve_math {params with injection = false} data problem
         else
           None
      | Some model ->
         let model = minimize_model model (Problem.phys problem) in
         if List.exists (Problem.attempts problem) ~f:(StringMap.equal veq model)
         then begin
             Printf.printf "ALREADY EXPLORED\n %s \n%!" (string_of_map model);
             failwith ""
           end
         else
           let problem = Problem.add_attempt problem model in
           let () = if params.debug then Printf.printf "=======================\n      ATTEMPTS:\n======================-%s\n=======================\n   Current Attempt\n=======================\n%s\n=======================\n%!"
                     (Problem.attempts_to_string problem)
                     (string_of_map model); in
           (* assert (Problem.num_attempts problem <= 1); *)
           let es = Edit.extract (Problem.phys problem) model in
           if params.debug then begin
               Printf.printf "***Edits***\n%!";
               List.iter es ~f:(fun e -> Printf.printf "%s\n%!" (Edit.to_string e));
               Printf.printf "***     ***\n"
             end;
           if List.length es = 0 then None else
           match
             cegis_math params data (Problem.(append_phys_edits problem es |> reset_model_space |> reset_attempts))
           with
           | None ->
              let model_space = Problem.model_space problem %&% negate_model model in
              if params.debug || params.interactive then
                Printf.printf "Backtracking with\n%s\nadded to\n%s\n%!" (string_of_test (negate_model model)) (string_of_test (Problem.model_space problem));
              if params.interactive then
                ignore(Stdio.In_channel.(input_char stdin) : char option);
            ProfData.incr !data.num_backtracks;
            solve_math params data (Problem.set_model_space problem model_space)
           | Some es -> Some es
    end
  else begin
      Printf.printf "Exhausted the Space\n%!";
      None
    end
