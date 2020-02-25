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
      else let i = random_int_nin (List.map ~f:fst domain) in
           comp x (Value (Int (i,sz)))
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
           let i = random_int_nin (List.map ~f:fst domain) in
           let sz = int_of_float (2. ** float_of_int i) in
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



let print_instance label linst =
  Printf.printf "%s instance is \n" label;
  StringMap.iteri linst ~f:(fun ~key ~data ->
      Printf.printf "%s -> \n" key;
      List.iter data ~f:(fun (keys,action) ->
          List.iter keys ~f:(fun k -> Printf.printf ",%s" (string_of_expr k));
          Printf.printf "  ---> %d \n%!" action)
      )

let get_one_model_edit
      (pkt : Packet.t)
      (data : ProfData.t ref)
      (params : Parameters.t)
      (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
      (problem : Problem.t)
  =
  (* print_instance "Logical" (apply_edit linst ledit);
   * print_instance "Physical" pinst; *)
  let linst_edited = Instance.update_list problem.log_inst problem.edits in
  let (pkt',_), wide, trace, actions = trace_eval_inst ~wide:StringMap.empty problem.log linst_edited (pkt,None) in
  let st = Time.now () in
  let cands = CandidateMap.apply_hints `Range hints actions problem.phys problem.phys_inst in
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
  let _ = if params.debug then
            Printf.printf "The logical trace is: %s \n%!" (string_of_cmd trace) in
  let wp_time = Time.diff (Time.now ()) st in
  let model =
    List.find_map wp_phys_paths ~f:(fun (wp_phys, acts) ->
        if wp_phys = False then None else
          let _ = if params.debug then Printf.printf "LOGWP %s\n => PHYSWP %s\n%!" (string_of_test log_wp) (string_of_test wp_phys) in
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
  let linst_edited =  Instance.update_list problem.log_inst problem.edits in
  let (pkt',_), _, trace, actions = trace_eval_inst ~wide:StringMap.empty
                                      problem.log linst_edited (pkt,None) in
  let _ = if params.debug || params.interactive then
            Printf.printf "CE input: %s \n%!CE TRACE: %s\nCE output: %s\n%!"
              (Packet.string__packet pkt)
              (string_of_cmd trace)
              (Packet.string__packet pkt');
          if params.interactive then
            (Printf.printf "Press enter to solve for CE input-output pair\n";
             In_channel.(input_char stdin) |> ignore
            )
                          
  in
  let st = Time.now () in
  let cands = CandidateMap.apply_hints `Exact hints actions problem.phys problem.phys_inst in
  let wp_phys_paths =
    List.fold cands ~init:[] ~f:(fun acc (path, acts) ->
        let precs = if Option.is_none hints
                    then
                      [wp path (Packet.to_test ~fvs:problem.fvs pkt')]
                    else [wp path True]
        in
        acc @ List.map precs ~f:(inj_l acts))
  in
  let wp_time = Time.diff (Time.now ()) st in
  let model =
    List.find_map wp_phys_paths ~f:(fun (wp_phys, acts) ->
        if wp_phys = False then
          None
        else
          let condition = (Packet.to_test ~fvs:problem.fvs ~random_fill:false pkt %=>% wp_phys) in
          if params.debug then Printf.printf "Checking %s  => %s\n%!" (Packet.to_test ~fvs:problem.fvs ~random_fill:false pkt |> string_of_test) (string_of_test wp_phys);
          if holes_of_test condition = [] then None else
            let (res, time) = check params `Sat condition in
            data := {!data with
                      model_z3_time = Time.Span.(!data.model_z3_time + time);
                      model_z3_calls = !data.model_z3_calls + 1
                    };
            match res with
            | None -> if params.debug then Printf.printf "no model\n%!";None
            | Some model -> Some (model, acts)
      )
  in
  data := {!data with search_wp_time = Time.Span.(!data.search_wp_time + wp_time)};
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
  List.dedup_and_sort ~compare (free_vars_of_cmd cmd @ fvs)
  |> symbolic_pkt
  |> wp cmd
  
let implements (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) =
  (* let _ = Printf.printf "IMPLEMENTS on\n%!    ";
   *         List.iter fvs ~f:(fun (x,_) -> Printf.printf " %s" x);
   *         Printf.printf "\n%!" *)
  (* in *)
  let u_log,_ = problem.log |> Instance.apply `NoHoles `Exact (Instance.update_list problem.log_inst problem.edits)  in
  let u_rea,_ = problem.phys |> Instance.apply `NoHoles `Exact problem.phys_inst in
  let st_mk_cond = Time.now () in
  let condition = equivalent problem.fvs u_log u_rea in
  let nd_mk_cond = Time.now () in
  let mk_cond_time = Time.diff nd_mk_cond st_mk_cond in
  let model_opt, z3time = check_valid params condition in
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
            make_vc_time = Time.Span.(!data.eq_time + mk_cond_time);
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
        : (Instance.t) =
  let values = multi_ints_of_cmd problem.log |> List.map ~f:(fun x -> Int x) in
  let pkt = packet |> Option.value ~default:(Packet.generate problem.fvs ~values) in
  let model_finder = if params.widening then get_one_model_edit else get_one_model_edit_no_widening in
  match model_finder pkt data params hints problem with
  | None -> Printf.sprintf "Couldnt find a model" |> failwith
  | Some (model, action_map) ->
     match Instance.fixup_edit (check params `Sat) params model action_map problem.phys problem.phys_inst with
     | `Ok pinst' -> pinst'
     | `Conflict pinst' ->
        Printf.printf "BACKTRACKING\n%!";
        (* failwith "BACKTRACKING" *)
        pinst'
  
let cegis ~iter
      (params : Parameters.t)
      (hints : (CandidateMap.trace -> CandidateMap.trace list) option)
      (data : ProfData.t ref)
      (problem : Problem.t) =
  let rec loop (params : Parameters.t) (problem : Problem.t) =
    if params.interactive then
      (Printf.printf "Press enter to loop again\n%!";
       Stdio.In_channel.(input_char stdin) |> ignore);    
    if params.debug || params.interactive then
      Printf.printf "======================= LOOP (%d, %d) =======================\n%!%s\n%!" (iter) (params.gas) (Problem.to_string problem);
    let res = implements params data problem in
    match res with
    | `Yes ->
       Some problem.phys_inst
    | `NoAndCE counter ->
       if params.interactive then
         (Printf.printf "Press enter to resolve counterexample\n%!";
          Stdio.In_channel.(input_char stdin) |> ignore);
       if params.gas = 0 then failwith "RAN OUT OF GAS" else
         let st = Time.now() in
         let pinst' = solve_concrete ~packet:(Some counter) data params hints problem in
         let dur = Time.diff (Time.now()) st in
         data := {!data with model_search_time = Time.Span.(!data.model_search_time + dur) };
         if StringMap.equal (=) problem.phys_inst pinst'
         then failwith ("Could not make progress on edits ")
         else loop
                {params with gas = params.gas-1}
                {problem with phys_inst = pinst'}
  in
  let pinst' = loop params problem in
  pinst'
    
let synthesize ~iter (params : Parameters.t) (hints : (CandidateMap.trace -> CandidateMap.trace list) option) (data : ProfData.t ref)  (problem : Problem.t) =
  let start = Time.now () in
  let pinst' = cegis ~iter params hints data problem in
  let pinst_out = Option.value ~default:(StringMap.empty) pinst' (*|> complete*) in
  let stop = Time.now() in
  Printf.printf "\nSynthesized Program:\n%s\n\n%!"
    (Instance.apply `NoHoles `Exact pinst_out problem.phys |> fst |> string_of_cmd);
  data := {!data with
            log_inst_size = List.length problem.edits + (Instance.size problem.log_inst);
            phys_inst_size = Instance.size problem.phys_inst;
            time = Time.diff stop start};
  pinst_out
