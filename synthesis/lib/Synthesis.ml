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



                                 

     
                                  
(** Solves the inner loop of the cegis procedure. 
 * pre-condition: pkt is at an ingress host 
**)
(* let get_one_model ?fvs:(fvs = []) mySolver (pkt : Packet.t) (logical : cmd) (phys : cmd) =
 *   let (pkt',_), _ = trace_eval logical (pkt,None) |> Option.value_exn in
 *   (\* let _ = Printf.printf "input: %s\n output: %s\n%!" (Packet.string__packet pkt) (Packet.string__packet pkt') in  *\)
 *   let st = Time.now () in
 *   let phi = Packet.to_test ~fvs pkt' in
 *   let wp_phys_paths = wp_paths phys phi |> List.filter_map ~f:(fun (_,pre) -> pre <> False) in
 *   let wp_time = Time.diff (Time.now ()) st in
 *   (\* if wp_phys_paths = [] then failwith "No feasible paths!" else
 *    *   Printf.printf "%d feasible paths\n\n%!" (List.length wp_phys_paths);
 *    * Printf.printf "------------------------------------------------\n";
 *    * List.iter wp_phys_paths ~f:(fun path ->
 *    *     Printf.printf "%s\n\n%!" (string_of_test path)
 *    *   )
 *    * ; Printf.printf "----------------------------------------------------\n%!"
 *    * ; *\)
 *     let time_spent_in_z3 = ref Time.Span.zero in
 *     let num_calls_to_z3 = ref 0 in
 *     let model =
 *       List.find_map wp_phys_paths ~f:(fun wp_phys ->
 *           let _ = Printf.printf "PHYSICAL WEAKEST_PRECONDITION:\n%s\n\nOF PROGRAM:\n%s\n%!"
 *                     (string_of_test wp_phys)
 *                     (string_of_cmd phys)
 *           in
 *           if wp_phys = False
 *           then (Printf.printf "-- contradictory WP\n%!"; None (\*find_match rest_paths*\))
 *           else
 *             let condition = substV wp_phys pkt in
 *             let _ = Printf.printf "CONDITION: \n%s\n%!" (string_of_test condition) in
 *             num_calls_to_z3 := !num_calls_to_z3 + 1;
 *             match check mySolver `Sat condition with
 *             | (None, d) -> Printf.printf "unsolveable!\n%!";
 *                            time_spent_in_z3 := Time.Span.(!time_spent_in_z3 + d);
 *                            None
 *                              
 *             | Some model, d ->
 *                time_spent_in_z3 := Time.Span.(!time_spent_in_z3 + d);
 *                Some model
 *         )
 *     in
 *     Printf.printf "Took %d reps over %s to find model\n!"
 *       (!num_calls_to_z3)
 *       (Time.Span.to_string !time_spent_in_z3)
 *     ; model, !time_spent_in_z3, !num_calls_to_z3, wp_time *)


let print_instance label linst =
  Printf.printf "%s instance is \n" label;
  StringMap.iteri linst ~f:(fun ~key ~data ->
      Printf.printf "%s -> \n" key;
      List.iter data ~f:(fun (keys,action) ->
          List.iter keys ~f:(fun k -> Printf.printf ",%s" (string_of_expr k));
          Printf.printf "  ---> %d \n%!" action)
      )

              

let get_one_model_edit
      ?fvs:(fvs = [])
      ~hints (pkt : Packet.t)
      mySolver
      (lline : cmd) (linst : Instance.t) (ledits : Edit.t list)
      (pline : cmd) pinst
  =
  (* print_instance "Logical" (apply_edit linst ledit);
   * print_instance "Physical" pinst; *)
  let time_spent_in_z3, num_calls_to_z3 = (ref Time.Span.zero, ref 0) in
  let (pkt',_), wide, trace, actions = trace_eval_inst ~wide:StringMap.empty lline (Instance.update_list linst ledits) (pkt,None) in
  let st = Time.now () in
  (* let phi = Packet.to_test ~fvs pkt' in *)
  let cands = CandidateMap.apply_hints `Range hints actions pline pinst in
  (* let _ = Printf.printf "Candidate programs:\n%!";
   *         List.iter cands ~f:(fun (c,_) -> Printf.printf "\n%s\n%!" (string_of_cmd c));
   *         Printf.printf "\n" in *)
  let _ = Printf.printf "WIDEST post condition: %s \n%!" (Packet.test_of_wide ~fvs wide |> string_of_test) in
  let log_wp = wp trace True in
  let wp_phys_paths =
    List.fold cands ~init:[] ~f:(fun acc (path, acts) ->
        Printf.printf "Candidate:\n%s \n" (string_of_cmd path);
        let precs = if Option.is_none hints
                    then
                      (* [wp path (Packet.to_test ~fvs pkt')] *)
                      wp_paths ~no_negations:true path (Packet.test_of_wide ~fvs wide) (* |> List.map ~f:(snd) *)
                                                                     (* Packet.to_test ~fvs pkt' *)
                      |> List.map ~f:(fun (trace, _) ->
                             let wide_test = Packet.test_of_wide ~fvs wide in
                             let wpt = wp trace wide_test in
                             Printf.printf "wide packet:\n %s \n%!" (string_of_test wide_test);
                             Printf.printf "Candidate :\n %s\n%!" (string_of_cmd trace);
                             Printf.printf "WP:\n %s\n%!" (string_of_test wpt);
                             wpt)
                    else [wp path True]
        in
        acc @ List.map precs ~f:(inj_l acts))
              (* if prec = False then None else Some(prec, acts)) *)
  in
  let _ = Printf.printf "The logical trace is: %s \n%!" (string_of_cmd trace) in
  let wp_time = Time.diff (Time.now ()) st in
  let model =
    List.find_map wp_phys_paths ~f:(fun (wp_phys, acts) ->
        if wp_phys = False then None else
          let _ = Printf.printf "LOGWP %s\n => PHYSWP %s\n%!" (string_of_test log_wp) (string_of_test wp_phys) in
          (* let (res, time) = check mySolver `Sat (substV wp_phys pkt) in *)
          if holes_of_test wp_phys = [] then
            (Printf.printf "no holes, so skipping\n%!";
            None)
          else
            let (res, time) = check mySolver `MinSat (log_wp %=>% wp_phys) in
            time_spent_in_z3 := Time.Span.(!time_spent_in_z3 + time);
            num_calls_to_z3 := !num_calls_to_z3 + 1;
          match res with
          | None -> None
          | Some model -> Some (model, acts)
      )
  in
  (model, !time_spent_in_z3, !num_calls_to_z3, wp_time)

let get_one_model_edit_no_widening
      ?fvs:(fvs = [])
      ~hints (pkt : Packet.t)
      mySolver
      (lline : cmd) (linst : Instance.t) (ledits : Edit.t list)
      (pline : cmd) pinst
  =
  (* print_instance "Logical" (apply_edit linst ledit);
   * print_instance "Physical" pinst; *)
  let time_spent_in_z3, num_calls_to_z3 = (ref Time.Span.zero, ref 0) in
  let (pkt',_), _, _, actions = trace_eval_inst ~wide:StringMap.empty lline (Instance.update_list linst ledits) (pkt,None) in
  let st = Time.now () in
  (* let phi = Packet.to_test ~fvs pkt' in *)
  let cands = CandidateMap.apply_hints `Exact hints actions pline pinst in
  (* let _ = Printf.printf "Candidate programs:\n%!";
   *         List.iter cands ~f:(fun (c,_) -> Printf.printf "\n%s\n%!" (string_of_cmd c));
   *         Printf.printf "\n" in *)
  let wp_phys_paths =
    List.fold cands ~init:[] ~f:(fun acc (path, acts) ->
        Printf.printf "Candidate:\n%s \n" (string_of_cmd path);
        let precs = if Option.is_none hints
                    then
                      (* [wp path ~no_negations:false (Packet.to_test ~fvs pkt')] *)
                      let _ = Printf.printf "out packet = %s\n%!" (Packet.string__packet pkt') in
                      [wp path (Packet.to_test ~fvs ~random_fill:true pkt')]
                      |> List.filter_map ~f:(fun (wp) ->
                             if wp = False then None else begin
                                 Printf.printf "pkt': %s\n" (pkt' |> Packet.to_test ~random_fill:true ~fvs |> string_of_test);
                                 (* Printf.printf "trace: %s\n" (string_of_cmd path); *)
                                 Printf.printf "WP: %s\n\n" (string_of_test wp);
                                 Some wp
                               end)
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
          let condition =  (Packet.to_test ~fvs ~random_fill:true pkt %=>% wp_phys) in
          let _ = Printf.printf "\nsubstituting:\n%s\ninto\n%s\nto get\n%s\n\n%!"                    
                    (Packet.string__packet pkt)
                    (string_of_test (wp_phys))
                    (string_of_test condition)
          in
          if holes_of_test condition = [] then None else
            let (res, time) = check mySolver `Sat condition in
            (* let (res, time) = check mySolver `MinSat (substV wp_phys pkt) in *)
            time_spent_in_z3 := Time.Span.(!time_spent_in_z3 + time);
            num_calls_to_z3 := !num_calls_to_z3 + 1;
            match res with
            | None -> Printf.printf "no model\n%!";None
            | Some model -> Some (model, acts)
      )
  in
  (model, !time_spent_in_z3, !num_calls_to_z3, wp_time)
    
  

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
  
let implements fvs mySolver (logical : cmd) (linst : Instance.t) (ledits : Edit.t list) (real : cmd) (pinst : Instance.t) =
  (* let _ = Printf.printf "IMPLEMENTS on\n%!    ";
   *         List.iter fvs ~f:(fun (x,_) -> Printf.printf " %s" x);
   *         Printf.printf "\n%!" *)
  (* in *)
  let u_log,_ = logical |> Instance.apply `NoHoles `Exact (Instance.update_list linst ledits)  in
  let u_rea,_ = real |> Instance.apply `NoHoles `Exact pinst in
  let _ = Printf.printf "Logical:\n%s\nPhysical:\n%s\n%!" (string_of_cmd u_log) (string_of_cmd u_rea) in  
  (* let st_log = Time.now () in
   * let log_wp  = symb_wp u_log ~fvs in
   * let log_time = Time.(diff (now()) st_log) in
   * let st_real = Time.now () in
   * let real_wp = symb_wp u_rea ~fvs in
   * let real_time = Time.(diff (now()) st_real) in *)
  (* Printf.printf "\n==== Checking Implementation =====\n%!\nLOGICAL \
   *                SPEC:\n%s\n\nREAL SPEC: \n%s\n\n%!"
   *   (string_of_test log_wp)
   *   (string_of_test real_wp); *)
  (* if log_wp = real_wp then Printf.printf "theyre syntactically equal\n%!";
   * let condition = log_wp %<=>% real_wp in *)
  let st_mk_cond = Time.now () in
  let condition = equivalent fvs u_log u_rea in
  let nd_mk_cond = Time.now () in
  let mk_cond_time = Time.diff nd_mk_cond st_mk_cond in
  let model_opt, z3time = check_valid mySolver condition in
  let pkt_opt = match model_opt with
    | None  -> Printf.printf "++++++++++valid+++++++++++++\n%!";
               `Yes
    | Some x ->
       let pce = Packet.from_CE x |> Packet.un_SSA in
       Printf.printf "----------invalid----------------\n%! CE = %s\n%!" (Packet.string__packet pce)
     ; `NoAndCE pce
  in pkt_opt, z3time, mk_cond_time, mk_cond_time, num_nodes_in_test condition

                                                         
(** solves the inner loop **)
let rec solve_concrete
          ?fvs:(fvs = [])
          ~widening
          mySolver
          ~hints
          ?packet:(packet=None)
          (logical : cmd) (linst : Instance.t) (edits : Edit.t list)
          (phys : cmd) (pinst : Instance.t)
        : (Instance.t * Time.Span.t * int * Time.Span.t) =
  let values = multi_ints_of_cmd logical |> List.map ~f:(fun x -> Int x) in
  let pkt = packet |> Option.value ~default:(Packet.generate fvs ~values) in
  let model_finder = if widening then get_one_model_edit else get_one_model_edit_no_widening in
  match model_finder ~fvs ~hints pkt mySolver logical linst edits phys pinst with
  | None, z3time, ncalls, _ ->
     Printf.sprintf "Couldnt find a model in %d calls and %f"
       ncalls (Time.Span.to_ms z3time)
     |> failwith
  | Some (model, action_map), z3time, ncalls, wp_time ->
     match Instance.fixup_edit model action_map phys pinst with
     | `Ok pinst' -> pinst', z3time, ncalls, wp_time
     | `Conflict pinst' ->
        Printf.printf "BACKTRACKING\n%!";
        (* failwith "BACKTRACKING" *)
        pinst', z3time, ncalls, wp_time
  
let cegis ~widening ?fvs:(fvs = []) ~hints ?gas:(gas=1000) ~iter mySolver (logical : cmd) linst (ledits : Edit.t list) (real : cmd) pinst =
  let fvs = if fvs = []
            then ((* Printf.printf "Computing the FVS!\n%!"; *)
              free_vars_of_cmd logical @ free_vars_of_cmd real)
            else fvs in
  let implements_time = ref Time.Span.zero in
  let implements_calls = ref 0 in
  let model_time = ref Time.Span.zero in
  let model_calls = ref 0 in
  let wp_time = ref Time.Span.zero in
  let log_wp_time = ref Time.Span.zero in
  let phys_wp_time = ref Time.Span.zero in
  let tree_sizes = ref [] in
  let rec loop gas pinst =
    Printf.printf "======================= LOOP (%d, %d) =======================\n%!" (iter) (gas);
    let (res, z3time, log_time, phys_time, treesize) =
      implements fvs mySolver logical linst ledits real pinst in
    implements_time := Time.Span.(!implements_time + z3time);
    implements_calls := !implements_calls + 1;
    log_wp_time := Time.Span.(!log_wp_time + log_time);
    phys_wp_time := Time.Span.(!phys_wp_time + phys_time);
    tree_sizes := treesize :: !tree_sizes;
    match Printf.printf "==++?+===++?\n%!"; res with
    | `Yes ->
       Some pinst
    | `NoAndCE counter ->
       if gas = 0 then failwith "RAN OUT OF GAS" else
         let (pinst', ex_z3_time, ncalls, wpt) =
           solve_concrete ~widening ~fvs ~hints ~packet:(Some counter) mySolver logical linst ledits real pinst in
         model_time := Time.Span.(!model_time + ex_z3_time);
         model_calls := !model_calls + ncalls;
         wp_time := Time.Span.(!wp_time + wpt);
         if StringMap.equal (=) pinst pinst'
         then failwith ("Could not make progress on edits ")
                                                                       
         else loop (gas-1) pinst'
  in
  let pinst' = loop gas pinst in
  (* Printf.printf "total z3 time to synthesize %s + %s = %s\n%!"
   *   (Time.Span.to_string !implements_time)
   *   (Time.Span.to_string !model_time)
   *   (Time.Span.(to_string (!implements_time + !model_time))); *)
  (pinst', !implements_time, !implements_calls, !model_time, !model_calls, !wp_time, !log_wp_time, !phys_wp_time, !tree_sizes)
    
let synthesize ~widening ?fvs:(fvs=[]) ~iter ?hints:(hints = None) ?gas:(gas = 1000)
      mySolver
      logical linst (ledits : Edit.t list) phys pinst =
  let start = Time.now () in
  let (pinst', checktime, checkcalls, searchtime, searchcalls, wpt, lwpt, pwpt, tree_sizes) =
    cegis ~widening ~fvs ~hints ~gas ~iter mySolver logical linst ledits phys pinst in
  let pinst_out = Option.value ~default:(StringMap.empty) pinst' (*|> complete*) in
  let stop = Time.now() in
  Printf.printf "\nSynthesized Program:\n%s\n\n%!"
    (Instance.apply `NoHoles `Exact pinst_out phys |> fst |> string_of_cmd);
  (Time.diff stop start, checktime, checkcalls, searchtime, searchcalls, wpt, lwpt, pwpt, tree_sizes, pinst_out)



   
let synthesize_edit_batch ?fvs:(fvs=[]) ?hints:(hints=None)
      ?gas:(gas=1000)
      ?iter:(iter = 0)
      ~widening
      mySolver
      (log_pipeline : cmd) (phys_pipeline : cmd)
      (linst : Instance.t)
      (pinst : Instance.t)
      (ledits : Edit.t list) =
  Printf.printf "Logical:\n%s\n\nPhysical:\n%s\n"
    (string_of_cmd (Instance.apply `NoHoles `Exact (Instance.update_list linst ledits) log_pipeline |> fst))
    (string_of_cmd (Instance.apply `NoHoles `Exact pinst phys_pipeline |> fst));

  synthesize ~widening ~fvs ~gas ~hints ~iter mySolver (log_pipeline) linst ledits
    (phys_pipeline) pinst
    
