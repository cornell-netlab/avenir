open Core
open Ast
open Packet
open Semantics
open Prover
open Manip
open Util
       

(* let rec widen_matches (ts : match_expr list) (hs : match_expr list) : match_expr list option =
 *     match ts, hs with
 *     | [], [] -> Some []
 *     | _, [] | [],_ -> failwith "different lengths!"
 *     | t::ts, h::hs ->
 *        let rec_cons m = Option.(widen_matches ts hs >>| mkCons m) in
 *        match t, h with
 *        | Exact (i,sz), Exact (j,_) ->
 *           if i = j
 *           then Exact (i,sz) |> rec_cons
 *           else if i = j + 1 || j = i + 1
 *           then Between (min i j, max i j, sz) |> rec_cons
 *           else None
 *        | Exact(i,sz), Between (lo, hi, _)
 *          | Between (lo, hi, _), Exact(i,sz)
 *          -> if i >= lo-1 && i <= hi + 1
 *             then Between (min i lo, max i hi, sz) |> rec_cons
 *             else None
 *        | Between(lo,hi,sz), Between (lo', hi', _)
 *          -> if hi >= lo' - 1 && lo <= hi' + 1
 *             then Between (min lo lo', max hi hi', sz) |> rec_cons
 *             else None
 * 
 * (\*ASSUMES ROWS ARE DISJOINT*\)                   
 * let rec insert_minimally (rows : row list) (row : row) : row list =
 *   match rows with
 *   | [] -> [row]
 *   | (m,a)::rows' ->
 *      let (m', a') = row in
 *      if a' = a
 *      then match widen_matches m m' with
 *               | None -> (m,a) :: insert_minimally rows' row
 *               | Some m'' -> (m'',a) :: rows'
 *      else (m,a) :: insert_minimally rows' row
 * 
 * 
 *                                     
 * 
 * let rec make_disjoint_matches novel exists : match_expr list list=
 *   match exists, novel with
 *   | [], [] -> [[]]
 *   | [], _ | _, [] -> failwith "Error different numbers of matches in existing and novel tables!"
 *   | (ex::exs), (nv::nvs)
 *     ->
 *      let cons_rec c = List.(make_disjoint_matches exs nvs >>| mkCons c) in
 *      match ex, nv with
 *      |  Exact (i,_), Exact (j, _) ->
 *          if i = j
 *          then []
 *          else cons_rec nv
 *      | Between (lo, hi, sz), Exact(j,_) ->
 *         if lo = hi
 *         then make_disjoint_matches (Exact (lo, sz)::exs) novel
 *         else if j >= lo && j <= hi
 *         then []
 *         else cons_rec nv
 *      | Exact (i, sz), Between (lo, hi, _) ->
 *         if lo = hi then
 *           make_disjoint_matches exists (Exact (lo, sz)::nvs)
 *         else if i < lo || i > hi
 *         then cons_rec nv
 *         else cons_rec (Between (lo, i-1, sz)) @ cons_rec (Between (lo, i + 1, sz))
 *      | Between(lo,hi,sz), Between(lo',hi',sz') ->
 *         if lo = hi
 *         then make_disjoint_matches (Exact (lo, sz)::exs) novel
 *         else if lo' = hi'
 *         then make_disjoint_matches exists (Exact (lo', sz')::exs)
 *         else if hi < lo' || hi' < lo
 *         then cons_rec nv
 *         else if hi' <= hi && lo' <= lo (\*nv covered by ex*\)
 *         then []
 *         else if hi <= hi' && lo >= lo' (\*ex subinterval of nv*\)
 *         then cons_rec (Between(lo',lo-1,sz))
 *              @ cons_rec (Between (hi+1, hi, sz))
 *         else if hi <= hi' && lo <= lo'
 *         then cons_rec (Between (hi+1, hi, sz))
 *         else cons_rec (Between (lo', lo+1, sz))
 *                        
 *                         
 * let make_disjoint_rows (novel : row) (exists : row) : row list =
 *   let (ex_mtchs, _) = exists in
 *   let (nv_mtchs, nv_act) = novel in
 *   let new_rows = List.(make_disjoint_matches nv_mtchs ex_mtchs
 *                        >>| inj_l nv_act) in
 *   (\* List.iter new_rows ~f:(fun row -> Printf.printf "  %s\n%!" (string_of_row row)); *\)
 *   new_rows
 *   
 * 
 * let disjoint_insertions (rows : row list) (new_row : row) =
 *   if rows = []
 *   then [new_row]
 *   else 
 *     List.(rows >>= make_disjoint_rows new_row) *)
            
(* let insert_optimally rows new_row =
 *   let inserts = disjoint_insertions rows new_row in
 *   rows @ inserts *)
                                    
                       
let apply_edit (inst : instance) ((tbl, row) : edit) =
  StringMap.update inst tbl
    ~f:(fun rows_opt ->
      match rows_opt with
      | None -> [row]
      | Some rows -> row::rows)
         
    
let rec apply_inst tag ?cnt:(cnt=0) (inst : instance) (prog : cmd) : (cmd * int) =
  match prog with
  | Skip 
    | Assign _
    | Assert _ 
    | Assume _ -> (prog, cnt)
  | Seq (c1,c2) ->
     let (c1', cnt1) = apply_inst tag ~cnt inst c1 in
     let (c2', cnt2) = apply_inst tag ~cnt:cnt1 inst c2 in
     (c1' %:% c2', cnt2)
  | While _ -> failwith "while loops not supported"
  | Select (typ, ss) ->
     let (ss, ss_cnt) =
       List.fold ss ~init:([],cnt)
         ~f:(fun (acc, cnt) (t, c) ->
           let (c', cnt') = apply_inst tag ~cnt inst c in
           acc @ [(t,c')], cnt'
         ) in
     (mkSelect typ ss, ss_cnt)
  | Apply (tbl, keys, acts, default) ->
     let actSize = max (log2(List.length acts)) 1 in
     let selects =
       StringMap.find_multi inst tbl
       |> List.fold ~init:[]
            ~f:(fun acc (matches, data, action) ->
              let t = List.fold2_exn keys matches
                        ~init:True
                        ~f:(fun acc x m -> (acc %&% encode_match x m)) in
              if action >= List.length acts then
                []
              else
                (t, (List.nth acts action
                     |> Option.value ~default:([], default)
                     |> bind_action_data data))
                :: acc)
     in
     let add_row_hole = Hole1 ("?AddRowTo" ^ tbl, 1) in
     let which_act_hole = Hole1 ("?ActIn" ^ tbl, actSize) in
     let holes =
       match tag with
       | `WithHoles -> 
          List.mapi acts
            ~f:(fun i (scope, act) -> 
              (List.fold keys ~init:True
                 ~f:(fun acc (x,sz) ->
                   acc %&% (Var1 (x,sz) %<=% Hole1 ("?"^x^"_hi",sz))
                   %&% (Var1(x,sz) %>=% Hole1("?"^x^"_lo",sz)))
               %&% (add_row_hole %=% mkVInt (1,1))
               %&% (which_act_hole %=% mkVInt (i,actSize))
              , holify (List.map scope ~f:fst) act))
       | `NoHoles -> []
     in
     let dflt_row =
       let cond =
         match tag with
         | `WithHoles -> True (*add_row_hole %=% mkVInt (0,1)*)
         | `NoHoles -> True in
       [(cond, default)] in
     (selects @ holes @ dflt_row |> mkOrdered
     , cnt (*+ 1*))
           


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
           comp x (Value1 (Int (i,sz)))
    in
    match t with
    | True | False -> t
    | Neg b -> !%(complete_aux_test ~falsify b)
    | And (a, b) -> complete_aux_test ~falsify a %&% complete_aux_test ~falsify b
    | Or (a, b) -> complete_aux_test ~falsify a %+% complete_aux_test ~falsify b
    | Impl (a, b) -> complete_aux_test ~falsify a %=>% complete_aux_test ~falsify b
    | Iff (a, b) -> complete_aux_test ~falsify a %<=>% complete_aux_test ~falsify b
    | Eq (Hole1 (_,sz), x) | Eq (x, Hole1 (_,sz)) -> hole_replace x sz (%=%)
    | Le (Hole1 (_,sz), x) | Le (x, Hole1 (_,sz)) -> hole_replace x sz (%<=%)
    | Eq _ | Le _ -> t
    | Member _ -> failwith "What do?"
  and complete_aux ~falsify cmd =
    match cmd with
    | Skip -> cmd
    | Assign (f, v) ->
      begin
        match v with
        | Hole1 _ ->
           let i = random_int_nin (List.map ~f:fst domain) in
           let sz = int_of_float (2. ** float_of_int i) in
           f %<-% Value1 (Int (i,sz))
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



let rec project_cmd_on_acts c (subst : expr1 StringMap.t) : cmd list =
  (* Printf.printf "PROJECTING\n%!"; *)
  let holes = true in
  match c with
  | Skip -> [c]
  | Assume b ->
     begin  match subst |> substitute ~holes b with
     | False -> []
     | b' -> [Assume b']
     end
  | Assert b -> [subst |> substitute ~holes b |> Assert]
  | Assign (v, e) ->
     begin match StringMap.find subst v with
     | None -> [v %<-% e]
     | Some _ ->
        [v %<-% e]
        (* Printf.printf "Replacing Assignment in %s with %s " v (string_of_expr1 ev);
         * begin match ev %=% e with
         * | False -> []
         * | t -> [Assume t]
         * end *)
     end
  | Seq (c1,c2) ->
     liftL2 mkSeq
       (project_cmd_on_acts c1 subst)
       (project_cmd_on_acts c2 subst)
  | Select (typ, cs) ->
     let open List in
     cs >>= (fun (t, a) ->
       project_cmd_on_acts a subst >>= fun act ->
       let t' = substitute ~holes t subst in
       if t' = False then [] else [(t', act)])
     |> mkSelect typ
     |> return 
  | Apply _ -> failwith "Shouldnt have applys at this stage"
  | While _ -> failwith "idk what to do with while loops"
     


let rec contains_inst_var e =
  match e with
  | Value1 _ -> false
  | Tuple _ -> failwith "tuples not allowed"
  | Var1 (str, _) | Hole1(str,_) ->
     String.is_substring str ~substring:"ActIn"
     || String.is_substring str ~substring:"AddRowTo"
  | Plus (e1,e2) | Times (e1,e2) | Minus(e1,e2)
    -> contains_inst_var e1 || contains_inst_var e2

let rec remove_inst_test b =
  match b with
  | True | False -> b
  | And  (b1,b2) -> remove_inst_test b1 %&% remove_inst_test b2
  | Or   (b1,b2) -> remove_inst_test b1 %+% remove_inst_test b2
  | Impl (b1,b2) -> remove_inst_test b1 %=>% remove_inst_test b2
  | Iff  (b1,b2) -> remove_inst_test b1 %<=>% remove_inst_test b2
  | Neg b1 -> !%(remove_inst_test b1)
  | Eq(e1,e2) -> if contains_inst_var e1 || contains_inst_var e2
                 then True
                 else b
  | Le (e1, e2) -> if contains_inst_var e1 || contains_inst_var e2
                   then True
                   else b
  | Member _ -> failwith "Membership unimplemented"
                             
                        
let rec remove_inst_cmd cmd =
  match cmd with
  | Skip -> cmd
  | Assume b -> remove_inst_test b |> Assume
  | Assert b -> remove_inst_test b |> Assert
  | Assign (v, e) ->
     if String.is_substring v ~substring:"ActIn"
        || String.is_substring v ~substring:"AddRowTo"
     then Skip
     else v %<-% e
  | Seq(c1,c2) ->
     remove_inst_cmd c1 %:% remove_inst_cmd c2
  | Select(typ, cs) ->
     List.rev cs
     |> List.fold ~init:[] ~f:(fun acc (v, c) ->
            (remove_inst_test v
            , remove_inst_cmd c)::acc
          )
     |> mkSelect typ
  | Apply _ | While _ -> failwith "Apply/While shouldn't be here"
          
  
                                  
let compute_candidates h pkt phys =
  match h with
  | None -> [phys]
  | Some f ->
     (* Printf.printf "Compute the candidates\n%!"; *)
     let action_mapping =
       let p = StringMap.filter_keys pkt ~f:(String.is_prefix ~prefix:"?ActIn") in
       (* Printf.printf "action mapping is %s" (Packet.string__packet p);        *)
       p |> StringMap.map ~f:(fun v -> Value1 v)
     in
     List.(f action_mapping >>= project_cmd_on_acts phys)
     
                                  
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


let rec compute_cand_for_trace line (pinst : instance) t : cmd =
  match line with
  | Skip 
    | Assert _
    | Assume _ 
    | Assign _
    -> line
  | Seq (c1,c2) -> compute_cand_for_trace c1 pinst t
                   %:% compute_cand_for_trace c2 pinst t
  | Select(typ, cs) ->
     List.map cs ~f:(fun (b, c) -> (b, compute_cand_for_trace c pinst t))
     |> mkSelect typ
  | Apply(name, keys, acts, default) ->
     (*might need to use existing instance to negate prior rules *)
     (* let misses =
      *   match StringMap.find pinst name with
      *   | None -> True
      *   | Some rows ->
      *      List.fold rows ~init:True
      *        ~f:(fun acc (ms, _) ->
      *          acc %&% !%(List.fold2_exn keys ms ~init:True ~f:(fun acc k m -> acc %&% encode_match k m))
      *        )
      * in *)
     begin match StringMap.find t name with
     | None -> Assume False
     | Some (data, act_idx) ->
        if act_idx >= List.length acts
        then (*Assert (misses) %:%*) default
        else List.fold keys ~init:True (*misses*)
               ~f:(fun acc (x, sz) -> acc
                                      %&% (Hole1("?"^x^"_lo", sz) %<=% Var1(x, sz))
                                      %&% (Var1(x, sz)  %<=% Hole1("?"^x^"_hi", sz)))
             |> Assert
             |> Fun.flip mkSeq (List.nth_exn acts act_idx |> bind_action_data data )
     end
  | While _ -> failwith "go away"
  
                                                    
let apply_hints
      (h_opt : ((action_data * int) StringMap.t -> (action_data * int) StringMap.t list) option)
      m
      pline pinst : (cmd * (action_data * int) StringMap.t option) list =
  match h_opt with
  | None -> [apply_inst `WithHoles pinst pline |> fst, None]
  | Some h ->
     List.map (h m) ~f:(fun t -> (compute_cand_for_trace pline pinst t, Some t))


let print_instance label linst =
  Printf.printf "%s instance is \n" label;
  StringMap.iteri linst ~f:(fun ~key ~data ->
      Printf.printf "%s -> \n" key;
      List.iter data ~f:(fun (keys,action) ->
          List.iter keys ~f:(fun k -> Printf.printf ",%s" (string_of_expr1 k));
          Printf.printf "  ---> %d \n%!" action)
      )

              

let get_one_model_edit
      ?fvs:(fvs = [])
      ~hints (pkt : Packet.t)
      mySolver
      (lline : cmd) (linst : instance) (ledit : edit)
      (pline : cmd) pinst
  =
  (* print_instance "Logical" (apply_edit linst ledit);
   * print_instance "Physical" pinst; *)
  let time_spent_in_z3, num_calls_to_z3 = (ref Time.Span.zero, ref 0) in
  let (pkt',_), wide, trace, actions = trace_eval_inst ~wide:StringMap.empty lline (apply_edit linst ledit) (pkt,None) in
  let st = Time.now () in
  (* let phi = Packet.to_test ~fvs pkt' in *)
  let cands = apply_hints hints actions pline pinst in
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
                      wp_paths path (Packet.test_of_wide ~fvs wide) (* |> List.map ~f:(snd) *)
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
          let (res, time) = check mySolver `MinSat (log_wp %=>% wp_phys) in
          time_spent_in_z3 := Time.Span.(!time_spent_in_z3 + time);
          num_calls_to_z3 := !num_calls_to_z3 + 1;
          match res with
          | None -> None
          | Some model -> Some (model, acts)
      )
  in
  (model, !time_spent_in_z3, !num_calls_to_z3, wp_time)
  
let rec fixup_val (model : value1 StringMap.t) (e : expr1)  : expr1 =
  (* let _ = Printf.printf "FIXUP\n%!" in *)
  let binop op e e' = op (fixup_val model e) (fixup_val model e') in
  match e with
  | Value1 _ | Var1 _ -> e
  | Hole1 (h,sz) -> 
     begin match StringMap.find model h with
     | None -> e
     | Some v -> let sz' = size_of_value1 v in
                 let strv = string_of_value1 v in
                 (if sz <> sz' then
                    (Printf.printf "[Warning] replacing %s#%d with %s, \
                                    but the sizes may be different, \
                                    taking the size of %s to be ground \
                                    truth\n%!" h sz strv strv));
                 Value1 v
     end
  | Plus  (e, e') -> binop mkPlus  e e'
  | Times (e, e') -> binop mkTimes e e'
  | Minus (e, e') -> binop mkMinus e e'
  | Tuple es -> List.map es ~f:(fixup_val model) |> mkTuple

let rec fixup_val2 (model : value1 StringMap.t) (set : expr2) : expr2 =
  match set with
  | Value2 _ | Var2 _ -> set
  | Hole2 _ -> failwith "Second-order holes not supported"
  | Single e -> Single (fixup_val model e)
  | Union (s,s') -> mkUnion (fixup_val2 model s) (fixup_val2 model s')

let rec fixup_test (model : value1 StringMap.t) (t : test) : test =
  let binop ctor call left right = ctor (call left) (call right) in 
  match t with
  | True | False -> t
  | Neg p -> mkNeg (fixup_test model p)
  | And  (p, q) -> binop (%&%)   (fixup_test model) p q
  | Or   (p, q) -> binop (%+%)   (fixup_test model) p q
  | Impl (p, q) -> binop (%=>%)  (fixup_test model) p q
  | Iff  (p, q) -> binop (%<=>%) (fixup_test model) p q
  | Eq (v, w) -> binop (%=%)  (fixup_val model) v w
  | Le (v, w) -> binop (%<=%) (fixup_val model) v w
  | Member(v,set) -> mkMember (fixup_val model v) (fixup_val2 model set)

let rec fixup_selects (model : value1 StringMap.t) (es : (test * cmd) list) =
  match es with
  | [] -> []
  | (cond, act)::es' ->
    let cond' = fixup_test model cond in
    let act' = fixup act model in
    (* Printf.printf "  [fixup] replacing %s with %s\n%!"
     *   (string_of_test cond) (string_of_test cond');
     * Printf.printf "  [fixup] replacing %s with %s\n%!" *)
      (* (string_of_cmd act) (string_of_cmd act'); *)
    (cond', act') :: (
      if cond = cond' && act = act' then
        fixup_selects model es'
      else
        (cond, act) :: fixup_selects model es'
    )    
and fixup (real:cmd) (model : value1 StringMap.t) : cmd =
  (* Printf.printf "FIXUP WITH MODEL: %s\n%!\n" (string_of_map model); *)
  match real with
  | Skip -> Skip
  | Assign (f, v) -> Assign(f, fixup_val model v)
  | Assert t -> Assert (fixup_test model t)
  | Assume t -> Assume (fixup_test model t)
  | Seq (p, q) -> Seq (fixup p model, fixup q model)
  | While (cond, body) -> While (fixup_test model cond, fixup body model)
  | Select (styp,cmds) -> fixup_selects model cmds |> mkSelect styp
  | Apply (name, keys, acts, dflt)
    -> Apply (name
            , keys
            , List.map acts ~f:(fun (data, a) -> (data, fixup a model))
            , fixup dflt model)

let symbolic_pkt fvs = 
  List.fold fvs ~init:True
    ~f:(fun acc_test (var,sz) ->
      if String.get var 0 |> Char.is_uppercase
         || String.substr_index var ~pattern:("NEW") |> Option.is_some
      then acc_test
      else
        Var1 (var,sz) %=% Var1 (symbolize var, sz)
        %&% acc_test)

let symb_wp ?fvs:(fvs=[]) cmd =
  List.dedup_and_sort ~compare (free_vars_of_cmd cmd @ fvs)
  |> symbolic_pkt
  |> wp cmd
  
let implements fvs mySolver (logical : cmd) (linst : instance) (ledit : edit) (real : cmd) (pinst : instance) =
  (* let _ = Printf.printf "IMPLEMENTS on\n%!    ";
   *         List.iter fvs ~f:(fun (x,_) -> Printf.printf " %s" x);
   *         Printf.printf "\n%!" *)
  (* in *)
  let u_log,_ = logical |> apply_inst `NoHoles (apply_edit linst ledit)  in
  let u_rea,_ = real |> apply_inst `NoHoles pinst in
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


let rec get_schema_of_table name phys =
  match phys with
  | Skip 
    | Assume _
    | Assert _
    | Assign _
    -> None
  | Seq (c1, c2) ->
     begin match get_schema_of_table name c1 with
     | None -> get_schema_of_table name c2
     | Some ks -> Some ks
     end
  | Select (_, cs) ->
     List.find_map cs ~f:(fun (_, c) -> get_schema_of_table name c)
  | Apply(name', ks, acts, def)
    -> if name = name' then Some (ks,acts,def) else None
  | While (_, c) -> get_schema_of_table name c


let match_cap m m' =
  match m, m' with
  | Exact (i,_), Exact (j,_) ->
     if i = j then [m] else []
  | Exact (i,_), Between (lo, hi, _)
    | Between (lo, hi, _), Exact(i,_)->
     if lo <= i && i <= hi then
       [m]
     else
       []
  | Between (lo, hi, sz), Between (lo', hi', _) ->
     let lo'' = max lo lo' in
     let hi'' = min hi hi' in
     if lo'' < hi'' then
       [Between (lo'', hi'', sz)]
     else if lo'' = hi'' then
       [Exact(lo'',sz)]
     else
       []

let off_by_one i j =  max i j - min i j = 1
  
let match_cup m m' =
  match m, m' with
  | Exact (i,_), Exact (j,sz) ->
     if i = j then
       [m]
     else if off_by_one i j
     then [Between (min i j, max i j, sz)]
     else [m; m']
  | Exact (i,_), Between (lo, hi, sz)
    | Between (lo, hi, sz), Exact(i,_)->
     if lo <= i && i <= hi then
       [Between (lo, hi, sz)]
     else if i + 1 = lo || hi + 1 = i
     then [Between(min lo i, max hi i, sz)]
     else [m;m']
  | Between (lo, hi, sz), Between (lo', hi', _) ->
     if hi >= lo - 1 && lo <= hi + 1
     then [Between (min lo lo', max hi hi', sz)]
     else [m;m']

let matches_cup ms =
  List.fold ms ~init:[]
    ~f:(fun acc m ->
      List.fold acc ~init:[]
        ~f:(fun acc' m' ->
          acc' @ match_cup m m' ))
          

let match_sub m m' =
  match m, m' with
  | Exact (i, _), Exact (j, _) -> i = j
  | Exact (i, _), Between (lo', hi',_) -> lo' <= i && i <= hi'
  | Between (lo, hi, _), Exact (j,_) -> lo = j && hi = j
  | Between (lo, hi, _), Between (lo', hi', _) ->  lo <= hi' && lo' <= hi
            
let rec match_minus m ms =
  let compare m m' =
    match m, m' with
    | Exact(i,_), Exact(j,_) -> compare i j
    | Exact(i,_), Between(lo,_,_) -> compare i lo
    | Between(lo, _,_), Exact(i,_) -> compare lo i
    | Between(lo, _, _), Between(lo',_,_) -> compare lo lo'
  in
  match m with
  | Exact(_, _) ->
     if List.exists ms ~f:(match_sub m)
     then []
     else [m]
  | Between(lo,hi, sz) ->
     match ms with
     | [] -> [m]
     | (m'::ms') ->
        match m' with
        | Exact(j,_) ->
           if j < lo || j > hi
           then match_minus (Between (lo,hi,sz)) ms'
           else if j = lo then
             match_minus (Between(lo+1, hi, sz)) ms'
           else if j = hi then
             match_minus (Between (lo, hi-1, sz)) ms'
           else
             match_minus (Between (lo, j-1, sz)) ms'
             @ match_minus (Between (j+1, hi, sz)) ms'
             |> matches_cup
             |> List.sort ~compare

        | Between(lo', hi', _) ->
           if lo' <= lo && hi <= hi'
           then []
           else if lo' <= lo
           then match_minus (Between(hi'+1, hi, sz)) ms'
           else if hi <= hi' then
             match_minus (Between(lo, lo'-1,sz)) ms'
           else (*lo < lo' && hi > hi'*)
             match_minus (Between (lo, lo'-1, sz)) ms'
             @ match_minus (Between (hi'+1, hi, sz)) ms'
             |> matches_cup
             |> List.sort ~compare
                                        
                                       
let fixup_edit match_model (action_map : (action_data * size) StringMap.t option) phys (pinst : instance) : instance =
  let mk_new_row tbl_name data_opt act : row list =
    match get_schema_of_table tbl_name phys with
    | None -> failwith ("Couldnt find keys for table " ^ tbl_name)
    | Some (ks, acts, d) ->
       let keys_holes =
         List.fold ks ~init:(Some [])
           ~f:(fun acc (v, sz) ->
             match acc,
                   fixup_val match_model (Hole1("?"^v^"_lo", sz)),
                   fixup_val match_model (Hole1("?"^v^"_hi", sz))
             with
             | None, _,_ -> None
             | Some ks, Hole1 _, Hole1 _ ->
                begin match fixup_val match_model (Hole1("?"^v, sz)) with
                | Hole1 _ -> None
                | Value1(Int (x, sz)) ->
                   Some (ks @ [Exact (x,sz)])
                | _ -> failwith "Model did something weird"
                end
             | Some ks, Value1(Int(lo,_)), Value1(Int(hi,_)) ->
                let k = if lo = hi
                        then [Exact (lo, sz)]
                        else if lo > hi
                        then failwith "Low value greater than high value in model from z3"
                        else [Between (lo, hi,sz)] in
                Some (ks @ k)
             | _, _,_ -> failwith "got something that wasn't a model"
           ) in
       let data =
         match data_opt with
         | Some ds -> ds
         | None ->
            match List.nth acts act with
            | None -> []
            | Some (params, _) ->
               Printf.printf "Params for act %d :%s\n%!" act
                 (List.fold params ~init:"" ~f:(fun acc (p,_) -> Printf.sprintf "%s %s" acc p));
               List.fold params ~init:[]
                 ~f:(fun acc (p,sz) ->
                   match StringMap.find match_model p with
                   | None ->
                      failwith ("Couldn't find " ^ p)
                   | Some v -> acc @ [(get_int v, sz)]
                 )
       in
       match keys_holes with
       | None -> []
       | Some ks -> [(ks, data, act)]
  in
  match action_map with
  | Some m -> StringMap.fold ~init:pinst m ~f:(fun ~key:tbl_name ~data:(act_data,act) acc ->
                  StringMap.update acc tbl_name
                    ~f:(function
                      | None -> mk_new_row tbl_name (Some act_data) act
                      | Some rows -> mk_new_row tbl_name (Some act_data) act @ rows
                    )
                )
  | None ->
     let tables_added_to =
       StringMap.fold match_model ~init:[]
         ~f:(fun ~key ~data acc ->
           if String.is_substring key ~substring:"AddRowTo"
           then (String.substr_replace_all key ~pattern:"?" ~with_:""
                 |> String.substr_replace_first ~pattern:"AddRowTo" ~with_:"")
                :: acc
           else acc 
         ) in
     List.fold tables_added_to ~init:pinst
       ~f:(fun acc tbl_name ->
         let act = StringMap.find_exn match_model ("?ActIn" ^ tbl_name) |> get_int in
         StringMap.update acc tbl_name           
           ~f:(function
             | None -> mk_new_row tbl_name None act
             | Some rows -> mk_new_row tbl_name None act @ rows)
       )
     
     
                                                         
(** solves the inner loop **)
let solve_concrete
      ?fvs:(fvs = [])
      mySolver
      ~hints
      ?packet:(packet=None)
      (logical : cmd) (linst : instance) (edit : edit)
      (phys : cmd) (pinst : instance)
    : (instance * Time.Span.t * int * Time.Span.t) =
  let values = multi_ints_of_cmd logical |> List.map ~f:(fun x -> Int x) in
  let pkt = packet |> Option.value ~default:(Packet.generate fvs ~values) in
  match get_one_model_edit ~fvs ~hints pkt mySolver logical linst edit phys pinst with
  | None, z3time, ncalls, _ ->
     Printf.sprintf "Couldnt find a model in %d calls and %f"
       ncalls (Time.Span.to_ms z3time)
     |> failwith
  | Some (model, action_map), z3time, ncalls, wp_time ->
     let pinst' = fixup_edit model action_map phys pinst in
     pinst', z3time, ncalls, wp_time
  
let cegis ?fvs:(fvs = []) ~hints ?gas:(gas=1000) ~iter mySolver (logical : cmd) linst (ledit : edit) (real : cmd) pinst =
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
      implements fvs mySolver logical linst ledit real pinst in
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
           solve_concrete ~fvs ~hints ~packet:(Some counter) mySolver logical linst ledit real pinst in
         model_time := Time.Span.(!model_time + ex_z3_time);
         model_calls := !model_calls + ncalls;
         wp_time := Time.Span.(!wp_time + wpt);
         if StringMap.equal (=) pinst pinst'
         then failwith ("Could not make progress on edit " ^ string_of_edit ledit)
         else loop (gas-1) pinst'
  in
  let pinst' = loop gas pinst in
  (* Printf.printf "total z3 time to synthesize %s + %s = %s\n%!"
   *   (Time.Span.to_string !implements_time)
   *   (Time.Span.to_string !model_time)
   *   (Time.Span.(to_string (!implements_time + !model_time))); *)
  (pinst', !implements_time, !implements_calls, !model_time, !model_calls, !wp_time, !log_wp_time, !phys_wp_time, !tree_sizes)
    
let synthesize ?fvs:(fvs=[]) ~iter ?hints:(hints = None) ?gas:(gas = 1000)
      mySolver
      logical linst ledit phys pinst =
  let start = Time.now () in
  let (pinst', checktime, checkcalls, searchtime, searchcalls, wpt, lwpt, pwpt, tree_sizes) =
    cegis ~fvs ~hints ~gas ~iter mySolver logical linst ledit phys pinst in
  let pinst_out = Option.value ~default:(StringMap.empty) pinst' (*|> complete*) in
  let stop = Time.now() in
  Printf.printf "\nSynthesized Program:\n%s\n\n%!"
    (apply_inst `NoHoles pinst_out phys |> fst |> string_of_cmd);
  (Time.diff stop start, checktime, checkcalls, searchtime, searchcalls, wpt, lwpt, pwpt, tree_sizes, pinst_out)



   
let synthesize_edit ?fvs:(fvs=[]) ?hints:(hints=None)
      ?gas:(gas=1000)
      ?iter:(iter = 0)
      mySolver
      (log_pipeline : cmd) (phys_pipeline : cmd)
      (linst : instance)
      (pinst : instance)
      (ledit : edit) =  
  Printf.printf "Logical:\n%s\n\nPhysical:\n%s\n"
    (string_of_cmd (apply_inst `NoHoles (apply_edit linst ledit) log_pipeline |> fst))
    (string_of_cmd (apply_inst `NoHoles pinst phys_pipeline |> fst));

  synthesize ~fvs ~gas ~hints ~iter mySolver (log_pipeline) linst ledit
    (phys_pipeline) pinst
    
