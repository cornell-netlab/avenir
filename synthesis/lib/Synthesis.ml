open Core
open Ast
open Semantics
open Graph
open Prover
open Manip
open Util
let well_formed (c:cmd) : bool =
  let well_formed_selects (ss : (test * cmd) list) : bool =
    no_nesting ss
    (* && instrumented ss *)
    (* && no_topo_loops ss *)
    && no_negated_holes ss
  in
  match c with
  | Seq(SetLoc _, While (_, body)) ->
    begin
      match body with
      | Select (_,ss)
        -> well_formed_selects ss
      | _
        -> false
    end
  | _ -> false



(* let symbolize x = x ^ "_SYMBOLIC" *)
(* let unsymbolize = String.chop_suffix_exn ~suffix:"_SYMBOLIC" *)
let is_symbolic = String.is_suffix ~suffix:"_SYMBOLIC"
           

(* Computes the traces between two points in graph *)
let find_traces (graph:graph) (in_loc : int) (out_loc : int) =
  let traces = get_all_paths_between graph in_loc out_loc in
  let _ = Printf.printf "ALL PATHS from %d to %d ;\n" in_loc out_loc;
          List.iter traces ~f:(fun tr ->
              Printf.printf "\t[ ";
              List.iter tr ~f:(Printf.printf "%d ");
              Printf.printf "]\n%!") in
  traces



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
    | True | False | LocEq _ -> t
    | Neg b -> !%(complete_aux_test ~falsify b)
    | And (a, b) -> complete_aux_test ~falsify a %&% complete_aux_test ~falsify b
    | Or (a, b) -> complete_aux_test ~falsify a %+% complete_aux_test ~falsify b
    | Eq (Hole1 (_,sz), x) | Eq (x, Hole1 (_,sz)) -> hole_replace x sz (%=%)
    | Lt (Hole1 (_,sz), x) | Lt (x, Hole1 (_,sz)) -> hole_replace x sz (%<%)
    | Eq _ | Lt _ -> t
    | Member _ -> failwith "What do?"
  and complete_aux ~falsify cmd =
    match cmd with
    | Skip | SetLoc _ -> cmd
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
              , List.map acts ~f:(complete_aux ~falsify)
              , complete_aux ~falsify dflt)
  in
  complete_aux ~falsify cmd

let complete cmd = complete_inner ~falsify:true cmd   

(** Solves the inner loop of the cegis procedure. 
 * pre-condition: pkt is at an ingress host 
**)
let get_one_model ?fvs:(fvs = []) (pkt : Packet.t) (logical : cmd) (phys : cmd) =
  let pkt_loc', _ = trace_eval logical (pkt,None) |> Option.value_exn in
  let phi = Packet.to_test ~fvs (fst pkt_loc') in
  let st = Time.now () in
  let wp_phys_paths = wp_paths phys phi |> List.filter ~f:(fun pre -> pre <> False) in
  let wp_time = Time.diff (Time.now ()) st in
  
  (* if wp_phys_paths = [] then failwith "No feasible paths!" else
   *   Printf.printf "%d feasible paths\n\n%!" (List.length wp_phys_paths); *)
  (* Printf.printf "------------------------------------------------\n";
   * List.iter wp_phys_paths ~f:(fun path ->
   *     Printf.printf "%s\n\n%!" (string_of_test path)
   *   )
   * ; Printf.printf "----------------------------------------------------\n%!"
   * ; *)
  let time_spent_in_z3 = ref Time.Span.zero in
  let num_calls_to_z3 = ref 0 in
  let model = List.find_map wp_phys_paths ~f:(fun wp_phys ->
      let _ = Printf.printf "PHYSICAL WEAKEST_PRECONDITION:\n%s\n\nOF PROGRAM:\n%s\nwrt condition: %s \n%!"
                  (string_of_test wp_phys)
                  (string_of_cmd phys)
                  (string_of_test phi)
        in
        if wp_phys = False
        then (Printf.printf "-- contradictory WP\n%!"; None (*find_match rest_paths*))
        else
          (* let condition = Packet.to_test pkt %=>% wp_phys in *)
          let _ = Printf.printf "Substituting %s into\n %s\n%!"
                    (Packet.string__packet pkt)
                    (string_of_test wp_phys) in
          let condition = substitute wp_phys (StringMap.filter_mapi pkt
                                                ~f:(fun ~key ~data ->
                                                  if is_symbolic key
                                                  then None
                                                  else Some(Value1 data) )) in
          let _ = Printf.printf "CONDITION: \n%s\n%!" (string_of_test condition) in
          num_calls_to_z3 := !num_calls_to_z3 + 1;
          match check `Sat condition with
          | (None, d) -> Printf.printf "unsolveable!\n%!";
                         time_spent_in_z3 := Time.Span.(!time_spent_in_z3 + d);
                         None
                                                             
          | Some model, d ->
             time_spent_in_z3 := Time.Span.(!time_spent_in_z3 + d);
             Some model)
  in
  Printf.printf "Took %d reps over %s to find model\n!"
    (!num_calls_to_z3)
    (Time.Span.to_string !time_spent_in_z3)
  ; model, !time_spent_in_z3, !num_calls_to_z3, wp_time
	       
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
  | True | False | LocEq _ -> t
  | Neg p -> mkNeg (fixup_test model p)
  | And(p, q) -> binop mkAnd (fixup_test model) p q
  | Or(p, q) -> binop mkOr (fixup_test model) p q
  | Eq (v, w) -> binop mkEq (fixup_val model) v w
  | Lt (v, w) -> binop mkLt (fixup_val model) v w
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
  | SetLoc l -> SetLoc l
  | Assign (f, v) -> Assign(f, fixup_val model v)
  | Assert t -> Assert (fixup_test model t)
  | Assume t -> Assume (fixup_test model t)
  | Seq (p, q) -> Seq (fixup p model, fixup q model)
  | While (cond, body) -> While (fixup_test model cond, fixup body model)
  | Select (styp,cmds) -> fixup_selects model cmds |> mkSelect styp
  | Apply (name, keys, acts, dflt)
    -> Apply (name
            , keys
            , List.map acts ~f:(fun a -> fixup a model)
            , fixup dflt model)

let unroll_fully c = unroll (diameter c) c 


let symbolize str =  str ^ "_SYMBOLIC"
let unsymbolize = String.substr_replace_all ~pattern:"_SYMBOLIC" ~with_:""
let is_symbolic = String.is_substring ~substring:"_SYMBOLIC"

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
  
let implements fvs logical real =
  let _ = Printf.printf "IMPLEMENTS on\n%!    ";
          List.iter fvs ~f:(fun (x,_) -> Printf.printf " %s" x);
          Printf.printf "\n%!"
  in
  let u_log = logical in
  let u_rea = real |> complete in
  let st_log = Time.now () in
  let log_wp  = symb_wp u_log ~fvs in
  let log_time = Time.(diff (now()) st_log) in
  let st_real = Time.now () in
  let real_wp = symb_wp u_rea ~fvs in
  let real_time = Time.(diff (now()) st_real) in
  (* Printf.printf "\n==== Checking Implementation =====\n%!\nLOGICAL \
   *                SPEC:\n%s\n\nREAL SPEC: \n%s\n\n%!"
   *   (string_of_test log_wp)
   *   (string_of_test real_wp); *)
  match check_valid (log_wp %<=>% real_wp) with
  | None, z3time   -> Printf.printf "++++++++++valid++++(%s)+++++++++\n%!"
                        (Time.Span.to_string z3time)
                    ; `Yes, z3time, log_time, real_time
  | Some x, z3time ->
     let pce = Packet.from_CE x in
     Printf.printf "----------invalid----------------\n%! CE = %s\n%!" (Packet.string__packet pce)
     ; `NoAndCE (Packet.from_CE x) , z3time, log_time, real_time
                   
(** solves the inner loop **)
let solve_concrete ?fvs:(fvs = []) ?packet:(packet=None) (logical : cmd) (real : cmd) =
  let values = multi_ints_of_cmd logical |> List.map ~f:(fun x -> Int x) in
  let pkt = packet |> Option.value ~default:(Packet.generate fvs ~values) in
  match get_one_model ~fvs pkt logical real with
  | None, z3time, ncalls, _ ->
     Printf.sprintf "Couldnt find a model in %d calls and %f"
       ncalls (Time.Span.to_ms z3time)
     |> failwith
  | Some model, z3time, ncalls, wp_time ->
     let real' =  fixup real model in
     Printf.printf "\n\nNEXT ITERATION OF REAL PROGRAM:\n%s\n%!\n" (string_of_cmd real')
     ; real', z3time, ncalls, wp_time

let check_edit (_:int) (_:cmd) (_:cmd) = failwith ""


let matchExpr ks =  List.map ks ~f:(fun x -> Var1 x)
let bitLen keys = size_of_expr1 (matchExpr keys |> mkTuple) 
let rec matchWF keys mVar2 i j =
  if i = j then matchWF keys mVar2 (i-1) j %&% matchWF keys mVar2 i (j-1)
  else if i < 0 || j < 0 then True
  else !%(mkMember (matchExpr keys |> mkTuple) (mVar2 i)
          %&% mkMember (matchExpr keys |> mkTuple) (mVar2 j))

let catchAll rows = List.fold rows ~init:True ~f:(fun acc (k,_) -> (!% k) %&% acc)


let rec base_translation (c:cmd) =
  match c with
  | Skip
    | SetLoc _
    | Assign _
    | Assume _
    | Assert _ -> c
  | Select (styp, cmds) ->
    List.map cmds
      ~f:(fun (t, c) -> (t, base_translation c))
    |> mkSelect styp
  | Seq (c, c')
    -> Seq(base_translation c, base_translation c')
  | While (b, c)
    -> While (b, base_translation c) 
  | Apply (name, keys, actions, default)
    ->
     let rec mVar2 ?n:(n=1) actId =
       if n <= 0 then Value2 Empty else
         mkUnion (Single(Var1 ("elM_"^name^"_"^string_of_int actId^"_"^ string_of_int n, bitLen keys)))
           (mVar2 ~n:(n-1) actId) in
     let positiveRows : (test * cmd) list =
       List.foldi actions ~init:[]
         ~f:(fun i acc act ->
           (mkMember (matchExpr keys |> mkTuple) (mVar2 i)
           , act) :: acc )
     in
     (* let n = (List.length actions) - 1 in *)
     (* Assert (matchWF keys mVar2 n n)
      * %:% *)
     (positiveRows @ ([catchAll positiveRows, default])
          |> mkSelect Partial)
           
                                                  
let rec add_symbolic_row (name:string) (c:cmd) : (string * size) list * cmd=
  match c with
  | Skip
    | SetLoc _
    | Assign _
    | Assume _
    | Assert _ -> [], c
  | Select (styp, selects) ->
     let (vars, selects) =
       List.fold ~init:([],[]) selects
          ~f:(fun (vs,ss) (t, c) ->
            let (vs', c') = add_symbolic_row name c in
            (vs @ vs', (t,c' ) :: ss)) in
     (vars, mkSelect styp selects)
  | Seq (c, c')
    -> let (v, cs) = add_symbolic_row name c in
       let (v', cs') = add_symbolic_row name c' in
       (v @ v', cs %:% cs')
  | While (b, c)
    -> let (vs, cs) = add_symbolic_row name c in
       (vs, mkWhile b cs)
  | Apply (name', keys, actions, default) as table
    ->
     let rec mVar2 ?n:(n=1) actId =
       if n <= 0 then Value2 Empty else
         mkUnion (Single(Var1 ("elM_"^name^"_"^string_of_int actId^"_"^ string_of_int n, bitLen keys)))
           (mVar2 ~n:(n-1) actId) in
     
     let newString (k,sz) = (k ^ "_" ^ name ^ "_NEW", sz) in
     let newVar1 (k,sz) =  newString (k,sz) |> Var1  in
     let n = (List.length actions) - 1 in
     let actionSize = log2 (n + 1) in
     let actionString = "action_NEW" in
     let actionVar = Var1 (actionString, actionSize) in
     ((actionString, actionSize) :: List.map keys ~f:newString,
     if name = name' then
         let row = List.map keys ~f:(newVar1) in
         let positiveRows : (test * cmd) list =
           List.foldi actions ~init:[]
             ~f:(fun i acc act ->
               (mkMember (matchExpr keys |> mkTuple) (mVar2 i)
                %&% ((matchExpr keys |> mkTuple) %<>% mkTuple row)
               , act) :: acc )
           @ List.foldi actions ~init:[]
               ~f:(fun i acc act ->
                 ((actionVar :: matchExpr keys |> mkTuple) %=% (mkVInt (i,actionSize) :: row |> mkTuple )
                 , act) :: acc)
         in
         let actionWF =  List.foldi actions
                           ~init:(Value2 Empty)
                           ~f:(fun i acc _ -> mkInsert (mkVInt (i, actionSize)) acc)
                         |> mkMember actionVar in
         Assert actionWF
         (* %:% Assert (matchWF keys mVar2 n n) *)
         %:% (positiveRows @ ([catchAll positiveRows, default])
              |> mkSelect Partial)
     else
       table)


         
(* enables at most [n] additions to each table in [c] *)
let rec concretely_instrument (n:int) (c:cmd) =  
  match c with
  | Skip
    | SetLoc _
    | Assign _
    | Assume _
    | Assert _ -> c
  | Select (styp, cmds) ->
    List.map cmds
      ~f:(fun (t, c) -> (t, concretely_instrument n c))
    |> mkSelect styp
  | Seq (c, c')
    -> Seq(concretely_instrument n c, concretely_instrument n c')
  | While (b, c)
    -> While (b, concretely_instrument n c) 
  | Apply  (name, keys, actions,default)
    -> 
     let mVar2 actId = Var2 ("M_"^name^"_"^string_of_int actId, bitLen keys) in
     let fillVar1 rowId (key, sz) =  Var1 (key^"_"^string_of_int rowId^"_"^name^"_FILL", sz) in
     let l = List.length actions - 1 in
     let actionSize = int_of_float(2. ** (float_of_int(l + 1))) - 1 in
     let actionVar rowId = Var1 ("action_"^name ^string_of_int rowId ^ "_FILL", actionSize) in
     
     let rec modifiedRows i =
       if i = 0 then Value2 Empty
       else List.map keys ~f:(fillVar1 i) |> mkTuple |> Single |> mkUnion (modifiedRows (i-1))
     in
       
     let positiveRows : (test * cmd) list =
       List.foldi actions ~init:[]
         ~f:(fun i acc act ->
           (mkMember (matchExpr keys |> mkTuple) (mVar2 i)
            %&% (!%(mkMember(matchExpr keys |> mkTuple) (modifiedRows n))) (*modifiedRows are new!*)
           , act) :: acc )
       @ List.foldi actions ~init:[]
           ~f:(fun actId acc act ->
             let rec newRow rowId   =
               if rowId <= 0 then []
               else (mkTuple (mkVInt (actId, actionSize) :: matchExpr keys)
                     %=% mkTuple(actionVar rowId :: List.map keys ~f:(fillVar1 rowId))
                    , act) :: newRow (rowId - 1)
             in newRow n @ acc)
     in
     (* let delWF i =  Member(matchExpr keys |> mkTuple, delSet) %=>% (Member(matchExpr keys |> mkTuple, matchUnion i)) in *)
     Assert (matchWF keys mVar2 l l) %:% Skip
     (* %:% Assume (delWF l) *)
     %:% (positiveRows @ ([catchAll positiveRows, default])
      |> mkSelect Partial)


let mkFunHole name i (k,sz) =
  Hole1 ("f" ^ k ^ "_" ^ name ^ "_" ^ string_of_int i , sz)
  

let rec edit_synth_real_inst ~numFs ~setSize (c : cmd) : cmd =
  match c with
  | Skip | SetLoc _ | Assign _ | Assume _ | Assert _ -> c
  | Select (styp, ss) ->
     List.map ss ~f:(fun (b,c) -> (b, edit_synth_real_inst ~numFs ~setSize c))
     |> mkSelect styp
  | Seq (c, c') ->
     edit_synth_real_inst ~numFs ~setSize  c
     %:%  edit_synth_real_inst ~numFs ~setSize c'
  | While (b, c) ->
     edit_synth_real_inst ~numFs ~setSize  c
     |> mkWhile b
  | Apply (name, keys, actions, default) ->
     (* ENCODE THE FUNCTIONS AS HOLES -- RELY ON THE Z3 ENCODING TO APPROPRIATELY PASS RHOVARS TO THEM*)
     let matchVars = List.mapi actions ~f:(fun idx _ -> (name ^ "_matchAction_" ^ string_of_int idx,idx)) in
     let keysExpr = List.map keys ~f:(fun k -> Var1 k) |> mkTuple in
     let rec notNewMatch i =
       if i <= 0 then True else 
         List.fold keys ~init:True
           ~f:(fun acc k -> (Var1 k %<>% mkFunHole name i k) %&% acc)
         %&% notNewMatch (i-1) in
     let oldRows = List.map matchVars ~f:(fun (mS, idx) ->
                       (keysExpr %=% Var1 (mS, size_of_expr1 keysExpr)
                        %&% notNewMatch numFs,
                        List.nth_exn actions idx))
     in
     let actSize = log2 (List.length actions + 1) in
     let rec newRows i =
       if i <= 0 then [] else
         List.mapi actions
           ~f:(fun actId act ->
             ( List.fold keys ~init:True
                 ~f:(fun acc  k ->
                   (Var1 k %=% mkFunHole name i k)
                   %&% (mkVInt (actId, actSize) %=% mkFunHole name i ("act" ^ string_of_int actId, actSize))
                   %&% acc)
             , act))
         @ newRows (i-1) in
     let defaultRow =
       [(List.fold_left (oldRows @ (newRows numFs))
           ~init:True
           ~f:(fun acc (t,_) -> !%(t) %&% acc)
        , default)]
     in
     oldRows
     @ newRows numFs
     @ defaultRow
     |> mkSelect Partial
     
                             
(* Pre :: neither program has any holes in it *)
(* Currently assume that no deletions are required to Real   *)
let check_add n name logical real = 
  let u_log = unroll_fully logical in
  let (_, u_log_add1) = add_symbolic_row name u_log in
  let u_rea = unroll_fully real in
  let u_rea_addn = concretely_instrument n u_rea in
  (* let fvs = free_vars_of_cmd u_rea
   *           @ free_vars_of_cmd u_log
   *           @ free_vars_of_cmd u_log_add1
   *           @ free_vars_of_cmd u_rea_addn in *)
  let log_wp  = base_translation u_log |> symb_wp in
  let real_wp = base_translation u_rea |> symb_wp in
  let log1_wp = symb_wp u_log_add1 in
  let rean_wp = symb_wp u_rea_addn in
  match check_valid_impl log_wp (*=*) real_wp (*=>*) log1_wp (*=*) rean_wp with
  | None -> Printf.printf "+++++valid+++++\n%!"; `Yes
  | Some x -> Printf.printf "-----invalid---\n%!"; `NoAndCE (Packet.from_CE x)

(* PRE :: neither program has any holes *)
(* Assume No deletions are required for an addition *)                                            
let synth_add n name logical real =
  let u_log = unroll_fully logical in
  let (rhoVars, u_log_add1) = add_symbolic_row name u_log in
  let u_rea = unroll_fully real in
  let u_rea_addn = edit_synth_real_inst ~numFs:n ~setSize:1 u_rea in
  (* let fvs = free_vars_of_cmd u_rea
   *           @ free_vars_of_cmd u_log
   *           @ free_vars_of_cmd u_log_add1
   *           @ free_vars_of_cmd u_rea_addn in *)
  let log_wp  = base_translation u_log |> symb_wp in
  let real_wp = base_translation u_rea |> symb_wp in
  let log1_wp = symb_wp u_log_add1 in
  let rean_wp = symb_wp u_rea_addn in
  let () =
    Printf.printf "Base logical: \n %s \n Base Real: \n %s \n Logical Add1: \n %s \n Real Add-N: \n %s \n\n "
      (string_of_cmd (base_translation u_log))
      (string_of_cmd (base_translation u_rea))
      (string_of_cmd u_log_add1)
      (string_of_cmd u_rea_addn) in

  let () =
    Printf.printf "Assumption logical: \n %s \n Assumption Real: \n %s \n Logical Add1: \n %s \n Real Add-N: \n %s \n\n "
      (string_of_test log_wp)
      (string_of_test real_wp)
      (string_of_test log1_wp)
      (string_of_test rean_wp) in
  let () =
    Printf.printf "RHOVARS HAS %d elements\n%!" (List.length rhoVars) in
  match checkSynthProblem rhoVars log_wp real_wp log1_wp rean_wp with
  | None -> Printf.printf "------unsat-----\n%!"; `Yes
  | Some x -> Printf.printf "++++++++sat!+++++\n%!"; `NoAndCE (Packet.from_CE x)

              
  
let cegis ?fvs:(fvs = []) ?gas:(gas=1000) (logical : cmd) (real : cmd) =
  let fvs = if fvs = []
            then (Printf.printf "Computing the FVS!\n%!";
                  free_vars_of_cmd logical @ free_vars_of_cmd real)
            else fvs in
  let implements_time = ref Time.Span.zero in
  let implements_calls = ref 0 in
  let model_time = ref Time.Span.zero in
  let model_calls = ref 0 in
  let wp_time = ref Time.Span.zero in
  let log_wp_time = ref Time.Span.zero in
  let phys_wp_time = ref Time.Span.zero in
  let rec loop gas real =
    Printf.printf "======================= LOOP (%d) =======================\n%!" (gas);
    let (res, z3time, log_time, phys_time) = implements fvs logical real in
    implements_time := Time.Span.(!implements_time + z3time);
    implements_calls := !implements_calls + 1;
    log_wp_time := Time.Span.(!log_wp_time + log_time);
    phys_wp_time := Time.Span.(!phys_wp_time + phys_time);
    match Printf.printf "==++?+===++?\n%!"; res with
    | `Yes ->
       Some real
    | `NoAndCE counter ->
       if gas = 0 then Some real else
         let (real', ex_z3_time, ncalls, wpt) = solve_concrete ~fvs ~packet:(Some counter) logical real in
         model_time := Time.Span.(!model_time + ex_z3_time);
         model_calls := !model_calls + ncalls;
         wp_time := Time.Span.(!wp_time + wpt);
         loop (gas-1) real'
  in
  let res = loop gas real in
  Printf.printf "total z3 time to synthesize %s + %s = %s\n%!"
    (Time.Span.to_string !implements_time)
    (Time.Span.to_string !model_time)
    (Time.Span.(to_string (!implements_time + !model_time)));
  (res, !implements_time, !implements_calls, !model_time, !model_calls, !wp_time, !log_wp_time, !phys_wp_time)
    
let synthesize ?fvs:(fvs=[]) logical real =
  let start = Time.now () in
  let (prog, checktime, checkcalls, searchtime, searchcalls, wpt, lwpt, pwpt) =
    cegis ~fvs ~gas:2 logical real in
  let comp_prog = Option.value ~default:(Assert False) prog (*|> complete*) in
    let stop = Time.now() in
    Printf.printf "\nSynthesized Program:\n%s\n\n%!"
      (string_of_cmd (comp_prog));
    (Time.diff stop start, checktime, checkcalls, searchtime, searchcalls, wpt, lwpt, pwpt, comp_prog)


let apply_edit inst (tbl, edit) =
  StringMap.add_multi inst ~key:tbl  ~data:edit
  
let rec apply_inst tag ?cnt:(cnt=0) inst prog : (cmd * int) =
  match prog with
  | Skip 
    | SetLoc _
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
  | Apply (tbl, keys, acts, def) ->
     let actSize = log2(List.length acts) in
     let selects =
       StringMap.find_multi inst tbl
       |> List.fold ~init:[]
            ~f:(fun acc (matches, action) ->
              (List.fold2_exn keys matches
                 ~init:(LocEq cnt)
                 ~f:(fun acc x m ->
                   acc %&% (Var1 x %=% m))
              , ((* (tbl ^ "_actID") %<-% mkVInt(action, actSize)
                  * %:% *) (List.nth_exn acts action)
                 %:% SetLoc (cnt (*+ 1*))))
              :: acc)
     in
     let add_row_hole = Hole1 ("?AddRowTo" ^ tbl, 1) in
     let which_act_hole = Hole1 ("?ActIn" ^ tbl, actSize) in
     let holes =
       match tag with
       | `WithHoles -> 
          List.mapi acts
            ~f:(fun i a -> 
              (List.fold keys ~init:(LocEq cnt)
                 ~f:(fun acc (x,sz) -> acc %&% (Var1 (x,sz) %=% Hole1 ("?"^x,sz)))
               %&% (add_row_hole %=% mkVInt (1,1))
               %&% (which_act_hole %=% mkVInt (i,actSize))
              , a %:% SetLoc (cnt(*+1*))))
       | `NoHoles -> []
     in
     let dflt_row =
       let cond =
         LocEq cnt %&%
           match tag with
           | `WithHoles -> True (*add_row_hole %=% mkVInt (0,1)*)
           | `NoHoles -> True in
       [(cond, def %:% SetLoc (cnt (*+ 1*))) ] in
     (selects @ holes @ dflt_row |> mkOrdered
     , cnt (*+ 1*))

   
let synthesize_edit ?fvs:(fvs=[])
      (log_pipeline : cmd) (phys_pipeline : cmd)
      (linst :  (expr1 list * int) list StringMap.t)
      (pinst : (expr1 list * int) list StringMap.t)
      (ledit : (string * (expr1 list * int))) =  
  let linst' = apply_edit linst ledit in
  let (log,_) = apply_inst `NoHoles linst' log_pipeline in
  let (phys,_) = apply_inst `WithHoles pinst phys_pipeline in
  Printf.printf "Logical:\n%s\n\nPhysical:\n%s\n"
    (string_of_cmd log) (string_of_cmd phys);
  synthesize ~fvs (SetLoc 0 %:% log) (SetLoc 0 %:% phys)
    
