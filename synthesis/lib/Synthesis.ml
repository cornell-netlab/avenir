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
    && instrumented ss
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



(* Computes the traces between two points in graph *)
let find_traces (graph:graph) (in_loc : int) (out_loc : int) =
  let traces = get_all_paths_between graph in_loc out_loc in
  let _ = Printf.printf "ALL PATHS from %d to %d ;\n" in_loc out_loc;
          List.iter traces ~f:(fun tr ->
              Printf.printf "\t[ ";
              List.iter tr ~f:(Printf.printf "%d ");
              Printf.printf "]\n%!") in
  traces

(** Plug_holes makes tests with holes always true and assignemnts with holes [skip] **)
(* let rec plug_holes real =
 *   let binop ctor call left right = ctor (call left) (call right) in
 *   let rec plug_holes_test t =
 *     match t with
 *     (\* Base *\)
 *     | True | False | LocEq _ -> t
 *     (\* The real work -- Tests are replaced with False *\)
 *     | Eq (Hole _, _) | Eq (_, Hole _)
 *     | Lt (Hole _, _) | Lt (_, Hole _)  -> False
 *     (\* If there's no hole, make no change *\)
 *     | Eq (_, _)      | Lt (_,_)        -> t
 *     (\* Homomorphic cases *\)
 *     | And (p, q) -> binop mkAnd plug_holes_test p q
 *     | Or (p, q) -> binop mkAnd plug_holes_test p q
 *     | Neg p ->
 *       if plug_holes_test p = p then
 *         Neg p
 *       else
 *         False
 *   in
 *   let plug_holes_select =
 *     List.map ~f:(fun (c, a) ->
 *         (plug_holes_test c, plug_holes a))
 *   in
 *   match real with
 *   | Skip -> Skip
 *   | SetLoc i -> SetLoc i
 *   | Assign (_, Hole _) -> Skip
 *   | Assign (_, _) -> real
 *   | Assert t -> Assert (plug_holes_test t)
 *   | Assume t -> Assume (plug_holes_test t)
 *   | Seq (p, q) -> plug_holes p %:% plug_holes q
 *   | While (cond, body) -> While(plug_holes_test cond, plug_holes body)
 *   | Select (styp,es) -> plug_holes_select es |> mkSelect styp *)


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
    let hole_replace x comp =
      if falsify
      then False
      else comp x (Value1 (Int (random_int_nin domain)))
    in
    match t with
    | True | False | LocEq _ -> t
    | Neg b -> !%(complete_aux_test ~falsify b)
    | And (a, b) -> complete_aux_test ~falsify a %&% complete_aux_test ~falsify b
    | Or (a, b) -> complete_aux_test ~falsify a %+% complete_aux_test ~falsify b
    | Eq (Hole1 _, x) | Eq (x, Hole1 _) -> hole_replace x (%=%)
    | Lt (Hole1 _, x) | Lt (x, Hole1 _) -> hole_replace x (%<%)
    | Eq _ | Lt _ -> t
    | Member _ -> failwith "What do?"
  and complete_aux ~falsify cmd =
    match cmd with
    | Skip | SetLoc _ -> cmd
    | Assign (f, v) ->
      begin
        match v with
        | Hole1 _ -> f %<-% Value1 (Int (random_int_nin domain))
        | _ -> cmd
      end
    | Assert b -> Assert (complete_aux_test ~falsify b)
    | Assume b -> Assume (complete_aux_test ~falsify b)
    | Seq (c, c') -> complete_aux ~falsify c %:% complete_aux ~falsify c'
    | While (b, c) -> While (complete_aux_test ~falsify b, complete_aux ~falsify c)
    | Select (styp, ss) ->
      List.map ss
        ~f:(fun (b, c) ->
            complete_aux_test ~falsify b , complete_aux ~falsify c )
      |> mkSelect styp
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
let get_one_model (pkt : Packet.t) (logical : cmd) (real : cmd) =
  let pkt_loc', log_trace = trace_eval logical (pkt,None) |> Option.value_exn in
  let _ = Printf.printf "[LOG] get_program_of logical path ";
          List.iter log_trace ~f:(Printf.printf "%d ");
          Printf.printf "\n%!" in
  let real_graph = make_graph real in
  let _ = Printf.printf "REAL GRAPH:\n%s\n%!" (string_of_graph real_graph) in
  let in_loc = List.hd log_trace |> Option.value_exn in
  let out_loc = List.last log_trace |> Option.value_exn in
  let all_traces = find_traces real_graph in_loc out_loc in
  let _ = Printf.printf "[LOG] There are %d traces\n%!" (List.length all_traces) in
  let rec find_match traces = match traces with
    | [] -> (failwith "Cannot implement logical network in real network : No path")
    | path :: rest_paths ->
       let _ = Printf.printf "[LOG] Check real path ";
               List.iter (List.rev path) ~f:(Printf.printf "%d ");
               Printf.printf "\n%!" in
       let path_cmd = get_program_of_rev_path real_graph path in
       let condition = Packet.to_test (fst pkt_loc') in
       let wp_of_path = wp path_cmd condition in
       let _ = Printf.printf "WEAKEST_PRECONDITION:\n(%s) =\nwp(%s, %s)\n\n%!"
                   (string_of_test wp_of_path)
                   (string_of_cmd path_cmd)
                   (string_of_test condition) in
       if (wp_of_path = False) 
       then (Printf.printf "-- contradictory WP\n%!"; find_match rest_paths)
       else begin
	   let condition = Packet.to_test pkt %=>% wp_of_path in
           let _ = Printf.printf "CONDITION: \n%s\n%!" (string_of_test condition) in
	   match check `Sat condition with
           | None -> Printf.printf "unsat!\n%!"; (find_match rest_paths)
           | Some model ->
	      (* Printf.printf "The model: %s\n" (string_of_map model); *)
             let real' = fill_holes real model |> complete in
             Printf.printf "FILLED HOLES WITH %s TO GET:\n%s\n%!\n" (string_of_map model) (string_of_cmd real');
             match trace_eval real' (pkt,None) with
             | None ->
               Printf.printf "No Match!\n%!";
               find_match rest_paths
             | Some (_,_) ->
               Printf.printf "Found a match!!\n%!";
               model
  end in 
  find_match all_traces
					
let rec fixup_val (model : value1 StringMap.t) (e : expr1)  : expr1 =
  let binop op e e' = op (fixup_val model e) (fixup_val model e') in
  match e with
  | Value1 _ | Var1 _ -> e
  | Hole1 h -> 
     begin match StringMap.find model h with
     | None -> e
     | Some v' -> Value1 v'
     end
  | Plus  (e, e') -> binop mkPlus  e e'
  | Times (e, e') -> binop mkTimes e e'
  | Minus (e, e') -> binop mkMinus e e'
  | Tuple es -> List.map es ~f:(fixup_val model) |> Tuple

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
  | Member(v,set) -> Member (fixup_val model v, fixup_val2 model set)

let rec fixup_selects (model : value1 StringMap.t) (es : (test * cmd) list) =
  match es with
  | [] -> []
  | (cond, act)::es' ->
    let cond' = fixup_test model cond in
    let act' = fixup act model in
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

let symbolic_pkt fvs = 
    List.fold fvs ~init:(True, 0)
      ~f:(fun (acc_test, fv_count) var ->
          (Var1 var %=% Var1 ("$" ^ string_of_int fv_count)
           %&% acc_test
          , fv_count + 1)
        )
    |> fst

let symb_wp ?fvs:(fvs=[]) cmd =
  List.dedup_and_sort ~compare (free_vars_of_cmd cmd @ fvs)
  |> symbolic_pkt
  |> wp cmd
  
let implements logical real =
  let u_log = unroll_fully logical in
  let u_rea = unroll_fully real |> complete in
  (* let fvs = List.dedup_and_sort ~compare (free_vars_of_cmd u_log @ free_vars_of_cmd u_rea) in
   * let pkt = symbolic_pkt fvs in *)
  let log_wp  = symb_wp u_log ~fvs:(free_vars_of_cmd u_rea) in
  let real_wp = symb_wp u_rea ~fvs:(free_vars_of_cmd u_log) in
  Printf.printf "\n==== Checking Implementation =====\n\nSYMBOLIC PACKET:\nOOPS\n%!\n\nLOGICAL SPEC:\n%s\n\nLOGICAL PROGRAM:\n%s\n\nREAL SPEC: \n%s\n\nREAL PROGRAM:\n%s\n\n%!"
    (string_of_test log_wp)
    (string_of_cmd u_log)
    (string_of_test real_wp)
    (string_of_cmd u_rea);
  match check_valid (log_wp %=>% real_wp) with
  | None   -> Printf.printf "++++++++++valid++++++++++++++++++\n%!"; `Yes
  | Some x -> Printf.printf "----------invalid----------------\n%!";`NoAndCE (Packet.from_CE x) 
                   
(** solves the inner loop **)
let solve_concrete ?packet:(packet=None) (logical : cmd) (real : cmd) =
  let fvs = free_vars_of_cmd logical @ free_vars_of_cmd real in
  let values = multi_ints_of_cmd logical |> List.map ~f:(fun x -> Int x) in
  let pkt = packet |> Option.value ~default:(Packet.generate fvs ~values) in
  let model = get_one_model pkt logical real in
  let real' =  fixup real model in
  Printf.printf "\n\nNEXT ITERATION OF REAL PROGRAM:\n%s\n%!\n" (string_of_cmd real');
  real'

let check_edit (_:int) (_:cmd) (_:cmd) = failwith ""


let matchExpr ks =  List.map ks ~f:(fun x -> Var1 x)

let rec matchWF keys mVar2 i j =
  if i = j then matchWF keys mVar2 (i-1) j %&% matchWF keys mVar2 i (j-1)
  else if i < 0 || j < 0 then True
  else (Member(matchExpr keys |> Tuple, mVar2 i) %=>% !%(Member (matchExpr keys |> Tuple, mVar2 j)))

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
     let mVar2 i = Var2 ("M_"^name^"_"^string_of_int i) in
     let positiveRows : (test * cmd) list =
       List.foldi actions ~init:[]
         ~f:(fun i acc act ->
           (Member(matchExpr keys |> Tuple, mVar2 i)
           , act) :: acc )
     in
     let n = (List.length actions) - 1 in
     Assume (matchWF keys mVar2 n n)
     %:% (positiveRows @ ([catchAll positiveRows, default])
          |> mkSelect Partial)
           
                                                  
let rec add_symbolic_row (name:string) (c:cmd) =
  match c with
  | Skip
    | SetLoc _
    | Assign _
    | Assume _
    | Assert _ -> c
  | Select (styp, cmds) ->
    List.map cmds
      ~f:(fun (t, c) -> (t, add_symbolic_row name c))
    |> mkSelect styp
  | Seq (c, c')
    -> Seq(add_symbolic_row name c, add_symbolic_row name c')
  | While (b, c)
    -> While (b, add_symbolic_row name c) 
  | Apply (name', keys, actions, default) as table
    ->
     let mVar2 i = Var2 ("M_"^name^"_"^string_of_int i) in
     let fillVar1 k =  Var1 (k^"_"^name^"!FILL")in
     let actionVar = Var1 "action!FILL" in
     if name = name' then
         let row = List.map keys ~f:(fillVar1) in
         let positiveRows : (test * cmd) list =
           List.foldi actions ~init:[]
             ~f:(fun i acc act ->
               (Member(matchExpr keys |> Tuple, mVar2 i)
                %&% ((matchExpr keys |> Tuple) %<>% Tuple row)
               , act) :: acc )
           @ List.foldi actions ~init:[]
               ~f:(fun i acc act ->
                 ((actionVar :: matchExpr keys |> Tuple) %=% (mkVInt i :: row |> Tuple )
                 , act) :: acc)
         in
         let actionWF = Member(actionVar,
                                 List.foldi actions
                                   ~init:(Value2 Empty)
                                   ~f:(fun i acc _ -> mkInsert (Value1 (Int i)) acc)) in
         let n = (List.length actions) - 1 in
         Assume actionWF
         %:% Assume (matchWF keys mVar2 n n)
         %:% (positiveRows @ ([catchAll positiveRows, default])
              |> mkSelect Partial)
     else
       table


         
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
  | Apply (name, keys, actions, default)
    ->
     let mVar2 actId = Var2 ("M_"^name^"_"^string_of_int actId) in
     let fillVar1 rowId key =  Var1 (key^"_"^string_of_int rowId^"_"^name^"!FILL") in
     let actionVar rowId = Var1 ("action_"^name ^string_of_int rowId ^ "!FILL") in

     (* let delSet = Var2 ("Del_" ^ name ^"!FILL") in *)
     let rec modifiedRows i =
       if i = 0 then Value2 Empty
       else List.map keys ~f:(fillVar1 i) |> Tuple |> Single |> mkUnion (modifiedRows (i-1))
     in
       
     let positiveRows : (test * cmd) list =
       List.foldi actions ~init:[]
         ~f:(fun i acc act ->
           (Member(matchExpr keys |> Tuple, mVar2 i)
            %&% (!%(Member(matchExpr keys |> Tuple, modifiedRows n))) (*modifiedRows are new!*)
           , act) :: acc )
       @ List.foldi actions ~init:[]
           ~f:(fun actId acc act ->
             let rec newRow rowId   =
               if rowId <= 0 then []
               else (Tuple (mkVInt actId :: matchExpr keys)
                     %=% Tuple(actionVar rowId :: List.map keys ~f:(fillVar1 rowId))
                    , act) :: newRow (rowId - 1)
             in newRow n @ acc)
     in
     let l = List.length actions - 1 in
     (* let rec matchUnion i =
      *   if n < 0 then Value2 Empty
      *   else Union(mVar2 i, matchUnion (i-1))
      * in *)
     (* let delWF i =  Member(matchExpr keys |> Tuple, delSet) %=>% (Member(matchExpr keys |> Tuple, matchUnion i)) in *)

     Assume (matchWF keys mVar2 l l)
     (* %:% Assume (delWF l) *)
     %:% (positiveRows @ ([catchAll positiveRows, default])
          |> mkSelect Partial)

     
     
  
  
                             
(* Pre :: neither program has any holes in it *)
(* Currently assume that no deletions are required to Real   *)
let check_add n name logical real = 
  let u_log = unroll_fully logical in
  let u_log_add1 = add_symbolic_row name u_log in
  let u_rea = unroll_fully real in
  let u_rea_addn = concretely_instrument n u_rea in
  let fvs = free_vars_of_cmd u_rea
            @ free_vars_of_cmd u_log
            @ free_vars_of_cmd u_log_add1
            @ free_vars_of_cmd u_rea_addn in
  let log_wp  = base_translation u_log |> symb_wp ~fvs in
  let real_wp = base_translation u_rea |> symb_wp ~fvs in
  let log1_wp = symb_wp u_log_add1 ~fvs in
  let rean_wp = symb_wp u_rea_addn ~fvs in
  match check_valid ((log_wp %<=>% real_wp) %=>% (log1_wp %<=>% rean_wp)) with
  | None -> Printf.printf "+++++valid+++++\n%!"; `Yes
  | Some x -> Printf.printf "-----invalid---\n%!"; `NoAndCE (Packet.from_CE x)
              
  
let cegis ?gas:(gas=1000) (logical : cmd) (real : cmd) =
  let rec loop gas real =
    Printf.printf "======================= LOOP (%d) =======================\n%!" (gas);
    match implements logical real with
    | `Yes -> Some (real) 
    | `NoAndCE counter ->
      if gas = 0 then Some real else
      solve_concrete ~packet:(Some counter) logical real |> loop (gas-1)
  in
  solve_concrete logical real |> loop gas
    
let synthesize logical real =
  Printf.printf "\nSynthesized Program:\n%s\n\n%!"
    (cegis ~gas:3 logical real
     |> Option.value ~default:(Assert False)
     |> complete
     |> string_of_cmd)
