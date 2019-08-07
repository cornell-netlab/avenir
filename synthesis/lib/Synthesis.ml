open Core
open Ast
open Semantics
open Graph
open Prover
open Manip

let well_formed (e:expr) : bool =
  let well_formed_selects (ss : (test * expr) list) : bool =
    no_nesting ss
    && instrumented ss
    (* && no_topo_loops ss *)
    && no_negated_holes ss
  in
  match e with
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

(** Plug_holes makes tests with holes always false assignemnts with holes abort **)
let rec plug_holes real =
  let binop ctor call left right = ctor (call left) (call right) in
  let rec plug_holes_test t =
    match t with
    (* Base *)
    | True | False | LocEq _ -> t
    (* The real work -- here we plug the holes in tests *)
    | Eq (Hole _, _) | Eq (_, Hole _)
    | Lt (Hole _, _) | Lt (_, Hole _)  -> False
    (* If there's no hole, make no change *)
    | Eq (_, _)      | Lt (_,_)        -> t
    (* Homomorphic cases *)
    | And (p, q) -> binop mkAnd plug_holes_test p q
    | Or (p, q) -> binop mkAnd plug_holes_test p q
    | Neg p ->
      if plug_holes_test p = p then
        Neg p
      else
        False
  in
  (* let has_hole_test c = c = plug_holes_test c in
   * let has_hole_expr e = e = plug_holes e in *)
  let plug_holes_select =
    List.map ~f:(fun (c, a) ->
        (plug_holes_test c, plug_holes a))
  in
  match real with
  | Skip -> Skip
  | SetLoc i -> SetLoc i
  | Assign (_, Hole _) -> Skip
  | Assign (_, _) -> real
  | Assert t -> Assert (plug_holes_test t)
  | Assume t -> Assume (plug_holes_test t)
  | Seq (p, q) -> plug_holes p %:% plug_holes q
  | While (cond, body) -> While(plug_holes_test cond, plug_holes body)
  | Select (styp,es) -> plug_holes_select es |> mkSelect styp


(** Solves the inner loop of the cegis procedure. 
 * pre-condition: pkt is at an ingress host 
**)
let get_one_model (pkt : Packet.t) (logical : expr) (real : expr) =
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
       let path_expr = get_program_of_rev_path real_graph path in
       let condition = Packet.to_test (fst pkt_loc') in
       let wp_of_path = wp path_expr condition in
       let _ = Printf.printf "WEAKEST_PRECONDITION:\n(%s) =\nwp(%s, %s)\n\n%!"
                   (string_of_test wp_of_path)
                   (string_of_expr path_expr)
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
             let real' = fill_holes real model |> plug_holes in
             Printf.printf "FILLED HOLES WITH %s TO GET:\n%s\n%!\n" (string_of_map model) (string_of_expr real');
             match trace_eval real' (pkt,None) with
             | None ->
               Printf.printf "No Match!\n%!";
               find_match rest_paths
             | Some (_,_) ->
               Printf.printf "Found a match!!\n%!";
               model
  end in 
  find_match all_traces
					
let fixup_val v model : value =
  match v with
  | Int _ | Var _ -> v
  | Hole h -> 
    match StringMap.find model h with
    | None -> v
    | Some v' -> v'

let rec fixup_test t model =
  let binop ctor call left right = ctor (call left model) (call right model) in 
  match t with
  | True | False | LocEq _ -> t
  | Neg p -> mkNeg (fixup_test p model)
  | And(p, q) -> binop mkAnd fixup_test p q
  | Or(p, q) -> binop mkOr fixup_test p q
  | Eq (v, w) -> binop mkEq fixup_val v w
  | Lt (v, w) -> binop mkLt fixup_val v w

let rec fixup_selects es model =
  match es with
  | [] -> []
  | (cond, act)::es' ->
    let cond' = fixup_test cond model in
    let act' = fixup act model in
    (cond', act') :: (
      if cond = cond' && act = act' then
        fixup_selects es' model
      else
        (cond, act) :: fixup_selects es' model
    )
    
and fixup (real:expr) (model : value StringMap.t) : expr =
  (* Printf.printf "FIXUP WITH MODEL: %s\n%!\n" (string_of_map model); *)
  match real with
  | Skip -> Skip
  | SetLoc l -> SetLoc l
  | Assign (f, v) -> Assign(f, fixup_val v model)
  | Assert t -> Assert (fixup_test t model)
  | Assume t -> Assume (fixup_test t model)
  | Seq (p, q) -> Seq (fixup p model, fixup q model)
  | While (cond, body) -> While (fixup_test cond model, fixup body model)
  | Select (styp,exprs) -> fixup_selects exprs model |> mkSelect styp



let implements logical real =
  let u_log = unroll (diameter logical) logical in
  let u_rea = unroll (diameter real) real |> plug_holes in
  let fvs = List.dedup_and_sort ~compare (free_vars_of_expr u_log @ free_vars_of_expr u_rea) in
  let symbolic_pkt =
    List.fold fvs  ~init:(True, 0)
      ~f:(fun (acc_test, fv_count) var ->
          (Var var %=% Var ("$" ^ string_of_int fv_count)
           %&% acc_test
          , fv_count + 1)
        )
    |> fst
  in
  let log_wp  = wp u_log symbolic_pkt in
  let real_wp = wp u_rea symbolic_pkt in
  Printf.printf "SYMBOLIC PACKET:\n%s\n%!\n\nLOGICAL SPEC:\n%s\n\nLOGICAL PROGRAM:\n%s\n\nREAL SPEC: \n%s\n\nREAL PROGRAM:\n%s\n\n%!"
    (string_of_test symbolic_pkt)
    (string_of_test log_wp)
    (string_of_expr u_log)
    (string_of_test real_wp)
    (string_of_expr u_rea);
  match check_valid (log_wp %=>% real_wp) with
  | None -> Printf.printf "valid\n%!"; `Yes
  | Some x -> `NoAndCE (Packet.from_CE x) 
                   
(** solves the inner loop **)
let solve_concrete ?packet:(packet=None) (logical : expr) (real : expr) =
  let fvs = free_vars_of_expr logical @ free_vars_of_expr real in
  let values = multi_ints_of_expr logical in
  let pkt = packet |> Option.value ~default:(Packet.generate fvs ~values) in
  let model = get_one_model pkt logical real in
  let real' =  fixup real model in
  Printf.printf "\n\nNEXT ITERATION OF REAL PROGRAM:\n%s\n%!\n" (string_of_expr real');
  real'


let cegis ?gas:(gas=1000) (logical : expr) (real : expr) =
  let rec loop gas real =
    Printf.printf "======================= LOOP (%d) =======================\n%!" (gas);
    match implements logical real with
    | `Yes -> Some (real |> plug_holes) 
    | `NoAndCE counter ->
      if gas = 0 then Some real else
      solve_concrete ~packet:(Some counter) logical real |> loop (gas-1)
  in
  solve_concrete logical real |> loop gas
    
let synthesize logical real =
  Printf.printf "\nSynthesized Program:\n%s\n\n%!"
    (cegis ~gas:10 logical real
     |> Option.value ~default:(Assert False)
     |> plug_holes
     |> string_of_expr)
