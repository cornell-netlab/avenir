open Core
open Ast
open Semantics
open Graph
open Prover
open Manip


(* Computes the traces between two points in graph *)
let find_traces (graph:graph) (in_pkt : Packet.t) (out_pkt : Packet.t) =
  let in_loc  = Packet.get_val in_pkt  "loc" in
  let out_loc = Packet.get_val out_pkt "loc" in
  let traces = get_all_paths_between graph in_loc out_loc in
  let _ = Printf.printf "ALL PATHS from %d to %d ;\n" in_loc out_loc;
          List.iter traces ~f:(fun tr ->
              Printf.printf "\t[ ";
              List.iter tr ~f:(Printf.printf "%d ");
              Printf.printf "]\n%!") in
  traces
   

(** Solves the inner loop of the cegis procedure. 
 * pre-condition: pkt is at an ingress host 
**)
let get_one_model (pkt : Packet.t) (logical : expr) (real : expr) =
  let pkt', log_trace = trace_eval logical pkt |> Option.value_exn in
  (* let log_graph = make_graph logical in *)
  let _ = Printf.printf "[LOG] get_program_of logical path ";
          List.iter log_trace ~f:(Printf.printf "%d ");
          Printf.printf "\n%!" in
  (* let log_trace_expr = get_program_of_rev_path log_graph (List.rev log_trace) in *)
  let real_graph = make_graph real in
  let _ = Printf.printf "REAL GRAPH:\n%s\n%!" (string_of_graph real_graph) in
  let all_traces = find_traces real_graph pkt pkt' in
  let _ = Printf.printf "[LOG] There are %d traces\n%!" (List.length all_traces) in
  let rec find_match traces = match traces with
    | [] -> (failwith "Cannot implement logical network in real network : No path")
    | path :: rest_paths ->
       let _ = Printf.printf "[LOG] Check real path ";
               List.iter (List.rev path) ~f:(Printf.printf "%d ");
               Printf.printf "\n%!" in
       let path_expr = get_program_of_rev_path real_graph path in
       let condition = Packet.to_test pkt' in
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
	   match check condition with
           | None -> Printf.printf "unsat!\n%!"; (find_match rest_paths)
           | Some model ->
	      (* Printf.printf "The model: %s\n" (string_of_map model); *)
              let real' = fill_holes real model in
              match trace_eval real' pkt with
              | None -> find_match rest_paths
              | Some (_,_) -> model
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
  | True | False -> t
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
  match real with
  | Skip -> Skip
  | Assign (f, v) -> Assign(f, fixup_val v model)
  | Assert t -> Assert (fixup_test t model)
  | Assume t -> Assume (fixup_test t model)
  | Seq (p, q) -> Seq (fixup p model, fixup q model)
  | While (cond, body) -> While (fixup_test cond model, fixup body model)
  | PartialSelect exprs -> fixup_selects exprs model |> PartialSelect
  | TotalSelect exprs -> fixup_selects exprs model |> TotalSelect




(** Plug_holes makes tests with holes always false assignemnts with holes abort **)
let rec plug_holes real =
  let binop ctor call left right = ctor (call left) (call right) in
  let rec plug_holes_test t =
    match t with
    (* Base *)
    | True | False -> t
    (* The real work -- here we plug the holes in tests *)
    | Eq (Hole _, _) | Eq (_, Hole _)
    | Lt (Hole _, _) | Lt (_, Hole _)  -> False
    (* If there's no hole, make no change *)
    | Eq (_, _)      | Lt (_,_)        -> t
    (* Homomorphic cases *)
    | And (p, q) -> binop mkAnd plug_holes_test p q
    | Or (p, q) -> binop mkAnd plug_holes_test p q
    | Neg p -> mkNeg (plug_holes_test p)
  in
  let plug_holes_select =
    List.map ~f:(fun (c, a) -> (plug_holes_test c, plug_holes a))
  in
  match real with
  | Skip -> Skip
  | Assign (_, Hole _) -> Assert False
  | Assign (_, _) -> real
  | Assert t -> Assert (plug_holes_test t)
  | Assume t -> Assume (plug_holes_test t)
  | Seq (p, q) -> plug_holes p %:% plug_holes q
  | While (cond, body) -> While(plug_holes_test cond, plug_holes body)
  | PartialSelect es -> PartialSelect (plug_holes_select es)
  | TotalSelect es -> TotalSelect (plug_holes_select es)


let path_specification (path_prog : expr) =
  failwith "TODO"

let specification (prog : expr) =
  let graph = make_graph prog in
  let paths = get_all_paths graph in
  concatMap paths ~c:(%&%)
    ~f:(fun path ->
        get_program_of_rev_path path
        |> path_specification 
      )

let implements n logical real correct =
  let u_log = unroll n logical in
  let u_rea = unroll n real |> plug_holes in
  match check_valid specification u_log %=>% specification u_rea) with
  | None -> `Yes
  | Some x -> `NoAndCE (Packet.from_CE x) 
                   
(** solves the inner loop **)
let solve_concrete ?packet:(packet=None) (logical : expr) (real : expr) =
  let fvs = free_vars_of_expr logical @ free_vars_of_expr real in
  let pkt = packet |> Option.value ~default:(Packet.generate fvs) in
  let model = get_one_model pkt logical real in
  fixup real model


let cegis ?gas:(gas=1000) (logical : expr) (real : expr) (correct : test) (unroll : int) =
  let rec loop gas real =
    if gas = 0 then None else 
    match implements unroll logical real correct with
    | `Yes -> Some real
    | `NoAndCE counter -> 
       solve_concrete ~packet:counter logical real |> loop (gas-1)
  in
  solve_concrete logical real |> loop gas
    
let synthesize logical real = cegis ~gas:1000 logical real ((*WRONG*)True) 10
  
