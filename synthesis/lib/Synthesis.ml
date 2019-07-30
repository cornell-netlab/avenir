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
  get_all_paths_between graph in_loc out_loc
  (* let _ = Printf.printf "ALL PATHS from %d to %d ;\n" in_loc out_loc;
   *         List.iter all_paths_to_pkt ~f:(fun path ->
   *             Printf.printf "\t[ ";
   *             List.iter path ~f:(Printf.printf "%d ");
   *             Printf.printf "]\n%!")
   *) 
   

(* Solves the inner loop of the cegis procedure. *)

(* pre-condition: pkt is at an ingress host *)

let get_one_model (pkt : Packet.t) (logical : expr) (real : expr) =
  let pkt', log_trace = trace_eval logical pkt in
  let log_graph = make_graph logical in
  let _ = Printf.printf "[LOG] get_program_of ";
          List.iter log_trace ~f:(Printf.printf "%d ");
          Printf.printf "\n%!" in
  let log_trace_expr = get_program_of_rev_path log_graph (List.rev log_trace) in
  let real_graph = make_graph real in
	let all_traces = find_traces real_graph pkt pkt' in
	let rec find_match traces = match traces with
	| [] -> (failwith "Cannot implement logical network in real network : No path")
	| rev_path :: rest_paths ->
         let path_expr = get_program_of_rev_path real_graph rev_path in
         let condition = Packet.to_test pkt' in
         let wp_of_path = wp path_expr condition in
         let _ = Printf.printf "WEAKEST_PRECONDITION:\n(%s) =\nwp(%s, %s)\n\n%!"
                   (string_of_test wp_of_path)
                   (string_of_expr path_expr)
                   (string_of_test condition) in
         if (wp_of_path = False) 
         then (find_match rest_paths)
         else begin
	     let condition = wp log_trace_expr (Packet.to_test pkt')
                             %=>% wp_of_path in
             let _ = Printf.printf "CONDITION: \n%s\n" (string_of_test condition) in
	     match check condition with
             | None -> Printf.printf "unsat!\n"; (find_match rest_paths)
             | Some model ->
		(* Printf.printf "The model: %s\n" (string_of_map model); *)
                model
	   end in 
	find_match all_traces
					

let fixup _ _ =
  failwith "TODO : the hard part"
              
let implements n logical real correct =
  let u_log = unroll n logical in
  let u_rea = unroll n real in
  match check_valid (wp u_log correct %=>% wp u_rea correct) with
  | None -> `Yes
  | Some x -> `NoAndCE (Packet.from_CE x) 
                   
(** solves the inner loop **)
let solve_concrete ?packet:(packet=None) (logical : expr) (real : expr) =
  let fvs = free_vars_of_expr logical @ free_vars_of_expr real in
  let pkt = Option.value packet ~default:(Packet.generate fvs) in
  let model = get_one_model pkt logical real in
  fixup real model


let cegis (logical : expr) (real : expr) (correct : test) (unroll : int) =
  let rec loop real =
    match implements unroll logical real correct with
    | `Yes -> real
    | `NoAndCE counter -> 
       solve_concrete ~packet:counter logical real |> loop
  in
  solve_concrete logical real |> loop
    
let synthesize p q =
  Printf.printf "Synthesize %s\n with %s \n"
    (string_of_expr p)
    (string_of_expr q)
  
