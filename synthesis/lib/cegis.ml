open Core
open Ast
open Semantics
open Graph
open Prover
open Manip


(* Computes the trace between two programs *)
let find_trace (graph:graph) (in_pkt : Packet.t) (out_pkt : Packet.t) =
  let in_loc  = Packet.get_val in_pkt  "loc"in
  let out_loc = Packet.get_val out_pkt "loc"in
  let all_paths_to_pkt = get_all_paths graph ~ingress:(Some in_loc) ~egress:(Some out_loc) in
  List.fold all_paths_to_pkt ~init:None
    ~f:(fun acc rev_path ->
      match acc with
      | Some _ -> acc
      | None ->
         let path_expr = get_program_of_rev_path graph rev_path in
         let wp_of_path = wp path_expr (Packet.to_test in_pkt) in
         if wp_of_path = False
         then None
         else Some wp_of_path
    )
   

(** Solves the inner loop of the cegis procedure. *)
let get_one_model (pkt : Packet.t) (logical : expr) (real : expr) : Z3.Model.model =
  let pkt' = eval logical pkt in
  let log_trace = failwith "" in
  let real_graph = make_graph real in
  match find_trace real_graph pkt pkt' with
  | None -> failwith "Cannot implement logical network in real network : No path"
  | Some real_wp ->
     match checkModel (wp log_trace (Packet.to_test pkt') %=>% real_wp) with
     | None -> failwith "Cannot implement logical network in real network : No model"
     | Some model -> model

(* (\** solves the inner loop **\)
 * let inner_cegis_loop (logical : expr) (real : expr) : Either expr pkt = *) 

