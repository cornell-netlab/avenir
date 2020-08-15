open Core
open Tables
open Ast



let cexs (params : Parameters.t) ?(log_out_pkt=None) ?(log_gcl=None) phy_gcl in_pkt =
  if params.debug then
    let log_out_pkt =
      match log_out_pkt, log_gcl with
      | Some pkt,_ -> pkt
      | None, Some log_gcl -> Semantics.eval_act log_gcl in_pkt
      | None, None ->
         failwith "Tried to print counterexample, but missing logical program _and_ logical_output"
    in
    let phy_out_pkt = Semantics.eval_act phy_gcl in_pkt in
    Printf.printf "Counterexample found!\nin: %s\nlog:  %s\nphys:  %s\n\n%!"
      (Packet.string__packet in_pkt)
      (Packet.string__packet log_out_pkt)
      (Packet.string__packet phy_out_pkt)


let already_explored_error model_space model =
  Printf.printf "ALREADY EXPLORED\n %s \n\n %s \n%!"
    (Ast.string_of_test model_space)
    (Ast.string_of_map model);
  let res = Manip.fixup_test model model_space in
  Printf.printf "applied \n\n\n %s\n\n\n" (Ast.string_of_test res)

let print_edits =
  List.iter ~f:(fun e -> Printf.printf "\t %s\n%!" (Edit.to_string e))



let print_search_state do_print problem es model =
  if do_print then begin
      let space = Problem.model_space problem in
      Printf.printf "\n\t***Space***\n\t%s\n\t***     ***" (Ast.string_of_test space);

      Printf.printf "\n\t***Edits*** (%d CEXs)\n%!" (List.length @@ Problem.cexs problem);
      print_edits (Problem.phys_edits problem);

      Printf.printf "\t*** New ***\n%!";
      print_edits es;
      Printf.printf "\t***     ***\n";

      Printf.printf "\t ***model***\n";
      Printf.printf "\t%s\n%!" (Ast.string_of_map model);
      (* ignore(Stdio.In_channel.(input_char stdin) : char option); *)
    end


let print_problem (params : Parameters.t) (problem : Problem.t) =
  if params.debug then
    Printf.printf "\n%s\n%!" (Problem.to_string params problem)


let backtracking (_ : Parameters.t) =
  Printf.printf "those edits were wrong, backtracking\n%!"

let print_and_return_test ?(pre="") ?(post="") t =
  Printf.printf "%s%s%s%!" pre (string_of_test t) post ;
  t
