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

let string_vars vs =
  let first = ref true in
  List.fold vs ~init:""
    ~f:(fun acc (v,sz) ->
      let s = Printf.sprintf "%s%s%s#%d" acc (if !first then "" else "; ") v sz in
      first := false;
      s
    )
  |> Printf.sprintf "[%s]"


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
  if params.debug then begin
      Printf.printf "\n%s\n%!" (Problem.to_string params problem);
      Interactive.pause true;
    end


let backtracking (_ : Parameters.t) =
  Printf.printf "those edits were wrong, backtracking\n%!"

let print_and_return_test ?(pre="") ?(post="") debug t =
  if debug then Printf.printf "%s%s%s%!" pre (string_of_test t) post;
  t


let edit_cache_miss d =
  if d then Printf.printf "tried edit_cache, missed\n%!"

let edit_cache_hit d es =
  if d then begin
      Printf.printf "tried edit_cache, hit!\n";
      print_edits es;
      Printf.printf "---\n%!"
    end


let check_attempts do_check problem =
  if do_check then
    match (List.find (Problem.attempts problem)
             ~f:(fun m -> True = Manip.fixup_test m (Problem.model_space problem))) with
    | None -> ()
    | Some model ->
       Printf.printf "Model\n %s \n is allowed by model space:\n%s\n\n There are %d total attempts\n%!"
         (string_of_map model)
         (string_of_test (Problem.model_space problem))
         (List.length (Problem.attempts problem));
       failwith "Duplicate model"


(* Check whether the quantifiers have all been eliminated *)
let check_qe do_check test =
  if do_check then
    let frees =
      free_of_test `Var test
      |> List.dedup_and_sort ~compare:(fun (x,_) (y,_) -> String.compare x y)
    in
    if List.is_empty frees
    then ()
    else begin
        Printf.printf "%d quantified variables have not been eliminated. Still have: %s"
          (List.length frees)
          (string_vars frees);
        failwith ""
      end
