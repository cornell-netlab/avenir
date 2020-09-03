open Core
open Tables
open Ast
open Util
(* open ActionGenerator *)


let cexs (params : Parameters.t) problem log_out_pkt in_pkt =
  if params.debug then
    let phy_gcl = Problem.phys_gcl_program params problem in
    (* let fvs = Problem.fvs problem in *)
    let phy_out_pkt = Semantics.eval_act phy_gcl in_pkt in
    Printf.printf "Counterexample found!\nin: %s\nlog:  %s\nphys:  %s\n\n%!"
      (Packet.string__packet in_pkt)
      (Packet.string__packet log_out_pkt)
      (Packet.string__packet phy_out_pkt);
    (* let pos_acts = ActionGenerator.positive_actions params (Problem.phys problem) fvs in_pkt log_out_pkt in
     * Printf.printf "There are %d positive actions across %d tables\n%!"
     *   (List.length @@ List.bind pos_acts ~f:snd)
     *   (List.length pos_acts); *)
    (* let log_matches =  List.hd_exn (Problem.log_edits problem) |> Edit.get_matches_exn in *)
    (* let traces =
     *   ActionGenerator.traces log_edit fvs (Problem.phys problem) in_pkt log_out_pkt
     * in *)
    (* let ftables = feasible_tables (Problem.phys problem) fvs log_matches in_pkt log_out_pkt in
     * Printf.printf "There are %d feasible tables:\n\n%!" (List.length ftables);
     * List.iter ftables ~f:(fun (t,keys,vars) ->
     *     Printf.printf "%s : %s -> %s\n%!" t (string_of_strset keys) (string_of_strset vars)
     *   ); *)
    (* let reach_pos_acts = reach_positive_actions params problem in_pkt log_out_pkt in *)
    (* Printf.printf "There are %d queries of sizes: %s\n%!"
     *   (List.length reach_pos_acts)
     *   (List.map reach_pos_acts ~f:num_nodes_in_test |> string_of_intlist)
     * ; *)
    Interactive.pause params.interactive


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
  let print_space = false in
  let print_model = false in
  if do_print then begin
      let space = Problem.model_space problem in
      if print_space then
        Printf.printf "\n\t***Space***\n\t%s\n\t***     ***" (Ast.string_of_test space);

      Printf.printf "\n\t***Edits*** (%d CEXs)\n%!" (List.length @@ Problem.cexs problem);
      print_edits (Problem.phys_edits problem);

      Printf.printf "\t*** New ***\n%!";
      print_edits es;
      Printf.printf "\t***     ***\n";

      if print_model then begin
          Printf.printf "\t ***model***\n";
          Printf.printf "\t%s\n%!" (Ast.string_of_map model)
        end;
      Interactive.pause true;
    end


let print_problem (params : Parameters.t) (problem : Problem.t) =
  if params.debug then begin
      Printf.printf "\n%s\n%!" (Problem.to_string params problem);
      Interactive.pause params.interactive;
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


let print_hints_map do_print (partial_model : value StringMap.t) =
  if do_print then begin
      Printf.printf "Hints are : {\n%!";
      StringMap.iteri partial_model
        ~f:(fun ~key ~data ->
          Printf.printf "\t%s -> %s\n" key (string_of_value data)
        );
      Printf.printf "}\n%!"
    end

let print_hints do_print (hints : Hint.t list) =
  if do_print then begin
      Printf.printf "Hints are :\n%!";
      List.iter hints ~f:(fun h ->
          Printf.printf "\t%s\n" (Hint.to_string h)
        );
      Printf.printf "\n%!"
    end
