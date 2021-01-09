open Core
open Ast

let remove_missed_edits params data problem es =
  List.fold es ~init:[] ~f:(fun acc e ->
      let cex = List.hd_exn (Problem.cexs problem) in
      let check = Packet.to_test ~fvs:(Problem.fvs problem) (fst cex)
                  %=>% FastCX.hits_pred params data (Problem.phys problem) (Problem.phys_inst problem) (Problem.phys_edits problem) e in
      match Prover.check_valid params check with
      | None,_ ->
         (* Printf.printf "Checked \n%s\n" (string_of_test check); *)
         acc @ [e]
      | Some _,_ -> acc)

let extract_reached_edits (params : Parameters.t) data problem model =
  let es = Edit.of_model (Problem.phys problem) model in
  let es = if params.reach_filter then
             remove_missed_edits params data problem es
           else es
  in
  if List.is_empty es then
    None
  else
    Some es
