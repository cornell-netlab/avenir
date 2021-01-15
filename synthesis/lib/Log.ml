open Core

(* open ActionGenerator *)

let cexs (params : Parameters.t) problem log_out_pkt in_pkt =
  if params.debug then (
    let phy_gcl = Problem.phys_gcl_program params problem in
    (* let fvs = Problem.fvs problem in *)
    let phy_out_pkt = Semantics.eval_act phy_gcl in_pkt in
    Printf.printf "Counterexample found!\nin: %s\nlog:  %s\nphys:  %s\n\n%!"
      (Packet.to_string in_pkt)
      (Packet.to_string log_out_pkt)
      (Packet.to_string phy_out_pkt) ;
    Interactive.pause params.interactive )

let already_explored_error model_space (model : Model.t) =
  Printf.printf "ALREADY EXPLORED\n %s \n\n %s \n%!"
    (Test.to_string model_space)
    (Model.to_string model) ;
  let res = Manip.fixup_test model model_space in
  Printf.printf "applied \n\n\n %s\n\n\n" (Test.to_string res)

let print_edits ?(tab = false) (params : Parameters.t) phys es =
  if not params.hot_start then
    List.iter es ~f:(fun e ->
        Printf.printf "%s%s\n%!"
          (if tab then "\t" else "")
          ( if params.thrift_mode then Edit.to_bmv2_string phys e
          else Edit.to_string e ))

let string_vars vs =
  let first = ref true in
  List.fold vs ~init:"" ~f:(fun acc (v, sz) ->
      let s =
        Printf.sprintf "%s%s%s#%d" acc (if !first then "" else "; ") v sz
      in
      first := false ;
      s)
  |> Printf.sprintf "[%s]"

let print_search_state (params : Parameters.t) problem es (model : Model.t) =
  let print_space = false in
  let print_model = true in
  if params.debug then (
    let space = Problem.model_space problem in
    if print_space then
      Printf.printf "\n\t***Space***\n\t%s\n\t***     ***"
        (Test.to_string space) ;
    Printf.printf "\n\t***Edits*** (%d CEXs)\n%!"
      (List.length @@ Problem.cexs problem) ;
    print_edits params (Problem.phys problem) (Problem.phys_edits problem) ;
    Printf.printf "\t*** New ***\n%!" ;
    print_edits params (Problem.phys problem) es ;
    Printf.printf "\t***     ***\n" ;
    if print_model then (
      Printf.printf "\t***model***\n" ;
      Printf.printf "%s\n%!"
        (Model.fold model ~init:"" ~f:(fun ~key ~data acc ->
             Printf.sprintf "%s\n\t%s |--> %s" acc key (Value.to_string data)))
      (* Interactive.pause params.interactive; *) ) )

let print_problem (params : Parameters.t) (problem : Problem.t) =
  if params.debug then (
    Printf.printf "\n%s\n%!" (Problem.to_string params problem) ;
    Interactive.pause params.interactive )

let print_and_return_test ?(pre = "") ?(post = "") debug t =
  if debug then Printf.printf "%s%s%s%!" pre (Test.to_string t) post ;
  t

let edit_cache_miss d =
  if d then Printf.printf "tried edit_cache, missed\n%!"

let edit_cache_hit (params : Parameters.t) prog es =
  if params.debug then (
    Printf.printf "tried edit_cache, hit!\n" ;
    print_edits params prog es ;
    Printf.printf "---\n%!" )

let check_attempts do_check problem =
  if do_check then
    match
      List.find (Problem.attempts problem) ~f:(fun m ->
          Problem.model_space problem
          |> Manip.fixup_test m |> Test.equals Test.True)
    with
    | None -> ()
    | Some model ->
        Printf.printf
          "Model\n\
          \ %s \n\
          \ is allowed by model space:\n\
           %s\n\n\
          \ There are %d total attempts\n\
           %!"
          (Model.to_string model)
          (Test.to_string (Problem.model_space problem))
          (List.length (Problem.attempts problem)) ;
        failwith "Duplicate model"

(* Check whether the quantifiers have all been eliminated *)
let check_qe do_check test =
  if do_check then
    let frees =
      Test.vars test
      |> List.dedup_and_sort ~compare:(fun (x, _) (y, _) ->
             String.compare x y)
    in
    if List.is_empty frees then ()
    else (
      Printf.printf
        "%d quantified variables have not been eliminated. Still have: %s"
        (List.length frees) (string_vars frees) ;
      failwith "" )

let print_hints_map do_print (partial_model : Model.t) =
  if do_print then (
    Printf.printf "Hints are : {\n%!" ;
    Model.iteri partial_model ~f:(fun ~key ~data ->
        Printf.printf "\t%s -> %s\n" key (Value.to_string data)) ;
    Printf.printf "}\n%!" )

let print_hints do_print (hints : Hint.t list) =
  if do_print then (
    Printf.printf "Hints are :\n%!" ;
    List.iter hints ~f:(fun h -> Printf.printf "\t%s\n" (Hint.to_string h)) ;
    Printf.printf "\n%!" )

let log do_log str = if do_log then Printf.printf "%s%!" str
