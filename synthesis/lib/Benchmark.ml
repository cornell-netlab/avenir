open Core
open Ast
open Synthesis
open Prover
       
let permute l =
  List.map l ~f:(fun x -> (Random.int (List.length l), x))
  |> List.sort ~compare:(fun (i, _) (j,_) -> compare i j)
  |> List.map ~f:(snd) 
              
let tbl = Printf.sprintf "tbl%d"

    
let rec mk_pipeline n =
  if n = 0 then [] else
    (tbl n
    , [("k_" ^tbl n, 2)]
    , List.fold [0;1;2;3] ~init:[] ~f:(fun acc i -> (("x_"^tbl n) %<-% mkVInt (i,2)) :: acc)
    , ("x_"^tbl n) %<-% mkVInt (0,2)
    ) :: mk_pipeline (n-1)


let rec generate_n_insertions length n : (string * (expr1 list * int)) list =
  if n = 0 then [] else
    let i = Random.int length + 1 in
    (tbl i, ([mkVInt (Random.int 4, 2)], Random.int 4))
    :: generate_n_insertions length (n-1)

module IntMap = Map.Make(Int)
                             
let reorder_benchmark length max_inserts =
  let logical_pipeline = mk_pipeline length in
  let physical_pipeline = permute logical_pipeline in
  let to_cmd line =  List.((line >>| fun t -> Apply t)
                           |> reduce_exn ~f:(%:%)) in
  let log = to_cmd logical_pipeline in
  let phys = to_cmd physical_pipeline in
  let insertion_sequence = generate_n_insertions length max_inserts in
  (* let hints =
   *   let open List in
   *   (logical_pipeline >>| (fun (tbl, _, acts, _) ->
   *      List.mapi acts ~f:(fun i _-> (tbl, i))))
   *   |> RelIR.list_cross >>| fun x -> (x,x)
   * in *)
  let rec run_experiment i seq linst pinst =
    match seq with
    | [] -> []
    | edit::edits ->
       let (totalt,checkt,checkn, searcht, searchn, wpt,lwpt,pwpt,_)  = synthesize_edit log phys linst pinst edit in
       (i, totalt, checkt, checkn, searcht, searchn, wpt,lwpt,pwpt)
       :: run_experiment (i + 1) edits (apply_edit linst edit) (apply_edit pinst edit)
  in
  let data = run_experiment 0 insertion_sequence StringMap.empty StringMap.empty in
  Printf.printf "size,time,check_time,num_z3_calls_check,model_search_z3_time,num_z3_calls_model_search,search_wp_time,check_log_wp_time,check_phys_wp_time\n";
  List.iter data ~f:(fun (i,t,c,cn,s,sn,wpt,lwpt, pwpt) ->
      Printf.printf "%d,%f,%f,%d,%f,%d,%f,%f,%f\n"
        i (Time.Span.to_ms t)
        (Time.Span.to_ms c) cn
        (Time.Span.to_ms s) sn
        (Time.Span.to_ms wpt)
        (Time.Span.to_ms lwpt)
        (Time.Span.to_ms pwpt)
    )
    
       
       
       
