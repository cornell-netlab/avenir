open Core
open Ast
open Synthesis
open Util
open Packet

module IntMap = Map.Make(Int)
  
                        
let permute l =
  List.map l ~f:(inj_r (Random.int (List.length l)))
  |> List.sort ~compare:(fun (i, _) (j,_) -> compare i j)
  |> List.map ~f:(snd) 
              
let tbl = Printf.sprintf "tbl%d"
    
let rec mk_pipeline varsize =
  fun n ->
  if n = 0 then [] else
    (tbl n
    , [("k_" ^tbl n, varsize)]
    , [(["v"],("x_"^tbl n) %<-% Var1("v",varsize))]
    , ("x_"^tbl n) %<-% mkVInt (0,varsize)
    ) :: mk_pipeline varsize (n-1)


let rec generate_n_insertions varsize length n avail_tables maxes =
  if n = 0 then
    let _ = Printf.printf "--generated--\n%!"in
    []
  else if avail_tables = [] then
    let _ = Printf.printf "--filled up --\n%!" in
    []
  else
    let _ = Printf.printf "generating %d \n%!" n in
    let rec loop_free_match avail_tables =
      if avail_tables = []
      then None
      else
        let i = Random.int (List.length avail_tables) |> List.nth_exn avail_tables in
        let max_i = StringMap.find maxes (tbl i) |> Option.value ~default:0 in
        Printf.printf "%s max : %d\n%!" (tbl i) max_i;
        if max_i >= pow 2 varsize
        then loop_free_match (List.filter avail_tables ~f:((<>) i))
        else
          let (max', mtch) =
            if Random.int 6 < 1 then
              (max_i + 1, Exact (max_i, varsize))
            else
              let lo = max_i in
              let hi = min (lo + Random.int 3) (pow 2 varsize - 1) in
              if lo = hi then
                (hi + 1, Exact (hi, varsize))
              else
                (hi + 1, Between (lo, hi, varsize))
          in
          let maxes' = StringMap.set maxes ~key:(tbl i) ~data:max' in
          let act_data = (Random.int (pow 2 varsize),varsize) in
          let row = ([mtch], [act_data], 0) in
          Some (maxes', avail_tables, tbl i, row)
    in
    match loop_free_match avail_tables with
    | None ->
       let _ = Printf.printf "--filled up --\n%!" in
       []
    | Some (maxes', avail_tables', name, row) ->
       let _ = Printf.printf "Inserting\n%!" in
       (name, row)
       :: generate_n_insertions varsize length (n-1) avail_tables' maxes'
                                  
let reorder_benchmark varsize length max_inserts =
  Random.init 99;
  let logical_pipeline = mk_pipeline varsize length in
  let physical_pipeline = permute logical_pipeline in
  let mk_empty_inst acc (name,_,_,_) = Map.set acc ~key:name ~data:[]  in
  let linst = List.fold logical_pipeline ~init:StringMap.empty ~f:mk_empty_inst in
  let pinst = List.fold logical_pipeline ~init:StringMap.empty ~f:mk_empty_inst in
  let to_cmd line =  List.((line >>| fun t -> Apply t)
                           |> reduce_exn ~f:(%:%)) in
  (* let hints = Some(fun vMap -> (\*[vMap]*\)
   *                 [List.fold ~init:vMap (range_ex 1 (length + 8))
   *                   ~f:(fun acc i ->
   *                     StringMap.set acc ~key:("?AddRowTo" ^ tbl i)
   *                       ~data:(mkVInt(1,1))
   *                 )]
   *               ) in *)
  let hints = Some (List.return) in
  (* let hints = None in *)
  let log = to_cmd logical_pipeline in
  let phys = to_cmd physical_pipeline in
  let insertion_sequence = 
    generate_n_insertions varsize length max_inserts (range_ex 1 (length +1)) StringMap.empty
  in
  let fvs = range_ex 1 (length + 1)
            |> List.map ~f:(fun i ->
                   [("k_" ^tbl i, 32)
                   ; (symbolize("k_" ^tbl i), 32)
                   ; (symbolize("x_" ^tbl i), 32)
                       (* ; ("?ActIn"^tbl i, 8) *)
                 ])
            |> List.join
  in
  let rec run_experiment i seq linst pinst =
    match seq with
    | [] -> []
    | edit::edits ->
       Printf.printf "==== BENCHMARKING INSERTION OF (%s) =====\n%!"
         (string_of_edit edit);
       let (totalt,checkt,checkn, searcht, searchn, wpt,lwpt,pwpt,sizes,pinst')  =
         synthesize_edit ~gas:5 ~fvs ~hints ~iter:i (Prover.solver ()) log phys linst pinst edit in
       Printf.printf "=== DONE=================================\n%!";
       (i, totalt, checkt, checkn, searcht, searchn, wpt,lwpt,pwpt, sizes)
       :: run_experiment (i + 1) edits (apply_edit linst edit) pinst'
  in
  let data = run_experiment 0 insertion_sequence linst pinst in
  let mean ds = List.fold ds ~init:0 ~f:((+)) / List.length ds in
  let max_l ds = List.fold ds ~init:0 ~f:(max) in
  let min_l ds = List.fold ds ~init:(max_l ds) ~f:(min) in
  Printf.printf "size,time,check_time,num_z3_calls_check,model_search_z3_time,num_z3_calls_model_search,search_wp_time,check_log_wp_time,check_phys_wp_time,mean_tree_size,max_tree_size,min_tree_size\n";
  List.iter data ~f:(fun (i,t,c,cn,s,sn,wpt,lwpt, pwpt,sizes) ->
      Printf.printf "%d,%f,%f,%d,%f,%d,%f,%f,%f,%d,%d,%d\n"
        i (Time.Span.to_ms t)
        (Time.Span.to_ms c) cn
        (Time.Span.to_ms s) sn
        (Time.Span.to_ms wpt)
        (Time.Span.to_ms lwpt)
        (Time.Span.to_ms pwpt)
        (mean sizes)
        (max_l sizes)
        (min_l sizes)
    )
