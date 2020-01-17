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
    Apply(tbl n
        , [("k_" ^tbl n, 2)]
        , List.fold [0;1;2;3] ~init:[] ~f:(fun acc i -> (("x_"^tbl n) %<-% mkVInt (i,2)) :: acc)
        , ("x_"^tbl n) %<-% mkVInt (0,2)
      ) :: mk_pipeline (n-1)


let rec generate_n_insertions length n : (string * (expr1 list * int)) list =
  if n = 0 then [] else
    let i = Random.int length + 1 in
    (tbl i, ([mkVInt (Random.int 4, 2)], Random.int 4))
    :: generate_n_insertions length (n-1)

    
                       
let reorder_benchmark length max_inserts =
  let logical_pipeline = mk_pipeline length in
  let physical_pipeline = permute logical_pipeline in
  let log = List.reduce_exn logical_pipeline ~f:(%:%) in
  let phys = List.reduce_exn physical_pipeline ~f:(%:%) in
  let insertion_sequence = generate_n_insertions length max_inserts in
  let rec run_experiment i seq linst pinst =
    match seq with
    | [] -> []
    | edit::edits ->
       let (duration, _)  = synthesize_edit log phys linst pinst edit in
       (i, duration)
       :: run_experiment (i + 1) edits (apply_edit linst edit) (apply_edit pinst edit)
  in
  run_experiment 0 insertion_sequence StringMap.empty StringMap.empty
  |> List.iter ~f:(fun (_, d) ->
         Printf.printf "%s\n" (Time.Span.to_string d)
       )  
       
       
