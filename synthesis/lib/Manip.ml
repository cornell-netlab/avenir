open Ast

module StringMap = Map.Make (String)

(* computes ex[x -> v] *)
let rec substitute ex subsMap = match ex with
	| True -> True
	| False -> False
	| _ -> failwith "TODO"

(* computes weakest pre-condition of condition phi w.r.t command c *)
let rec wp c phi = match c with
  | Seq (firstdo, thendo) ->
		wp firstdo (wp thendo phi)
  | Assign (field, value) ->
	  substitute phi (StringMap.add field value)
	| _ -> failwith "TODO"
  
