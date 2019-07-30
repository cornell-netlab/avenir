open Core
open Ast
   
let rec check_test (cond : test) (pkt : Packet.t) : bool =
  match cond with
  | True -> true
  | False -> false     
  | Neg (cond) -> not (check_test cond pkt)
  | And (a, b) -> check_test a pkt && check_test b pkt
  | Or (a, b) -> check_test a pkt || check_test b pkt
  | Eq p ->
     (let valOf = Packet.get_val pkt in
     match p with
     | (Int i, Int i') -> i = i'
     | (Var v, Var v') -> valOf v = valOf v'
     | (Int i, Var v) | (Var v, Int i) -> i = valOf v
     | (Hole _, _ ) | (_, Hole _) -> failwith "Semantics of holes are undefined")
  | Lt p ->
     let valOf = Packet.get_val pkt in
     match p with
     | (Int i, Int i') -> i < i'
     | (Var v, Var v') -> valOf v < valOf v'
     | (Int i, Var v) | (Var v, Int i) -> i < valOf v
     | (Hole _, _ ) | (_, Hole _) -> failwith "Semantics of holes are undefined"

(** Appends two lists eliminating the equal boundary 
  * e.g. squash [1; 2; 3] [3; 4; 5] == [1; 2; 3; 4; 5]
  **)
let squash xs ys =
  xs @ ys
  |> List.remove_consecutive_duplicates
       ~which_to_keep:`First
       ~equal:(=)
let (%@) = squash
                        
(*
  TODO:
	1 - This function assumes that "loc" is a field in the packet header which is incorrect in general
	2 - The semantics of assume and assert is different
*)
	 
let rec trace_eval (expr : expr) (pkt : Packet.t) =
  let output_for pkt = (pkt, [Packet.get_val pkt "loc"]) in
  match expr with
  | Skip -> output_for pkt
  | Assign (f, v) ->
     output_for ( Packet.set_field_of_value pkt f v)
  | Assert (t) | Assume (t) ->
     if check_test t pkt then
       output_for pkt
     else
       failwith ("AssertionFailure: " ^ string_of_test t ^ "was false")
  | Seq ( firstdo, thendo ) ->
     let (pkt', trace') = trace_eval firstdo pkt in
     let (pkt'', trace'') = trace_eval thendo pkt' in
     (pkt'', [Packet.get_val pkt "loc"]
             %@ trace'
             %@ trace'')
     
  | TotalSelect selects ->
     let rec find_match ss =
       match ss with 
       | [] -> failwith "ABORT, COULD NOT FIND MATCH IN SELECT STATEMENT"
       | (cond, action) :: rest ->
          if check_test cond pkt then
            action
          else
            find_match rest
     in
     trace_eval (find_match selects) pkt
     
  | PartialSelect selects ->
     let rec find_match ss =
       match ss with
       | [] -> Skip (* If no case matches, just skip *)
       | (cond, action) :: rst ->
          if check_test cond pkt then
            action
          else
            find_match rst
     in
     trace_eval (find_match selects) pkt
     
  | While ( cond , body ) ->
     let _, trace = output_for pkt in
     if check_test cond pkt then
       let pkt', trace' = trace_eval (Seq(body,expr)) pkt in
       (pkt', trace %@ trace')
     else 
       (pkt, trace)
