open Core
open Ast
   
   
module Packet = struct

  module StringMap = Map.Make (String)

  type t = int StringMap.t

  let set_field pkt field i  =
    StringMap.set pkt ~key:field ~data:i
    
  let get_val pkt field =
    match StringMap.find pkt field with
    | None -> failwith ("UseBeforeDef error" ^ field)
    | Some v -> v
    
  let set_field_of_value pkt field value =
    match value with
    | Int i -> set_field pkt field i
    | Var v ->
       get_val pkt v
       |> set_field pkt field
    | Hole _ ->
       failwith "Packets cannot have holes in them"

  let init_field_to_random bound pkt v =
    set_field pkt v (Random.int bound) 

  let to_test pkt =
    StringMap.fold pkt ~init:True
      ~f:(fun ~key ~data test ->
        Var key %=% Int data
        %&% test
      )

  let empty = StringMap.empty

  let generate ?bound:(bound=10000000) vars =
    List.fold vars ~init:empty ~f:(init_field_to_random bound)

  let from_CE _ =
    failwith "How?"
    
end

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
                                   
let rec trace_eval (expr : expr) (pkt : Packet.t) =
  let output_for pkt = (pkt, [Packet.get_val pkt "loc"]) in
  match expr with
  | Skip -> output_for pkt
  | Assign (f, v) ->
     Packet.set_field_of_value pkt f v
     |> output_for
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
     
  | SelectFrom selects ->
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
