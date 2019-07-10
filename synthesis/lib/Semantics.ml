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

  let to_test pkt =
    StringMap.fold pkt ~init:True
      ~f:(fun ~key ~data test ->
        Var key %=% Int data
        %&% test
      )

end

let rec check_test (cond : test) (pkt : Packet.t) : bool =
  match cond with
  | True -> true
  | False -> false     
  | Neg (cond) -> not (check_test cond pkt)
  | And (a, b) -> check_test a pkt && check_test b pkt
  | Or (a, b) -> check_test a pkt || check_test b pkt
  | Eq p ->
     let valOf = Packet.get_val pkt in
     match p with
     | (Int i, Int i') -> i = i'
     | (Var v, Var v') -> valOf v = valOf v'
     | (Int i, Var v) | (Var v, Int i) -> i = valOf v
              
let rec eval (expr : expr) (pkt : Packet.t) =
  match expr with
  | Skip -> pkt
  | Assign (f, v) ->
     Packet.set_field_of_value pkt f v
  | Assert (t) ->
     if check_test t pkt then
       pkt
     else
       failwith ("AssertionFailure: " ^ string_of_test t ^ "was false")
  | Seq ( firstdo, thendo ) ->
     pkt
     |> eval firstdo
     |> eval thendo
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
     eval (find_match selects) pkt
     
  | While ( cond , body ) ->
     if check_test cond pkt then
       pkt
     else
       pkt
       |> eval body
       |> eval expr
