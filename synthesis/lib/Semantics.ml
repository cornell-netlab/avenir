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
     | (Hole _, _ ) | (_, Hole _) -> true)
  | Lt p ->
     let valOf = Packet.get_val pkt in
     match p with
     | (Int i, Int i') -> i < i'
     | (Var v, Var v') -> valOf v < valOf v'
     | (Int i, Var v) | (Var v, Int i) -> i < valOf v
     | (Hole _, _ ) | (_, Hole _) -> true

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
	 
let rec trace_eval ?gas:(gas=1000) (expr : expr) (pkt : Packet.t) : (Packet.t * (int list)) option =
  let rec find_match ss ~default:default =
    match ss with 
    | [] -> default ()
    | (cond, action) :: rest ->
       if check_test cond pkt then
         action
       else
         find_match rest ~default
  in
  if gas = 0
  then None
  else match expr with
       | Skip -> Some (pkt, [])
       | Assign ("loc", Int i ) ->
          Some (Packet.set_field pkt "loc" i, [i])
       | Assign ("loc", _) -> failwith "Cannot assign location to a hole or variable"
        | Assign (f, v) ->
           Some (Packet.set_field_of_value pkt f v, [])
        | Assert (t) ->
           if check_test t pkt then
             Some (pkt, [])
           else
             failwith ("AssertionFailure: " ^ string_of_test t ^ "was false")
        | Assume _ -> Some (pkt, [])
        | Seq (firstdo, thendo) ->
           let open Option in
           trace_eval ~gas firstdo pkt >>= fun (pkt', trace') ->
           trace_eval ~gas thendo pkt' >>= fun (pkt'', trace'') ->
           Some (pkt'', trace' @ trace'')
        | TotalSelect selects ->
           let abort _ = failwith "SelectionError: Could not find match in select" in
           trace_eval ~gas (find_match selects ~default:abort) pkt
        | PartialSelect selects ->
           trace_eval (find_match selects ~default:(fun _ -> Skip)) pkt
        |  While ( cond , body ) ->
            if check_test cond pkt then
              trace_eval ~gas:(gas-1) (Seq(body,expr)) pkt 
            else 
              Some (pkt, [])
