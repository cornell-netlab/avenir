open Core
open Ast    
   
let rec check_test (cond : test) (pkt_loc : Packet.located) : bool =
  let (pkt, loc) = pkt_loc in
  match cond with
  | True -> true
  | False -> false
  | LocEq testLoc ->    
    (* Printf.printf "\tTESTING LOCATION: %d\n%!" testLoc; *)
    begin match loc with
         | None -> false
         | Some l -> (l = testLoc)
    end
  | Neg (cond) -> not (check_test cond pkt_loc)
  | And (a, b) -> check_test a pkt_loc && check_test b pkt_loc
  | Or (a, b) -> check_test a pkt_loc || check_test b pkt_loc
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
                        
(*
  TODO:
	1 - This function assumes that "loc" is a field in the packet header which is incorrect in general
	2 - The semantics of assume and assert is different
*)
	 
let rec trace_eval ?gas:(gas=10) (expr : expr) (pkt_loc : Packet.located) : (Packet.located * (int list)) option =
  (* Printf.printf "\n###TRACE EVAL\nPROGRAM:\n%s\n\tPACKET: %s\n\tLOCATION: %s\n%!"
   *   (string_of_expr expr)
   *   (Packet.string_of_packet (fst pkt_loc))
   *   (match snd pkt_loc with
   *    | None -> "None"
   *    | Some l -> string_of_int l); *)
  let rec find_match ss ~default:default =
    match ss with 
    | [] -> default ()
    | (cond, action) :: rest ->
       if check_test cond pkt_loc then
         action
       else
         find_match rest ~default
  in
  let (pkt, loc_opt) = pkt_loc in
  if gas = 0
  then (Printf.printf "========OUT OF EVAL GAS============\n"; None)
  else match expr with
    | Skip ->
      Some (pkt_loc, [])
    | SetLoc i ->
      (* Printf.printf "\tSetting Loc to %d\n" i; *)
      Some ((pkt, Some i), [i])
    | Assign (f, v) ->
      Some ((Packet.set_field_of_value pkt f v, loc_opt), [])
    | Assert (t) ->
      if check_test t pkt_loc then
        Some (pkt_loc, [])
      else
        failwith ("AssertionFailure: " ^ string_of_test t ^ "was false")
    | Assume _ ->
      Some (pkt_loc, [])
    | Seq (firstdo, thendo) ->
      let open Option in
      trace_eval ~gas firstdo pkt_loc >>= fun (pkt_loc', trace') ->
      trace_eval ~gas thendo pkt_loc' >>= fun (pkt_loc'', trace'') ->
      Some (pkt_loc'', trace' @ trace'')
    | TotalSelect selects ->
      let abort _ = failwith "SelectionError: Could not find match in select" in
      trace_eval ~gas (find_match selects ~default:abort) pkt_loc
    | PartialSelect selects ->
      trace_eval (find_match selects ~default:(fun _ -> Skip)) pkt_loc
    |  While ( cond , body ) ->
      if check_test cond pkt_loc then
        trace_eval ~gas:(gas-1) (Seq(body,expr)) pkt_loc
      else 
        Some (pkt_loc, [])
          
