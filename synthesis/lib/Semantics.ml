open Core
open Ast    

let rec evalExpr (pkt_loc : Packet.located) ( e : value ) : int option =
  let binop op e e' = match evalExpr pkt_loc e, evalExpr pkt_loc e' with
    | None, _ -> None
    | _, None -> None
    | Some x, Some y -> Some (op x y)
  in
  match e with
  | Int i -> Some i
  | Var v -> Some (Packet.get_val (fst pkt_loc) v)
  | Hole _ -> None
  | Plus  (e, e') -> binop ( + ) e e'
  | Times (e, e') -> binop ( * ) e e'
  | Minus (e, e') -> binop ( - ) e e'


let rec evalExpr2 (pkt_loc : Packet.located) ( e : value2 ) : int list =
  match e with
  | Var2 s -> Printf.printf "Warning::Cannot evaluate (symbolic) second-order variable %s, skipping ...\n%!" s;
              []
  | Hole2 s -> Printf.printf "Warning::Cannot evaluate (symbolic) second-order hole %s, skipping ...\n%!" s;
               []
  | Singleton e -> begin match evalExpr pkt_loc e with
                   | None -> []
                   | Some i -> [i]
                   end
  | Union (set, set') -> evalExpr2 pkt_loc set @ evalExpr2 pkt_loc set'
  
                   
let member (pkt_loc : Packet.located) (e : value) (set : value2) =
  match evalExpr pkt_loc e with
  | None -> false
  | (Some i) -> List.exists ~f:((=) i) (evalExpr2 pkt_loc set)
      
  
                   
let rec check_test (cond : test) (pkt_loc : Packet.located) : bool =
  let binopt op a b = op (check_test a pkt_loc) (check_test b pkt_loc) in
  let binope op e e' = match evalExpr pkt_loc e, evalExpr pkt_loc e' with
    | None, _ -> true
    | _, None -> true
    | Some x, Some y -> op x y
  in
  match cond with
  | True -> true
  | False -> false
  | LocEq testLoc ->    
    (* Printf.printf "\tTESTING LOCATION: %d\n%!" testLoc; *)
    begin match snd pkt_loc with
         | None -> false
         | Some l -> (l = testLoc)
    end
  | Neg (cond) -> not (check_test cond pkt_loc)
  | And (a, b) -> binopt (&&) a b
  | Or (a, b) -> binopt (||) a b
  | Eq (e,e') -> binope (=) e e'
  | Lt (e,e') -> binope (<) e e'
  | Member (e, set) -> member pkt_loc e set
	 
let rec trace_eval ?gas:(gas=10) (cmd : cmd) (pkt_loc : Packet.located) : (Packet.located * (int list)) option =
  (* Printf.printf "\n###TRACE EVAL\nPROGRAM:\n%s\n\tPACKET: %s\n\tLOCATION: %s\n%!"
   *   (string_of_cmd cmd)
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
  else match cmd with
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
    | Select (styp, selects) ->
      let default _ = match styp with
        | Total   -> failwith "SelectionError: Could not find match in [if total]"
        | Partial
        | Ordered ->
          Printf.printf "[EVAL (%d)] Skipping selection, no match for %s\n"
            (gas)
            (string_of_test (Packet.to_test pkt %&% LocEq (Option.value loc_opt ~default:(-100))));
          Skip
      in
      trace_eval ~gas (find_match selects ~default) pkt_loc
    | While ( cond , body ) ->
      if check_test cond pkt_loc then
        trace_eval ~gas:(gas-1) (Seq(body,cmd)) pkt_loc
      else 
        Some (pkt_loc, [])
    | Apply _ -> failwith "Cannot Evaluate table -- need configuration"
