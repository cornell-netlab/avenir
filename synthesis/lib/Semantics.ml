open Core
open Ast
open Util
open Manip

let rec eval_expr1 (pkt_loc : Packet.located) ( e : expr1 ) : value1 =
  let binop op e e' = op (eval_expr1 pkt_loc e) (eval_expr1 pkt_loc e') in
  match e with
  | Value1 v -> v
  | Var1 (v,_) -> Packet.get_val (fst pkt_loc) v
  | Hole1 (h,_) -> Packet.get_val (fst pkt_loc) h
  | Plus  (e, e') -> binop add_values1 e e'
  | Times (e, e') -> binop multiply_values1 e e'
  | Minus (e, e') -> binop subtract_values1 e e'
  | Tuple es -> List.map es ~f:(eval_expr1 pkt_loc) |> VTuple


let rec eval_expr2 (pkt_loc : Packet.located) ( e : expr2 ) : value2 =
  match e with
  | Value2 v -> v
  | Var2 (s,_) -> failwith ("Cannot evaluate (symbolic) second-order variable " ^ s)
  | Hole2 (s,_) -> failwith("Cannot evaluate (symbolic) second-order hole " ^ s)
  | Single e -> eval_expr1 pkt_loc e  |> VSingle
  | Union (set, set') -> VUnion(eval_expr2 pkt_loc set, eval_expr2 pkt_loc set')
  

let rec memberVal (elem : value1) (set : value2) : bool =
  match set with
  | Empty -> false
  | VSingle v -> elem = v
  | VUnion (setl, setr) -> memberVal elem setl || memberVal elem setr
                       
let member (pkt_loc : Packet.located) (e : expr1) (set : expr2) : bool =
  memberVal (eval_expr1 pkt_loc e) (eval_expr2 pkt_loc set)
                   
let rec check_test (cond : test) (pkt_loc : Packet.located) : bool =
  let binopt op a b = op (check_test a pkt_loc) (check_test b pkt_loc) in
  let binope op e e' = op (eval_expr1 pkt_loc e) (eval_expr1 pkt_loc e') in
  match cond with
  | True -> true
  | False -> false
  | Neg (cond) -> not (check_test cond pkt_loc)
  | And (a, b) -> binopt (&&) a b
  | Or (a, b) -> binopt (||) a b
  | Impl(a, b) -> check_test (!%(a) %+% b) pkt_loc
  | Iff (a, b) -> check_test ((!%(a) %+% b) %&% (!%b %+% a)) pkt_loc
  | Eq (e,e') -> binope (equal_values1) e e'
  | Lt (e,e') -> binope (lt_values1) e e'
  | Member (e, set) -> member pkt_loc e set


let rec find_match ?idx:(idx = 0) pkt_loc ss ~default:default =
  match ss with 
  | [] -> default ()
  | (cond, action) :: rest ->
     if check_test cond pkt_loc then
       (cond, action, idx)
     else
       (find_match ~idx:(idx+1) pkt_loc rest ~default)

let rec trace_eval ?gas:(gas=10) (cmd : cmd) (pkt_loc : Packet.located) : (Packet.located * cmd) option =
  (* Printf.printf "\n###TRACE EVAL\nPROGRAM:\n%s\n\tPACKET: %s\n\tLOCATION: %s\n%!"
   *   (string_of_cmd cmd)
   *   (Packet.string_of_packet (fst pkt_loc))
   *   (match snd pkt_loc with
   *    | None -> "None"
   *    | Some l -> string_of_int l); *)
  let (pkt, loc_opt) = pkt_loc in
  if gas = 0
  then (Printf.printf "========OUT OF EVAL GAS============\n"; None)
  else match cmd with
       | Skip ->
          Some (pkt_loc, Skip)
       | Assign (f, e) ->
          Some ((Packet.set_field_of_expr1 pkt f e, loc_opt), Assign (f, e))
       | Assert (t) ->
          if check_test t pkt_loc then
            Some (pkt_loc, Assert t)
          else
            failwith ("AssertionFailure: " ^ string_of_test t ^ "was false")
       | Assume b ->
          Some (pkt_loc, Assume b)
       | Seq (firstdo, thendo) ->
          let open Option in
          trace_eval ~gas firstdo pkt_loc >>= fun (pkt_loc', trace') ->
          trace_eval ~gas thendo pkt_loc' >>= fun (pkt_loc'', trace'') ->
          Some (pkt_loc'', trace' %:% trace'')
       | Select (styp, selects) ->
          let default _ = match styp with
            | Total   -> failwith "SelectionError: Could not find match in [if total]"
            | Partial
              | Ordered ->
               Printf.printf "[EVAL (%d)] Skipping selection, no match for %s\n"
                 (gas)
                 (string_of_test (Packet.to_test pkt));
               (True, Skip, List.length selects)
          in
          let (t, a, _) = find_match pkt_loc selects ~default in
          let open Option in 
          trace_eval ~gas a pkt_loc
          >>| fun (pkt_loc',  c) ->
          (pkt_loc', Assume t %:% a %:% c)

       | While ( _ , _ ) ->
          failwith "Cannot process while loops"
          (* if check_test cond pkt_loc then
           *   trace_eval ~gas:(gas-1) (Seq(body,cmd)) pkt_loc
           * else 
           *   Some (pkt_loc, []) *)
       | Apply _ -> failwith "Cannot Evaluate table -- need configuration"


let encode_match k m =
  match m with 
  | Exact (x,sz) -> (Var1 k %=% mkVInt(x,sz))
  | Between (lo, hi,sz) -> (Var1 k %>=% mkVInt(lo,sz)) %&% (Var1 k %<=% mkVInt(hi,sz))
                                 

let rec trace_eval_inst ?gas:(gas=10) (cmd : cmd) (inst : instance) (pkt_loc : Packet.located)
        : (Packet.located * cmd * ((int * size) list * int) StringMap.t) =
  let (pkt, loc_opt) = pkt_loc in
  if gas = 0
  then (failwith "========OUT OF EVAL GAS============\n")
  else match cmd with
       | Skip ->
          (pkt_loc, cmd, StringMap.empty)
       | Assign (f, e) ->
          ((Packet.set_field_of_expr1 pkt f e, loc_opt), cmd, StringMap.empty)
       | Assert t ->
          (* failwith "Asserts are deprecated" *)
          if check_test t pkt_loc then
            (pkt_loc, cmd, StringMap.empty)
          else
            failwith ("AssertionFailure: " ^ string_of_test t ^ "was false")
       | Assume _ ->
          (* failwith "Raw assumes are deprecated" *)
          (pkt_loc, cmd, StringMap.empty)
       | Seq (firstdo, thendo) ->
          let pkt_loc', cmd', trace' = trace_eval_inst ~gas firstdo inst pkt_loc in
          let pkt_loc'', cmd'', trace'' = trace_eval_inst ~gas thendo inst pkt_loc' in
          (pkt_loc''
          , cmd' %:% cmd''
          , StringMap.merge trace' trace'' ~f:(fun ~key:_ -> function
                | `Left v -> Some v
                | `Right v -> Some v
                | `Both (v,_) -> Some v)
          )
       | Select (styp, selects) ->
          let default _ = match styp with
            | Total   -> failwith "SelectionError: Could not find match in [if total]"
            | Partial
              | Ordered ->
               Printf.printf "[EVAL (%d)] Skipping selection, no match for %s\n"
                 (gas)
                 (string_of_test (Packet.to_test pkt));
               (True, Skip, List.length selects)
          in
          let (_, a, _) = find_match pkt_loc selects ~default in
          trace_eval_inst ~gas a inst pkt_loc

       | Apply (name, keys, actions, default) ->
          let action_to_execute (rows : row list ) =
            List.fold rows ~init:(True,None)
              ~f:(fun rst (matches, data, action) ->
                match rst  with
                | ((*missed*) _, None) -> 
                   let cond = List.fold2_exn keys matches ~init:True ~f:(fun acc k m ->
                                  acc %&% encode_match k m
                                ) in
                   if check_test cond pkt_loc
                   then ((*missed %&%*) cond, Some (data, action))
                   else ((*missed %&% !%(cond)*) True, None)
                | (_, _) -> rst
              )
          in
          begin match StringMap.find inst name with
          | None -> trace_eval_inst default inst pkt_loc 
          | Some rules ->
             begin
               match action_to_execute rules with
               | (cond, None) ->
                  let pkt',cmd', trace = trace_eval_inst default inst pkt_loc in
                  (pkt' , Assert cond %:% cmd', StringMap.set ~key:name ~data:([],List.length actions) trace )
               | (cond, Some (data, aid)) ->
                  let pkt', cmd', trace = trace_eval_inst (List.nth_exn actions aid |> bind_action_data data) inst pkt_loc in
                  (pkt', Assert cond %:% cmd', StringMap.set ~key:name ~data:(data, aid) trace)
             end
          end
       | While ( _ , _ ) ->
          failwith "Cannot process while loops"
          (* if check_test cond pkt_loc then
           *   trace_eval ~gas:(gas-1) (Seq(body,cmd)) pkt_loc
           * else 
           *   Some (pkt_loc, []) *)













                             
let eval_act (act : cmd) (pkt : Packet.t) : Packet.t =
  match trace_eval act (pkt, None) with
  | None -> failwith "Ran out of gas -- but it's an action!?"
  | Some ((pkt, _), _) -> pkt
