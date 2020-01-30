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
  | Le (e,e') ->
     begin match eval_expr1 pkt_loc e, eval_expr1 pkt_loc e' with
     | Int (v,_), Int(v',_) -> v <= v'
     | _, _ -> failwith "tuples not supported"
     end
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
  | Between (lo, hi,sz) -> (mkVInt(lo,sz) %<=% Var1 k) %&% (Var1 k %<=% mkVInt(hi,sz))
                                 

let rec wide_eval wide (e : expr1) =
  match e with
  | Value1(Int(x,sz)) -> (x,x,sz)
  | Value1 _ | Tuple _ -> failwith "Tuples deprecated"
  | Var1(x, sz) ->
     begin match StringMap.find wide x with
     | None -> failwith ("USE BEFORE DEF " ^ x)
     | Some (lo,hi, sz) -> (lo,hi,sz)
     end
  | Hole1 _ -> failwith "dont know how to eval holes"
  | Plus(x,y) -> let (lox,hix, szx) = wide_eval wide x in
                 let (loy, hiy, _) = wide_eval wide y in
                 (lox + loy, hix+hiy, szx)
  | Times(x,y) -> let (lox, hix, szx) = wide_eval wide x in
                  let (loy, hiy, _) = wide_eval wide y in
                  (lox * loy, hix * hiy, szx)
  | Minus(x,y) -> let (lox, hix, szx) = wide_eval wide x in
                  let (loy, hiy, _) = wide_eval wide y in
                  (lox - hiy, hix - loy, szx)

                                                   
let widening_assignment (wide : (int*int*size) StringMap.t) f e : (int * int * size) StringMap.t =
  StringMap.set wide ~key:f ~data:(wide_eval wide e)

let rec widening_test pkt wide t =
  match t with
  | True -> wide
  | False -> StringMap.empty
  | And(b1,b2) -> widening_test pkt (widening_test pkt wide b1) b2
  | Or (b1,b2) ->
     begin match check_test b1 (pkt, None), check_test b2 (pkt,None) with
     | true, true -> widening_test pkt (widening_test pkt wide b1) b2
     | true, false -> widening_test pkt wide b1
     | false, true -> widening_test pkt wide b2
     | false, false -> failwith "shouldn't be executing a test that fails"
     end
  | Impl(b1,b2) ->
     begin match check_test b1 (pkt, None), check_test b2 (pkt,None) with
     | true, true -> widening_test pkt (widening_test pkt wide b1) b2
     | true, false -> failwith "shouldn't be executing a test that fails"
     | false, _ -> widening_test pkt wide b2
     end
  | Iff (b1,b2) -> failwith "IDK HOW TO widen iff"
  | Eq(Var1(v,sz), e) | Eq(e,Var1(v,sz)) ->
     if check_test t (pkt,None)
     then
       let (lo,hi,sz) = wide_eval wide e in
       Printf.printf "adding %s = [%d,%d]" v lo hi;
       StringMap.set wide ~key:v ~data:(lo,hi,sz)
     else failwith "shouldn't be executing a test that fails"
  | Le(Var1(v,sz), e) | Le(e,Var1(v,sz)) ->
     if check_test t (pkt,None)
     then
       let (lo, hi, sz) = wide_eval wide e in
       Printf.printf "adding %s <= [%d,%d]" v lo hi;
       StringMap.update wide v ~f:(function
           | None -> (lo, hi, sz)
           | Some (lo', hi', sz)
             ->  (min lo lo', min hi hi', sz))
     else failwith "shouldn't be executing a test that fails"
  | _ -> failwith "dont know how to handle that kind of test"
          
let widening_match pkt wide matches =
  Printf.printf "WIDENING A MATCH\n";
  List.fold matches
    ~init:wide
    ~f:(fun acc ((key,_), m)  ->
      match m with 
      | Exact (v, sz) ->
         StringMap.set acc ~key ~data:(v,v,sz)
      | Between (lo, hi, sz) ->
         StringMap.update acc key
           ~f:(function
             | None -> (lo, hi, sz)
             | Some (lo',hi', sz) ->
                (max lo lo', min hi hi', sz)
           )
    )
                                                             
let rec trace_eval_inst ?gas:(gas=10) (cmd : cmd) (inst : instance) ~wide(* :(wide = StringMap.empty) *) (pkt_loc : Packet.located)
        : (Packet.located * (int * int * int) StringMap.t * cmd * ((int * size) list * int) StringMap.t) =
  let (pkt, loc_opt) = pkt_loc in
  if gas = 0
  then (failwith "========OUT OF EVAL GAS============\n")
  else match cmd with
       | Skip ->
          (pkt_loc, wide, cmd, StringMap.empty)
       | Assign (f, e) ->
          ((Packet.set_field_of_expr1 pkt f e, loc_opt),
           widening_assignment wide f e,
           cmd, StringMap.empty)
       | Assert t ->
          (* failwith "Asserts are deprecated" *)
          if check_test t pkt_loc then
            (pkt_loc, widening_test pkt wide t, cmd, StringMap.empty)
          else
            failwith ("AssertionFailure: " ^ string_of_test t ^ "was false")
       | Assume t ->
          (* failwith "Raw assumes are deprecated" *)
          (pkt_loc, widening_test pkt wide t, cmd, StringMap.empty)
       | Seq (firstdo, thendo) ->
          let pkt_loc', wide, cmd', trace' = trace_eval_inst ~gas ~wide firstdo inst pkt_loc in
          let pkt_loc'', wide'', cmd'', trace'' = trace_eval_inst ~gas ~wide thendo inst pkt_loc' in
          (pkt_loc''
          , wide''
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
          trace_eval_inst ~gas ~wide a inst pkt_loc

       | Apply (name, keys, actions, default) ->
          let action_to_execute wide (rows : row list ) =
            List.fold rows ~init:(True,None,None)
              ~f:(fun rst (matches, data, action) ->
                match rst  with
                | ((*missed*) _, _, None) -> 
                   let cond = List.fold2_exn keys matches ~init:True ~f:(fun acc k m ->
                                  acc %&% encode_match k m
                                ) in
                   if check_test cond pkt_loc
                   then
                     ((*missed %&%*) cond, widening_match pkt wide (List.zip_exn keys matches) |> Some, Some (data, action))
                   else
                     let _ = Printf.printf "%s not in  %s" (Packet.string__packet (fst pkt_loc)) (string_of_test cond)   in
                     ((*missed %&% !%(cond)*) True, Some wide, None)
                | (_, _, _) -> rst
              )
          in
          begin match StringMap.find inst name with
          | None -> trace_eval_inst ~gas ~wide default inst pkt_loc 
          | Some rules ->
             begin
               Printf.printf "Widening a match! %s\n" (Packet.test_of_wide wide |> string_of_test);
               match action_to_execute wide rules with
               | (cond, Some wide, Some (data, aid)) ->
                  Printf.printf "HIT A RULE\n%!";
                  let pkt', wide', cmd', trace = trace_eval_inst ~wide (List.nth_exn actions aid |> bind_action_data data) inst pkt_loc in
                  (pkt', wide', Assert cond %:% cmd', StringMap.set ~key:name ~data:(data, aid) trace)
               | (cond, _, _) ->
                  Printf.printf "Missed everything\n%!";
                  let pkt',wide', cmd', trace = trace_eval_inst ~wide default inst pkt_loc in
                  (pkt' , wide', Assert cond %:% cmd', StringMap.set ~key:name ~data:([],List.length actions) trace )
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
