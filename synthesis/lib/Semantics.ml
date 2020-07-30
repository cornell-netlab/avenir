open Core
open Ast
open Util
open Manip
open Tables

let rec eval_expr (pkt_loc : Packet.located) ( e : expr ) : value =
  let binop op e e' =
    let f = eval_expr pkt_loc in
    let v = f e in
    let v' = f e' in
    (* if size_of_value v <> size_of_expr e then
     *   failwith
     *   @@ Printf.sprintf "%s ====> %s, with different sizes!!!!" (sexp_string_of_expr e) (sexp_string_of_value v)
     * else *)
    op v v'
  in
  match e with
  | Value v -> v
  | Var (v,_) -> Packet.get_val (fst pkt_loc) v
  | Hole (h,_) -> Packet.get_val (fst pkt_loc) h
  | Cast (i,e) -> cast_value i @@ eval_expr pkt_loc e
  | Plus  (e1, e2) -> binop add_values e1 e2
  | Times (e1, e2) -> binop multiply_values e1 e2
  | Minus (e1, e2) -> binop subtract_values e1 e2
  | Mask (e1, e2) -> binop mask_values e1 e2
  | Xor (e1,e2) -> binop xor_values e1 e2
  | BOr (e1,e2) -> binop or_values e1 e2
  | Shl (e1,e2) -> binop shl_values e1 e2
  | Slice {hi;lo;bits} -> slice_value hi lo @@ eval_expr pkt_loc bits

let rec check_test (cond : test) (pkt_loc : Packet.located) : bool =
  let binopt op a b = op (check_test a pkt_loc) (check_test b pkt_loc) in
  let binope op e e' = op (eval_expr pkt_loc e) (eval_expr pkt_loc e') in
  match cond with
  | True -> true
  | False -> false
  | Neg (cond) -> not (check_test cond pkt_loc)
  | And (a, b) -> binopt (&&) a b
  | Or (a, b) -> binopt (||) a b
  | Impl(a, b) -> check_test (!%(a) %+% b) pkt_loc
  | Iff (a, b) -> check_test ((!%(a) %+% b) %&% (!%b %+% a)) pkt_loc
  | Eq (e,e') -> binope (veq) e e'
  | Le (e,e') -> binope (vleq) e e'


let rec find_match ?idx:(idx = 0) pkt_loc ss ~default:default =
  match ss with
  | [] -> default ()
  | (cond, action) :: rest ->
     if check_test cond pkt_loc then
       (cond, action, idx)
     else
       (find_match ~idx:(idx+1) pkt_loc rest ~default)

(* let rec wide_eval wide (e : expr) = (mkInt(0,-1),mkInt(0,-1)) *)
  (* match e with
   * | Value(x) -> (x,x)
   * | Var(x, sz) ->
   *    begin match StringMap.find wide x with
   *    | None -> failwith ("USE BEFORE DEF " ^ x)
   *    | Some (lo,hi) -> (lo,hi)
   *    end
   * | Hole _ -> failwith "dont know how to eval holes"
   * | Plus(x,y) -> let (lox,hix) = wide_eval wide x in
   *                let (loy, hiy) = wide_eval wide y in
   *                (add_values lox loy, add_values hix hiy)
   * | Times(x,y) -> let (lox, hix) = wide_eval wide x in
   *                 let (loy, hiy) = wide_eval wide y in
   *                 (multiply_values lox loy, multiply_values hix hiy)
   * | Minus(x,y) -> let (lox, hix) = wide_eval wide x in
   *                 let (loy, hiy) = wide_eval wide y in
   *                 (subtract_values lox hiy, subtract_values hix loy)
   * | Mask (x,y) -> failwith "Don't know how to widely evaluate a mask" *)

(*
let widening_assignment (wide : (value*value) StringMap.t) f e : (value * value) StringMap.t =
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
  | Eq(Var(v,sz), e) | Eq(e,Var(v,sz)) ->
     if check_test t (pkt,None)
     then
       let (lo,hi) = wide_eval wide e in
       Printf.printf "adding %s = [%s,%s]" v (string_of_value lo) (string_of_value hi);
       StringMap.set wide ~key:v ~data:(lo,hi)
     else let vlu = StringMap.find_exn pkt v in
          StringMap.set wide v (vlu, vlu)
  | Le(Var(v,sz), e) | Le(e,Var(v,sz)) ->
     if check_test t (pkt,None)
     then
       let (lo, hi) = wide_eval wide e in
       Printf.printf "adding %s <= [%s,%s]" v (string_of_value lo) (string_of_value hi);
       StringMap.update wide v ~f:(function
           | None -> (lo, hi)
           | Some (lo', hi')
             ->  (Stdlib.min lo lo', Stdlib.min hi hi'))
     else let vlu = StringMap.find_exn pkt v in
          StringMap.set wide v (vlu, vlu)
  | _ -> failwith "dont know how to handle that kind of test"

let widening_match pkt wide matches =
  (* Printf.printf "WIDENING A MATCH\n"; *)
  List.fold matches
    ~init:wide
    ~f:(fun acc ((key,_), m)  ->
      let open Match in
      match m with
      | Exact (v) ->
         StringMap.set acc ~key ~data:(v,v)
      | Between (lo, hi) ->
         StringMap.update acc key
           ~f:(function
             | None -> (lo, hi)
             | Some (lo',hi') ->
                (Stdlib.max lo lo', Stdlib.min hi hi')
           )
      | _ -> failwith "dont know how to widen masks"
    )
*)

let action_to_execute pkt wide keys (rows : Row.t list ) =
  List.fold rows ~init:(True,None,None)
    ~f:(fun rst (matches, data, action) ->
      match rst  with
      | (missed, _, None) ->
         let cond = Match.list_to_test keys matches in
         if check_test cond pkt
         then
           (missed %&% cond, None, Some (data, action))
         else
           (* let _ = Printf.printf "%s not in  %s" (Packet.string__packet (fst pkt_loc)) (string_of_test cond)   in *)
           (missed %&% !%(cond), Some wide, None)
      | (_, _, _) -> rst
    )


let rec trace_eval_inst ?gas:(gas=10) (cmd : cmd) (inst : Instance.t) ~wide(* :(wide = StringMap.empty) *) (pkt_loc : Packet.located)
        : (Packet.located * (value * value) StringMap.t * cmd * (value list * int) StringMap.t) =
  let (pkt, loc_opt) = pkt_loc in
  if gas = 0
  then (failwith "========OUT OF EVAL GAS============\n")
  else match cmd with
       | Skip ->
          (pkt_loc, wide, cmd, StringMap.empty)
       | Assign (f, e) ->
          ((Packet.set_field_of_expr pkt f e, loc_opt),
           StringMap.empty,
           cmd, StringMap.empty)
       | Assert _
         (* failwith "Asserts are deprecated" *)
         (* if check_test t pkt_loc then
          *   (pkt_loc, StringMap.empty, cmd, StringMap.empty)
          * else
          *   failwith ("AssertionFailure: " ^ string_of_test t ^ "was false") *)
         | Assume _ ->
          (pkt_loc, StringMap.empty, cmd, StringMap.empty)
       | Seq (firstdo, thendo) ->
          let pkt_loc', wide', cmd', trace' = trace_eval_inst ~gas ~wide firstdo inst pkt_loc in
          let pkt_loc'', wide'', cmd'', trace'' = trace_eval_inst ~gas ~wide:wide' thendo inst pkt_loc' in
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
                 (Packet.string__packet pkt);
               (True, Skip, List.length selects)
          in
          let (_, a, _) = find_match pkt_loc selects ~default in
          trace_eval_inst ~gas ~wide a inst pkt_loc

       | Apply t ->
          begin match StringMap.find inst t.name with
          | None -> trace_eval_inst ~gas ~wide t.default inst pkt_loc
          | Some rules ->
             begin
               (* Printf.printf "Widening a match! %s\n" (Packet.test_of_wide wide |> string_of_test); *)
               match action_to_execute pkt_loc wide t.keys rules with
               | (cond, Some wide, Some (data, aid)) ->
                  (* Printf.printf "HIT A RULE\n%!"; *)
                  let pkt', wide', cmd', trace = trace_eval_inst ~wide (List.nth_exn t.actions aid |> bind_action_data data) inst pkt_loc in
                  (pkt', wide', Assert cond %:% cmd', StringMap.set ~key:t.name ~data:(data, aid) trace)
               | (cond, _, _) ->
                  (* Printf.printf "Missed everything\n%!"; *)
                  let pkt',wide', cmd', trace = trace_eval_inst ~wide t.default inst pkt_loc in
                  (pkt' , wide', Assert cond %:% cmd', StringMap.set ~key:t.name ~data:([],List.length t.actions) trace )
             end
          end
       | While ( _ , _ ) ->
          failwith "Cannot process while loops"
                   (* if check_test cond pkt_loc then
                    *   trace_eval ~gas:(gas-1) (Seq(body,cmd)) pkt_loc
                    * else
                    *   Some (pkt_loc, []) *)





let eval_act_trace (act : cmd) (pkt : Packet.t) :  (Packet.t * cmd) =
  match trace_eval_inst act Instance.empty ~wide:StringMap.empty (pkt, None) with
  | ((pkt, _), _ ,trace ,_) -> (pkt,trace)

let eval_act (act : cmd) (pkt : Packet.t) : Packet.t =
  fst @@ eval_act_trace act pkt


let eval_cmd (cmd : cmd) (inst : Instance.t) (pkt : Packet.t) : Packet.t =
  (* Printf.printf "EVALUATING WITH PACKET:\n %s\n%!" (Packet.string__packet pkt); *)
  match trace_eval_inst cmd inst ~wide:StringMap.empty (pkt, None) with
  | ((pkt, _), _ ,_ ,_) -> pkt





let rec trace_nd_hits (c : cmd) (inst : Instance.t) (pkt : Packet.t) : ((string * int) list * Packet.t) list =
  match c with
  | Skip -> [[], pkt]
  | Assume b ->
     if check_test b (pkt, None)
     then [[], pkt]
     else []
  | Assert b ->
     if check_test b (pkt, None)
     then failwith ("Assertion failure")
     else [[],pkt]
  | Assign (f,e) ->
     [[],
      eval_expr (pkt, None) e
      |> Packet.set_field pkt f]
  | Seq (c1,c2) ->
     let nd_hits1 = trace_nd_hits c1 inst pkt in
     List.fold nd_hits1 ~init:[] ~f:(fun acc (hits1, pkt') ->
         let nd_hits2 = trace_nd_hits c2 inst pkt' in
         List.map nd_hits2
           ~f:(fun (hits2,pkt2) ->
             (hits1 @ hits2), pkt2) @ acc
       )
  | Apply t ->
     begin match StringMap.find inst t.name with
     | None -> trace_nd_hits t.default inst pkt
     | Some rules ->
        List.foldi rules ~init:[] ~f:(fun i acc (ms, data, aid) ->
            let cond = Match.list_to_test t.keys ms in
            if check_test cond (pkt,None)
            then List.map (trace_nd_hits (List.nth_exn t.actions aid |> bind_action_data data) inst pkt)
                   ~f:(fun (hits, pkt') -> (t.name,i) :: hits, pkt')
                 @ acc
            else acc)
     end
  | Select(Partial, ss) ->
     List.fold ss ~init:[] ~f:(fun acc (b,c) ->
         if check_test b (pkt,None)
         then trace_nd_hits c inst pkt
              @ acc
         else acc)

  | Select(Ordered, ss) ->
     List.find_map ss ~f:(fun (b,c) ->
         if check_test b (pkt,None)
         then Some (trace_nd_hits c inst pkt)
         else None
       )
     |> Option.value ~default:[]

  | Select(Total, _ ) -> failwith "Deprecated"
  | While _ -> failwith "unsupported"

let get_nd_hits (c : cmd) (inst : Instance.t) (pkt : Packet.t) : (string * int) list =
  let open List in
  dedup_and_sort ~compare:Stdlib.compare (trace_nd_hits c inst pkt >>= fst)
