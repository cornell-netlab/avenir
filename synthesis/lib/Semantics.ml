open Core
open Util

let rec eval_expr (pkt : Packet.t) (e : Expr.t) : Value.t option =
  let open Option in
  let binop op e e' =
    let f = eval_expr pkt in
    oLift2 op (f e) (f e')
  in
  match e with
  | Value v -> Some v
  | Var (v, _) -> Packet.get_val_opt pkt v
  | Hole (h, _) -> Packet.get_val_opt pkt h
  | Cast (i, e) -> eval_expr pkt e >>| Value.cast i
  | Plus (e1, e2) -> binop Value.add e1 e2
  | SatPlus (e1, e2) -> binop Value.sat_add e1 e2
  | Times (e1, e2) -> binop Value.multiply e1 e2
  | Minus (e1, e2) -> binop Value.subtract e1 e2
  | SatMinus (e1, e2) -> binop Value.sat_subtract e1 e2
  | Mask (e1, e2) -> binop Value.mask e1 e2
  | Xor (e1, e2) -> binop Value.xor e1 e2
  | BOr (e1, e2) -> binop Value.or_ e1 e2
  | Shl (e1, e2) -> binop Value.shl e1 e2
  | Concat (e1, e2) -> binop Value.concat e1 e2
  | Slice {hi; lo; bits} -> eval_expr pkt bits >>| Value.slice hi lo

let rec check_test (cond : Test.t) (pkt : Packet.t) : bool option =
  let binopt op a b = oLift2 op (check_test a pkt) (check_test b pkt) in
  let binope op e e' = oLift2 op (eval_expr pkt e) (eval_expr pkt e') in
  let open Test in
  match cond with
  | True -> Some true
  | False -> Some false
  | Neg cond -> Option.(check_test cond pkt >>| not)
  | And (a, b) -> binopt ( && ) a b
  | Or (a, b) -> binopt ( || ) a b
  | Impl (a, b) -> check_test (!%a %+% b) pkt
  | Iff (a, b) -> check_test (!%a %+% b %&% (!%b %+% a)) pkt
  | Eq (e, e') -> binope Value.eq e e'
  | Le (e, e') -> binope Value.leq e e'

let ifte_test cond pkt_loc tru fls =
  match check_test cond pkt_loc with
  | None ->
      Printf.printf "[UseBeforeDefError] in test %s" (Test.to_string cond) ;
      failwith ""
  | Some t when t -> tru ()
  | _ -> fls ()

let rec find_match ?(idx = 0) pkt_loc ss ~default =
  match ss with
  | [] -> default ()
  | (cond, action) :: rest ->
      ifte_test cond pkt_loc
        (fun _ -> (cond, action, idx))
        (fun _ -> find_match ~idx:(idx + 1) pkt_loc rest ~default)

let action_to_execute pkt wide (rows : Row.t list) =
  let open Test in
  List.fold rows ~init:(True, None, None)
    ~f:(fun rst (matches, data, action) ->
      match rst with
      | missed, _, None ->
          let cond = Match.list_to_test matches in
          ifte_test cond pkt
            (fun _ -> (missed %&% cond, None, Some (data, action)))
            (fun _ -> (missed %&% !%cond, Some wide, None))
      | _, _, _ -> rst)

let rec trace_eval_inst ?(gas = 10) (cmd : Cmd.t) (inst : Instance.t) ~wide
    (* :(wide = StringMap.empty) *) (pkt : Packet.t) :
    Packet.t
    * (Value.t * Value.t) StringMap.t
    * Cmd.t
    * (Value.t list * int) StringMap.t =
  if gas = 0 then failwith "========OUT OF EVAL GAS============\n"
  else
    match cmd with
    | Skip -> (pkt, wide, cmd, StringMap.empty)
    | Assign (f, e) ->
        let pkt =
          match Packet.set_field_of_expr_opt pkt f e with
          | Some pkt -> pkt
          | None ->
              Printf.printf "[UseBeforeDefError] on command %s "
                (Cmd.to_string cmd) ;
              failwith ""
        in
        (pkt, StringMap.empty, cmd, StringMap.empty)
    | Assume _ -> (pkt, StringMap.empty, cmd, StringMap.empty)
    | Seq (firstdo, thendo) ->
        let pkt', wide', cmd', trace' =
          trace_eval_inst ~gas ~wide firstdo inst pkt
        in
        let pkt'', wide'', cmd'', trace'' =
          trace_eval_inst ~gas ~wide:wide' thendo inst pkt'
        in
        ( pkt''
        , wide''
        , Cmd.seq cmd' cmd''
        , StringMap.merge trace' trace'' ~f:(fun ~key:_ -> function
            | `Left v -> Some v
            | `Right v -> Some v
            | `Both (v, _) -> Some v) )
    | Select (styp, selects) ->
        let default _ =
          match styp with
          | Total ->
              failwith "SelectionError: Could not find match in [if total]"
          | Partial | Ordered ->
              Printf.printf
                "[EVAL (%d)] Skipping selection, no match for %s\n" gas
                (Packet.to_string pkt) ;
              (Test.True, Cmd.Skip, List.length selects)
        in
        let test, a, _ = find_match pkt selects ~default in
        let p, w, cmd, trace = trace_eval_inst ~gas ~wide a inst pkt in
        (p, w, Cmd.(assume test %:% cmd), trace)
    | Apply t -> (
        let rules = Instance.get_rows inst t.name in
        match action_to_execute pkt wide rules with
        | cond, Some wide, Some (data, aid) ->
            (* Printf.printf "HIT A RULE\n%!"; *)
            let pkt', wide', cmd', trace =
              trace_eval_inst ~wide
                (List.nth_exn t.actions aid |> Manip.bind_action_data data)
                inst pkt
            in
            ( pkt'
            , wide'
            , Cmd.(assume cond %:% cmd')
            , StringMap.set ~key:t.name ~data:(data, aid) trace )
        | cond, _, _ ->
            (* Printf.printf "Missed everything\n%!"; *)
            let pkt', wide', cmd', trace =
              trace_eval_inst ~wide t.default inst pkt
            in
            ( pkt'
            , wide'
            , Cmd.(assume cond %:% cmd')
            , StringMap.set ~key:t.name
                ~data:([], List.length t.actions)
                trace ) )

let eval_act_trace (act : Cmd.t) (pkt : Packet.t) : Packet.t * Cmd.t =
  match trace_eval_inst act Instance.empty ~wide:StringMap.empty pkt with
  | pkt, _, trace, _ -> (pkt, trace)

(** TODO :: Can be drastically optimized via memoization and removing
    widening and tracing *)
let eval_act (act : Cmd.t) (pkt : Packet.t) : Packet.t =
  fst @@ eval_act_trace act pkt

(*TODO: Can do this faster(?) by transposing the traversal*)
let fails_on_some_example params problem =
  let eval_log = eval_act (Problem.log_gcl_program params problem) in
  let cexs = Problem.cexs problem in
  let fvs = Some (Problem.fvs problem) in
  List.find cexs ~f:(fun (inpkt, outpkt) ->
      not (Packet.equal ~fvs outpkt (eval_log inpkt)))
