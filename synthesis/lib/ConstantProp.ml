open Core
open Util

let meet m m' : 'a StringMap.t =
  StringMap.merge m m' ~f:(fun ~key:_ case ->
      match case with
      | `Both (l, r) when Stdlib.(l = r) -> Some l
      | `Both (_, _) ->
          (* Printf.printf "branches disagree on value of %s\n%!" key; *)
          None
      | `Left _ ->
          (* Printf.printf "Forgetting the Left value of %s\n%!" key; *)
          None
      | `Right _ ->
          (* Printf.printf "Forgetting the Right value of %s\n%!" key; *)
          None)

let meet_opt m_opt m : 'a StringMap.t =
  Option.value_map m_opt ~default:m ~f:(meet m)

let rec eval_const_expr e =
  let binop op (e1, e2) =
    match (eval_const_expr e1, eval_const_expr e2) with
    | Some e1', Some e2' -> Some (op e1' e2')
    | _, _ -> None
  in
  let open Expr in
  match e with
  | Value v -> Some v
  | Var _ | Hole _ -> None
  | Plus es -> binop Value.add es
  | SatPlus es -> binop Value.sat_add es
  | Times es -> binop Value.multiply es
  | Minus es -> binop Value.subtract es
  | SatMinus es -> binop Value.sat_subtract es
  | Mask es -> binop Value.mask es
  | Xor es -> binop Value.xor es
  | BOr es -> binop Value.or_ es
  | Shl es -> binop Value.shl es
  | Concat es -> binop Value.concat es
  | Cast (i, e') ->
      let open Option in
      eval_const_expr e' >>| Value.cast i
  | Slice {hi; lo; bits} ->
      let open Option in
      eval_const_expr bits >>| Value.slice hi lo

let rec propogate_expr (map : Expr.t StringMap.t) (e : Expr.t) =
  let eval_if_const e =
    let open Option in
    eval_const_expr e >>| (fun v -> Expr.Value v) |> value ~default:e
  in
  let binop ctor (e1, e2) =
    let e' = ctor (propogate_expr map e1) (propogate_expr map e2) in
    eval_if_const e'
  in
  let unop ctor e = eval_if_const @@ ctor @@ propogate_expr map e in
  let open Expr in
  match e with
  | Value _ | Hole _ -> e
  | Var (x, _) -> StringMap.find map x |> Option.value ~default:e
  | Plus es
   |Times es
   |Minus es
   |Mask es
   |Xor es
   |BOr es
   |Shl es
   |Concat es
   |SatPlus es
   |SatMinus es ->
      binop (bin_ctor e) es
  | Cast (i, e') -> unop (cast i) e'
  | Slice {hi; lo; bits} -> unop (slice hi lo) bits

let rec propogate_test (map : Expr.t StringMap.t) (t : Test.t) =
  let open Test in
  let tbinop ctor (t1, t2) =
    ctor (propogate_test map t1) (propogate_test map t2)
  in
  let ebinop ctor (e1, e2) =
    ctor (propogate_expr map e1) (propogate_expr map e2)
  in
  let unop ctor t = ctor @@ propogate_test map t in
  match t with
  | True | False -> t
  | Eq es -> ebinop eq es
  | Le es -> ebinop leq es
  | And ts -> tbinop and_ ts
  | Or ts -> tbinop or_ ts
  | Impl ts -> tbinop impl ts
  | Iff ts -> tbinop iff ts
  | Neg t -> unop neg t

let rec propogate_cmd (map : Expr.t StringMap.t) (cmd : Cmd.t) =
  let open Cmd in
  match cmd with
  | Skip -> (map, Skip)
  | Assign (f, e) ->
      let e' = propogate_expr map e in
      let map' = StringMap.set map ~key:f ~data:e' in
      (map', f %<-% e')
  | Seq (c1, c2) ->
      let map', c1' = propogate_cmd map c1 in
      let map'', c2' = propogate_cmd map' c2 in
      (map'', c1' %:% c2')
  | Assume t -> (map, assume (propogate_test map t))
  | Select (typ, cases) ->
      let map', cases', _ =
        List.fold cases ~init:(None, [], false)
          ~f:(fun (accMap, cases, _) (b, c) ->
            let b' = propogate_test map b in
            let map', c' = propogate_cmd map c in
            ( Some (meet_opt accMap map')
            , cases @ [(b', c')]
            , Test.equals b' True && Cmd.styp_equals typ Ordered ))
      in
      (Option.value_exn map', select typ cases')
  | Apply {name; keys; actions; default} ->
      let facts_acts, actions' =
        List.fold actions ~init:(None, [])
          ~f:(fun (acc_map, acc_acts) (n, data, act) ->
            let map', act' =
              propogate_cmd (strmap_remove_list map (fsts data)) act
            in
            let post_action_facts = strmap_remove_list map' (fsts data) in
            let pre_action_facts = strmap_project_list map (fsts data) in
            let facts =
              StringMap.merge pre_action_facts post_action_facts
                ~f:(fun ~key -> function
                | `Both (pre, post) when Stdlib.(pre = post) -> Some pre
                | `Both (pre, post) ->
                    Printf.sprintf
                      "got different expressions for %s pre action and post \
                       action: %s and %s"
                      key (Expr.to_string pre) (Expr.to_string post)
                    |> failwith
                | `Left pre -> Some pre
                | `Right post -> Some post)
            in
            (Some (meet_opt acc_map facts), acc_acts @ [(n, data, act')]))
      in
      let facts_def, default' = propogate_cmd map default in
      let keys' =
        List.map keys ~f:(fun key ->
            let k, sz = Key.to_sized key in
            match StringMap.find map k with
            | Some (Var (k', _)) ->
                Key.make (k', sz) (* should update with [key]'s value? *)
            | Some (Value v) -> (
              match Key.value key with
              | Some v' when not (Value.eq v v') ->
                  Printf.sprintf
                    "conflicting values for key %s in table %s : have %s \
                     inferred %s"
                    k name (Value.to_string v') (Value.to_string v)
                  |> failwith
              | _ -> Key.(set_val (make (k, sz)) v) )
            | Some e ->
                Printf.printf
                  "[INFO] could replace key %s in table %s with value %s\n%!"
                  k name (Expr.to_string e) ;
                Key.make (k, sz)
            | None -> Key.make (k, sz))
      in
      ( meet_opt facts_acts facts_def
      , Apply {name; keys= keys'; actions= actions'; default= default'} )

let propogate cmd = propogate_cmd StringMap.empty cmd |> snd

let rec fix ~equal f x_old x_new =
  if equal x_old x_new then x_new else fix ~equal f x_new (f x_new)

let propogate_fix cmd = fix propogate Skip cmd ~equal:Stdlib.( = )

(*known facts is a collection of facts we know*)
let rec infer (known_facts : Expr.t StringMap.t) (b : Test.t) :
    Expr.t StringMap.t * Test.t =
  let open Manip in
  let open Test in
  let binop mk f (e1, e2) = mk (f e1) (f e2) in
  match b with
  | True | False -> (known_facts, b)
  | Eq (Var (x, _), Value v) -> (
    match StringMap.find known_facts x with
    | None -> (StringMap.set known_facts ~key:x ~data:(Value v), b)
    | Some (Value v') when Value.eq v v' -> (known_facts, True)
    | Some (Value _) -> (known_facts, False)
    | _ -> (known_facts, substitute b known_facts) )
  | Eq es -> (known_facts, binop eq (substituteE known_facts) es)
  | Le es -> (known_facts, binop leq (substituteE known_facts) es)
  | And (t1, t2) ->
      let known_facts2, t2' = infer known_facts t2 in
      let known_facts1, t1' = infer known_facts2 t1 in
      (known_facts1, t1' %&% t2')
  | Or (t1, t2) ->
      let known_facts2, t2' = infer known_facts t2 in
      let known_facts1, t1' = infer known_facts t1 in
      (meet known_facts2 known_facts1, t1' %+% t2')
  | Impl ts -> (known_facts, binop iff (Fn.flip substitute known_facts) ts)
  | Iff ts -> (known_facts, binop iff (Fn.flip substitute known_facts) ts)
  | Neg t -> (known_facts, !%(substitute t known_facts))

let to_value_map (emap : Expr.t StringMap.t) : Value.t StringMap.t =
  StringMap.filter_map emap ~f:(fun data ->
      match data with
      | Value v -> Some v
      | _ -> None)

let to_expr_map (vmap : Value.t StringMap.t) : Expr.t StringMap.t =
  StringMap.map vmap ~f:(fun v -> Expr.Value v)

let rec passive_propogate_aux dir map cmd =
  let open Cmd in
  match cmd with
  | Skip -> (map, Skip)
  | Assign (_, _) ->
      failwith "[ERROR] Assumed in passive form but got ASSIGN"
  | Assume t ->
      let t' = Manip.substitute t map in
      let map', t'' = infer map t' in
      (map', assume t'')
  | Seq (c1, c2) ->
      let proc_fst, proc_snd =
        match dir with
        | `Fwd -> (c1, c2)
        | `Rev -> (c2, c1)
      in
      let map', proc_fst' = passive_propogate_aux dir map proc_fst in
      let map'', proc_snd' = passive_propogate_aux dir map' proc_snd in
      let cmd' =
        match dir with
        | `Fwd -> proc_fst' %:% proc_snd'
        | `Rev -> proc_snd' %:% proc_fst'
      in
      (map'', cmd')
  | Select (typ, cases) ->
      let map', cases' =
        List.fold cases ~init:(None, [])
          ~f:(fun (acc_map, acc_cases) (b, c) ->
            let map_b, b' = Manip.substitute b map |> infer map in
            let map', c' = passive_propogate_aux dir map_b c in
            let map_opt =
              let open Option in
              acc_map >>| meet map' |> value ~default:map' |> return
            in
            (map_opt, acc_cases @ [(b', c')]))
      in
      (Option.value_exn map', select typ cases')
  | Apply _ -> failwith "[Error] assumed tables are applied out"

let passive_propogate (map, cmd) =
  let map, cmd' = passive_propogate_aux `Rev map cmd in
  passive_propogate_aux `Fwd map cmd'

let passive_propogate_fix map cmd =
  fix passive_propogate (StringMap.empty, Skip) (map, cmd)
    ~equal:(fun (map, cmd) (map', cmd') ->
      StringMap.equal Stdlib.( = ) map map' && Stdlib.(cmd = cmd'))
  |> snd

let rec eval_expr_choices facts (e : Expr.t) : Value.t list option =
  let open Option in
  let map_over_product f (e1, e2) =
    let v1s =
      eval_expr_choices facts e1
      >>| List.dedup_and_sort ~compare:Stdlib.compare
    in
    let v2s =
      eval_expr_choices facts e2
      >>| List.dedup_and_sort ~compare:Stdlib.compare
    in
    liftO2 List.cartesian_product v1s v2s >>| List.map ~f:(uncurry f)
  in
  let map f e = eval_expr_choices facts e >>| List.map ~f in
  let open Expr in
  match e with
  | Value v -> Some [v]
  | Var (x, _) -> StringMap.find facts x
  | Hole _ -> failwith "cannot evaluate holes"
  | Plus es
   |SatPlus es
   |SatMinus es
   |Times es
   |Minus es
   |Mask es
   |Xor es
   |BOr es
   |Shl es
   |Concat es ->
      map_over_product (bin_eval e) es
  | Cast (_, bits) | Slice {bits; _} -> map (un_eval e) bits

let rec propogate_choices (facts : Value.t list StringMap.t) =
  let open Cmd in
  function
  | Skip -> facts
  | Assign (f, e) ->
      if false then Printf.printf "Propogating assignment \n%!" ;
      let vs = eval_expr_choices facts e in
      StringMap.change facts f ~f:(lossless_append vs)
  | Seq (c1, c2) -> propogate_choices (propogate_choices facts c1) c2
  | Select (_, cs) ->
      snds cs
      |> List.map ~f:(propogate_choices facts)
      |> List.fold ~init:StringMap.empty ~f:multimap_union
  | Assume _ -> facts
  | Apply _ -> failwith "[Unimplemented] don't want to touch apply"
