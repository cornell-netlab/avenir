open Core
open Util
open Ast


let meet m m' : 'a StringMap.t =
  StringMap.merge m m'
    ~f:(fun ~key:_ case ->
      match case with
      | `Both (l,r) when Stdlib.(l = r) -> Some l
      | `Both (_,_) ->
         (* Printf.printf "branches disagree on value of %s\n%!" key; *)
         None
      | `Left  _ ->
         (* Printf.printf "Forgetting the Left value of %s\n%!" key; *)
         None
      | `Right _ ->
         (* Printf.printf "Forgetting the Right value of %s\n%!" key; *)
         None)

let meet_opt m_opt m : 'a StringMap.t =
  Option.value_map m_opt ~default:m ~f:(meet m)


let rec eval_const_expr e =
  let binop op (e1, e2) =
    match eval_const_expr e1, eval_const_expr e2 with
    | Some e1', Some e2' -> Some (op e1' e2')
    | _,_ -> None
  in
  match e with
  | Value v -> Some (v)
  | Var _ | Hole _ -> None
  | Plus es -> binop add_values es
  | SatPlus es -> binop sat_add_values es
  | Times es -> binop multiply_values es
  | Minus es -> binop subtract_values es
  | SatMinus es -> binop sat_subtract_values es
  | Mask es -> binop mask_values es
  | Xor es -> binop xor_values es
  | BOr es -> binop or_values es
  | Shl es -> binop shl_values es
  | Concat es -> binop concat_values es
  | Cast (i, e') ->
     let open Option in
     eval_const_expr e' >>| cast_value i
  | Slice {hi;lo;bits} ->
     let open Option in
     eval_const_expr bits >>| slice_value hi lo

let rec propogate_expr (map : expr StringMap.t) (e : expr) =
  let eval_if_const e =
    let open Option in
    (eval_const_expr e >>| fun v -> Value v)
    |> value ~default:e
  in
  let binop ctor (e1,e2) =
    let e' = ctor (propogate_expr map e1) (propogate_expr map e2) in
    eval_if_const e'
  in
  let unop ctor e =
    eval_if_const @@ ctor @@ propogate_expr map e
  in
  match e with
  | Value _ | Hole _ -> e
  | Var (x,_) ->
     StringMap.find map x
     |> Option.value ~default:e
  | Plus es
  | Times es
  | Minus es
  | Mask es
  | Xor es
  | BOr es
  | Shl es
  | Concat es
  | SatPlus es
  | SatMinus es ->
     binop (ctor_for_binexpr e) es
  | Cast (i, e') ->
     unop (mkCast i) e'
  | Slice {hi;lo;bits} ->
     unop (mkSlice hi lo) bits

let rec propogate_test (map : expr StringMap.t) (t : test) =
  let tbinop ctor (t1,t2) =
    ctor (propogate_test map t1) (propogate_test map t2)
  in
  let ebinop ctor (e1,e2) = ctor (propogate_expr map e1) (propogate_expr map e2) in
  let unop ctor t = ctor @@ propogate_test map t in
  match t with
  | True | False -> t
  | Eq es -> ebinop mkEq es
  | Le es -> ebinop mkLe es
  | And ts -> tbinop mkAnd ts
  | Or ts -> tbinop mkOr ts
  | Impl ts -> tbinop mkImplies ts
  | Iff ts -> tbinop mkIff ts
  | Neg t -> unop mkNeg t

let rec propogate_cmd (map : expr StringMap.t) (cmd : cmd) =
  match cmd with
  | Skip -> map, Skip
  | Assign (f,e) ->
     let e' = propogate_expr map e in
     let map' = StringMap.set map ~key:f ~data:e' in
     map', f %<-% e'
  | Seq (c1,c2) ->
     let map',c1' = propogate_cmd map c1 in
     let map'', c2' = propogate_cmd map' c2 in
     map'', c1' %:% c2'
  | Assume t -> map, mkAssume (propogate_test map t)
  | Select (typ, cases) ->
     let map', cases',_ =
       List.fold cases ~init:(None, [], false)
         ~f:(fun (accMap, cases, _) (b,c) ->
             let b' = propogate_test map b in
             let map',c' = propogate_cmd map c in
             Some(meet_opt accMap map'), cases @ [b', c'], (b' = True && typ = Ordered)
         )
     in
     Option.value_exn map', mkSelect typ cases'

  | Apply {name;keys;actions;default} ->
     let facts_acts, actions' =
       List.fold actions ~init:(None,[])
         ~f:(fun (acc_map,acc_acts) (data, act) ->
           let map', act' = propogate_cmd (strmap_remove_list map (fsts data)) act in
           let post_action_facts = strmap_remove_list map' (fsts data) in
           let pre_action_facts = strmap_project_list map (fsts data) in
           let facts =
             StringMap.merge pre_action_facts post_action_facts
               ~f:(fun ~key -> function
                 | `Both(pre,post) when Stdlib.(pre = post) -> Some pre
                 | `Both(pre,post) ->
                    Printf.sprintf "got different expressions for %s pre action and post action: %s and %s"
                      key
                      (string_of_expr pre)
                      (string_of_expr post)
                    |> failwith
                 | `Left pre -> Some pre
                 | `Right post -> Some post
               )
           in
           (Some (meet_opt acc_map facts),acc_acts@[(data, act')])
         )
     in
     let facts_def, default' = propogate_cmd map default in
     let keys' =
       List.map keys
         ~f:(fun (k,sz,v_opt) ->
           match StringMap.find map k with
           | Some (Var(k',_)) -> (k',sz, None)
           | Some (Value v) ->
              begin match v_opt with
              | Some v' when not(veq v v') ->
                 Printf.sprintf "conflicting values for key %s in table %s : have %s inferred %s"
                   k name (string_of_value v') (string_of_value v)
                 |> failwith
              | _ ->
                 (k,sz, Some v)
              end
           | Some (e) ->
              Printf.printf "[INFO] could replace key %s in table %s with value %s\n%!" (k) (name) (string_of_expr e);
              (k,sz, None)
           | None -> (k,sz, None)
         )
     in
     (meet_opt facts_acts facts_def
     , Apply {name;
              keys = keys';
              actions = actions';
              default = default'})


let propogate cmd = propogate_cmd StringMap.empty cmd |> snd

let rec fix ~equal f x_old x_new =
  if equal x_old x_new
  then x_new
  else fix ~equal f x_new (f x_new)


let propogate_fix cmd =
  fix propogate Skip cmd ~equal:Stdlib.(=)



(*known facts is a collection of facts we know*)
let rec infer (known_facts : expr StringMap.t) (b : test) : (expr StringMap.t * test) =
  let open Manip in
  let binop mk f (e1,e2) = mk (f e1) (f e2) in
  match b with
  | True | False -> (known_facts, b)
  | Eq (Var (x,_),Value v) ->
     begin match StringMap.find known_facts x with
     | None ->
        (StringMap.set known_facts ~key:x ~data:(Value v), b)
     | Some (Value v') when veq v v' -> (known_facts, True)
     | Some (Value _) -> (known_facts, False)
     | _ -> (known_facts, substitute b known_facts)
     end
  | Eq es -> (known_facts, binop mkEq (substituteE known_facts) es)
  | Le es -> (known_facts, binop mkLe (substituteE known_facts) es)
  | And (t1,t2) ->
     let known_facts2, t2' = infer known_facts t2 in
     let known_facts1, t1' = infer known_facts2 t1 in
     known_facts1, t1' %&% t2'
  | Or (t1,t2) ->
     let known_facts2, t2' = infer known_facts t2 in
     let known_facts1, t1' = infer known_facts t1 in
     meet known_facts2 known_facts1, t1' %+% t2'

  | Impl ts -> (known_facts, binop mkIff (Fn.flip substitute known_facts) ts)
  | Iff ts -> (known_facts, binop mkIff (Fn.flip substitute  known_facts) ts)

  | Neg t -> (known_facts, !%(substitute t known_facts))

let to_value_map (emap : expr StringMap.t) : value StringMap.t =
  StringMap.filter_map emap ~f:(fun data ->
      match data with
      | Value v -> Some v
      | _ -> None)

let to_expr_map (vmap : value StringMap.t) : expr StringMap.t =
  StringMap.map vmap ~f:(fun v -> Value v)

let rec passive_propogate_aux dir map cmd =
  match cmd with
  | Skip -> map, Skip
  | Assign (_,_) -> failwith "[ERROR] Assumed in passive form but got ASSIGN"
  | Assume t ->
     let t' = Manip.substitute t map in
     let map', t'' = infer map t' in
     map', mkAssume t''
  | Seq(c1,c2) ->
     let proc_fst, proc_snd =
       match dir with
       | `Fwd -> (c1,c2)
       | `Rev -> (c2,c1)
     in
     let map', proc_fst'  = passive_propogate_aux dir map  proc_fst in
     let map'', proc_snd' = passive_propogate_aux dir map' proc_snd in
     let cmd' = match dir with
       | `Fwd -> proc_fst' %:% proc_snd'
       | `Rev -> proc_snd' %:% proc_fst'
     in
     map'', cmd'
  | Select (typ,cases) ->
     let map', cases' =
       List.fold cases ~init:(None,[])
         ~f:(fun (acc_map,acc_cases) (b,c) ->
           let map_b, b' = Manip.substitute b map |> infer map in
           let map',c' = passive_propogate_aux dir map_b c in
           let map_opt =
             let open Option in
             (acc_map >>| meet map') |> value ~default:map' |> return
           in
           map_opt, acc_cases @ [b',c']
         )
     in
     Option.value_exn map', mkSelect typ cases'
  | Apply _ -> failwith "[Error] assumed tables are applied out"

let passive_propogate (map, cmd) =
  let (map, cmd') = passive_propogate_aux `Rev map cmd in
  passive_propogate_aux `Fwd map cmd'

let passive_propogate_fix map cmd =
  fix (passive_propogate) (StringMap.empty,Skip) (map,cmd)
    ~equal:(fun (map,cmd) (map',cmd') ->
      StringMap.equal (Stdlib.(=)) map map'
      && Stdlib.(cmd = cmd'))
  |> snd
