open Core
open Util

let freshen v sz i = (v ^ "$" ^ string_of_int i, sz)

let rec indexVars_expr e (sub : (int * int) StringMap.t) =
  let open Expr in
  let binop f (e1, e2) =
    let e1', sub1 = indexVars_expr e1 sub in
    let e2', sub2 = indexVars_expr e2 sub in
    ( f e1' e2'
    , StringMap.merge sub1 sub2 ~f:(fun ~key -> function
        | `Both (i1, i2) ->
            if fst i1 = fst i2 && snd i1 = snd i2 then Some i1
            else
              failwith
              @@ Printf.sprintf "collision on %s: (%d,%d) <> (%d,%d)" key
                   (fst i1) (snd i1) (fst i2) (snd i2)
        | `Left i | `Right i -> Some i) )
  in
  match e with
  | Value _ -> (e, sub)
  | Var (x, sz) -> (
    match StringMap.find sub x with
    | None -> (Var (freshen x sz 0), StringMap.set sub ~key:x ~data:(0, sz))
    | Some (i, _) -> (Var (freshen x sz i), sub) )
  | Hole (x, sz) -> (
    match StringMap.find sub x with
    | None ->
        (Hole (x, sz), sub)
        (*"couldn't find "^x^" in substitution map " |> failwith*)
    | Some (i, _) -> (Hole (freshen x sz i), sub) )
  | Cast (i, e) ->
      let e', sub' = indexVars_expr e sub in
      (cast i e', sub')
  | Slice {hi; lo; bits} ->
      let e', sub' = indexVars_expr bits sub in
      (slice hi lo e', sub')
  | Plus es
   |Minus es
   |Times es
   |Mask es
   |Xor es
   |BOr es
   |Shl es
   |Concat es
   |SatPlus es
   |SatMinus es ->
      binop (bin_ctor e) es

let rec indexVars b sub =
  let open Test in
  let binop_t op a b = op (indexVars a sub) (indexVars b sub) in
  let binop_e f (e1, e2) =
    let e1', _ = indexVars_expr e1 sub in
    let e2', _ = indexVars_expr e2 sub in
    f e1' e2'
  in
  match b with
  | True | False -> b
  | Neg b -> !%(indexVars b sub)
  | And (a, b) -> binop_t ( %&% ) a b
  | Or (a, b) -> binop_t ( %+% ) a b
  | Impl (a, b) -> binop_t ( %=>% ) a b
  | Iff (a, b) -> binop_t ( %<=>% ) a b
  | Eq es -> binop_e ( %=% ) es
  | Le es -> binop_e ( %<=% ) es

let inits fvs sub =
  StringMap.fold sub ~init:[] ~f:(fun ~key:v ~data:(_, sz) vs ->
      if List.exists fvs ~f:(fun (x, _) -> String.(x = v)) then
        freshen v sz 0 :: vs
      else vs)
  |> List.dedup_and_sort ~compare:(fun (u, _) (v, _) -> Stdlib.compare u v)

let rec apply_init_expr (e : Expr.t) =
  let open Expr in
  let unop op e = op @@ apply_init_expr e in
  let binop op (e, e') = op (apply_init_expr e) (apply_init_expr e') in
  match e with
  | Value _ | Hole _ -> e
  | Var (v, sz) -> Var (freshen v sz 0)
  | Cast (sz, e) -> unop (cast sz) e
  | Slice {hi; lo; bits} -> unop (slice hi lo) bits
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

let rec apply_init_test t =
  let open Test in
  let ebinop op (e, e') = op (apply_init_expr e) (apply_init_expr e') in
  let tbinop op (t, t') = op (apply_init_test t) (apply_init_test t') in
  let tunop op t = op @@ apply_init_test t in
  match t with
  | True | False -> t
  | Eq es -> ebinop eq es
  | Le es -> ebinop leq es
  | And ts -> tbinop and_ ts
  | Or ts -> tbinop or_ ts
  | Impl ts -> tbinop impl ts
  | Iff ts -> tbinop iff ts
  | Neg t -> tunop neg t

let finals fvs sub =
  StringMap.fold sub ~init:[] ~f:(fun ~key:v ~data:(i, sz) vs ->
      if List.exists fvs ~f:(fun (x, _) -> String.(x = v)) then
        freshen v sz i :: vs
      else vs)
  |> List.sort ~compare:(fun (u, _) (v, _) -> Stdlib.compare u v)

let apply_finals_sub_packet (pkt : Packet.t) sub : Packet.t =
  Packet.fold pkt ~init:Packet.empty ~f:(fun ~key ~data acc ->
      let key =
        match StringMap.find sub key with
        | Some (i, _) -> fst (freshen key (Value.size data) i)
        | None -> key
      in
      Packet.set_field acc key data)

let rec apply_finals_sub_expr e sub =
  let open Expr in
  let unop op e = op @@ apply_finals_sub_expr e sub in
  let binop op (e, e') =
    op (apply_finals_sub_expr e sub) (apply_finals_sub_expr e' sub)
  in
  match e with
  | Value _ | Hole _ -> e
  | Var (v, sz) -> (
    match StringMap.find sub v with
    | Some (i, _) -> Var (freshen v sz i)
    | None -> Var (v, sz) )
  | Cast (sz, e) -> unop (cast sz) e
  | Slice {hi; lo; bits} -> unop (slice hi lo) bits
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

let rec apply_finals_sub_test t sub =
  let open Test in
  let ebinop op (e, e') =
    op (apply_finals_sub_expr e sub) (apply_finals_sub_expr e' sub)
  in
  let tbinop op (t, t') =
    op (apply_finals_sub_test t sub) (apply_finals_sub_test t' sub)
  in
  let tunop op t = op @@ apply_finals_sub_test t sub in
  match t with
  | True | False -> t
  | Eq es -> ebinop eq es
  | Le es -> ebinop leq es
  | And ts -> tbinop and_ ts
  | Or ts -> tbinop or_ ts
  | Impl ts -> tbinop impl ts
  | Iff ts -> tbinop iff ts
  | Neg t -> tunop neg t

let zip_eq_exn xs ys =
  let open Test in
  List.fold2_exn xs ys ~init:True ~f:(fun acc x y ->
      acc %&% (Var x %=% Var y))

let prepend_str = Printf.sprintf "%s%s"

let rec prepend_expr pfx e =
  let open Expr in
  let binop op (e, e') = op (prepend_expr pfx e) (prepend_expr pfx e') in
  match e with
  | Value _ -> e
  | Var (v, sz) -> Var (prepend_str pfx v, sz)
  | Hole (v, sz) -> Var (prepend_str pfx v, sz)
  | Cast (i, e) -> cast i @@ prepend_expr pfx e
  | Slice {hi; lo; bits} -> slice hi lo @@ prepend_expr pfx bits
  | Plus es
   |Minus es
   |Times es
   |Mask es
   |Xor es
   |BOr es
   |Shl es
   |Concat es
   |SatPlus es
   |SatMinus es ->
      binop (bin_ctor e) es

let rec prepend_test pfx b =
  let open Test in
  match b with
  | True | False -> b
  | Neg b -> !%(prepend_test pfx b)
  | Eq (e1, e2) -> prepend_expr pfx e1 %=% prepend_expr pfx e2
  | Le (e1, e2) -> prepend_expr pfx e1 %<=% prepend_expr pfx e2
  | And (b1, b2) -> prepend_test pfx b1 %&% prepend_test pfx b2
  | Or (b1, b2) -> prepend_test pfx b1 %+% prepend_test pfx b2
  | Impl (b1, b2) -> prepend_test pfx b1 %=>% prepend_test pfx b2
  | Iff (b1, b2) -> prepend_test pfx b1 %<=>% prepend_test pfx b2

let rec prepend pfx c =
  let open Cmd in
  match c with
  | Skip -> Skip
  | Assign (f, e) -> Assign (prepend_str pfx f, prepend_expr pfx e)
  | Assume b -> prepend_test pfx b |> Assume
  | Seq (c1, c2) -> prepend pfx c1 %:% prepend pfx c2
  | Select (typ, cs) ->
      List.map cs ~f:(fun (t, c) -> (prepend_test pfx t, prepend pfx c))
      |> select typ
  | Apply t ->
      Apply
        { name= prepend_str pfx t.name
        ; keys= List.map t.keys ~f:(Key.str_map ~f:(prepend_str pfx))
        ; actions=
            List.map t.actions ~f:(fun (n, scope, act) ->
                ( n
                , List.map scope ~f:(fun (x, sz) -> (prepend_str pfx x, sz))
                , prepend pfx act ))
        ; default= prepend pfx t.default }

let rec passify_aux sub c : (int * int) StringMap.t * Cmd.t =
  let open Test in
  let open Cmd in
  match c with
  | Skip -> (sub, Skip)
  | Assume b -> (sub, Assume (indexVars b sub))
  | Assign (f, e) -> (
    match StringMap.find sub f with
    | None ->
        let sz = Expr.size e in
        ( StringMap.set sub ~key:f ~data:(1, sz)
        , Assume (Var (freshen f sz 1) %=% fst (indexVars_expr e sub)) )
    | Some (idx, sz) ->
        ( StringMap.set sub ~key:f ~data:(idx + 1, sz)
        , Assume (Var (freshen f sz (idx + 1)) %=% fst (indexVars_expr e sub))
        ) )
  | Seq (c1, c2) ->
      let sub1, c1' = passify_aux sub c1 in
      let sub2, c2' = passify_aux sub1 c2 in
      (sub2, c1' %:% c2')
  | Select (Total, _) -> failwith "Don't know what to do for if total"
  | Select (typ, ss) ->
      let sub_lst =
        List.map ss ~f:(fun (t, c) ->
            let sub', c' = passify_aux sub c in
            (sub', (indexVars t sub, c')))
      in
      let merged_subst =
        List.fold sub_lst ~init:StringMap.empty ~f:(fun acc (sub', _) ->
            StringMap.merge acc sub' ~f:(fun ~key:_ -> function
              | `Left i -> Some i
              | `Right i -> Some i
              | `Both ((i, sz), (j, _)) -> Some (max i j, sz)))
      in
      let rewriting sub =
        StringMap.fold sub ~init:Skip ~f:(fun ~key:v ~data:(idx, _) acc ->
            let merged_idx, sz = StringMap.find_exn merged_subst v in
            if merged_idx > idx then
              Assume
                (Var (freshen v sz merged_idx) %=% Var (freshen v sz idx))
              %:% acc
            else acc)
      in
      let ss' =
        List.filter_map sub_lst ~f:(fun (sub', (t', c')) ->
            let rc = rewriting sub' in
            Some (t', c' %:% rc))
      in
      (merged_subst, select typ ss')
  | Apply _ -> failwith "Cannot passify (yet) table applications"

let passify fvs c =
  let init_sub =
    List.fold fvs ~init:StringMap.empty ~f:(fun sub (v, sz) ->
        StringMap.set sub ~key:v ~data:(0, sz))
  in
  (* Printf.printf "active : \n %s \n" (string_of_cmd c); *)
  passify_aux init_sub c

let rec good_wp c =
  let open Test in
  let open Cmd in
  match c with
  | Skip -> True
  | Assume b -> b
  | Seq (c1, c2) -> good_wp c1 %&% good_wp c2
  | Select (Total, _) -> failwith "Totality eludes me"
  | Select (Partial, ss) ->
      List.fold ss ~init:False ~f:(fun cond (t, c) ->
          cond %+% (t %&% good_wp c))
  | Select (Ordered, ss) ->
      List.fold ss ~init:(False, False) ~f:(fun (cond, misses) (t, c) ->
          (cond %+% (t %&% !%misses %&% good_wp c), t %+% misses))
      |> fst
  | Assign _ ->
      failwith
        "ERROR: PROGRAM NOT IN PASSIVE FORM! Assignments should have been \
         removed"
  | Apply _ -> failwith "Tables should be applied at this stage"

let rec bad_wp c =
  let open Test in
  let open Cmd in
  match c with
  | Skip -> False
  | Assume _ -> False
  | Seq (c1, c2) -> bad_wp c1 %+% (good_wp c1 %&% bad_wp c2)
  | Select (Total, _) -> failwith "totality eludes me "
  | Select (Partial, ss) ->
      List.fold ss ~init:True ~f:(fun acc (t, c) -> acc %+% (t %&% bad_wp c))
  | Select (Ordered, ss) ->
      List.fold ss ~init:(False, False) ~f:(fun (cond, misses) (t, c) ->
          (cond %+% (t %&% !%misses %&% bad_wp c), t %+% misses))
      |> fst
  | Assign _ ->
      failwith
        "ERROR: PROGRAM NOT IN PASSIVE FORM! Assignments should have been \
         removed"
  | Apply _ -> failwith "Tables should be applied at this stage"

let good_execs fvs c =
  (* let c = CompilerOpts.optimize fvs c in *)
  let merged_sub, passive_c = passify fvs c in
  let passive_c =
    ConstantProp.passive_propogate (StringMap.empty, passive_c) |> snd
  in
  (* Printf.printf "passive : \n %s\n" (string_of_cmd passive_c); *)
  (* let vc = good_wp passive_c in *)
  (* Printf.printf "good_executions:\n %s\n%!" (string_of_test vc); *)
  (merged_sub, passive_c, good_wp passive_c, bad_wp passive_c)

let equivalent ?(neg = Test.True) (data : ProfData.t ref) eq_fvs l p =
  let open Cmd in
  let l = Assume neg %:% l in
  let p = Assume neg %:% p in
  let phys_prefix = "phys_" in
  let p' = prepend phys_prefix p in
  let st = Time.now () in
  let fvs = vars l @ vars p in
  ProfData.update_time !data.prefixing_time st ;
  let prefix_list = List.map ~f:(fun (x, sz) -> (phys_prefix ^ x, sz)) in
  let fvs_p = prefix_list fvs in
  let eq_fvs_p = prefix_list eq_fvs in
  let st = Time.now () in
  let sub_l, _, gl, _ = good_execs fvs l in
  let sub_p, _, gp, _ = good_execs fvs_p p' in
  ProfData.update_time !data.good_execs_time st ;
  let st = Time.now () in
  let lin = inits eq_fvs sub_l in
  let pin = inits eq_fvs_p sub_p in
  let lout = finals eq_fvs sub_l in
  let pout = finals eq_fvs_p sub_p in
  let in_eq = zip_eq_exn lin pin in
  let out_eq = zip_eq_exn lout pout in
  ProfData.update_time !data.ingress_egress_time st ;
  Test.(gl %&% gp %&% in_eq %=>% out_eq)

let hoare_triple_passified_relabelled assum good_n conseq =
  let open Test in
  impl assum @@ impl good_n conseq

let hoare_triple_passified sub assum good_n conseq =
  hoare_triple_passified_relabelled (apply_init_test assum) good_n
    (apply_finals_sub_test conseq sub)

let passive_hoare_triple_pkt ~fvs in_pkt cmd out_pkt =
  let sub, _, good_N, _ = good_execs fvs cmd in
  hoare_triple_passified sub
    (Packet.to_test in_pkt ~fvs)
    good_N
    (Packet.to_test out_pkt ~fvs)

let passive_hoare_triple ~fvs assum cmd conseq =
  let sub, _, good_N, _ = good_execs fvs cmd in
  hoare_triple_passified sub assum good_N conseq
