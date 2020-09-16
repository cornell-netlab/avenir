open Core
open Ast
open Util


let get_val subsMap str default =
  StringMap.find subsMap str |> Option.value ~default


let rec substituteE ?holes:(holes = false) substMap e =
  let binop op (e,e') = op (substituteE ~holes substMap e) (substituteE ~holes substMap e') in
  let subst = get_val substMap in
    match e with
    | Value _ -> e
    | Var (field,_) ->
       let replacement = subst field e in
       (* Printf.printf "substituting %s for %s;\n%!" field (string_of_expr e); *)
       replacement
    | Hole (field,_) ->
       if holes then
         let e' = subst field e in
         ((*Printf.printf "%s -> %s \n%!" field (string_of_expr e');*)
          e')
       else ((*Printf.printf "NO SUBST\n%!";*)  e)
    | Cast (i,e) -> mkCast i @@ substituteE ~holes substMap e
    | Slice {hi;lo;bits} -> mkSlice hi lo @@ substituteE ~holes substMap bits
    | Plus es | Times es | Minus es | Mask es | Xor es | BOr es | Shl es | Concat es | SatPlus es | SatMinus es
      -> binop (ctor_for_binexpr e) es

                                             
(** computes ex[xs -> vs], replacing only Vars whenever holes is false, replacing both whenever holes is true *)
let rec substitute ?holes:(holes = false) ex subsMap =
  match ex with
  | True | False -> ex
  (* Homomorphic Rules*)               
  | Neg e       -> !%(substitute ~holes e subsMap)
  | Or   (e, e') -> substitute ~holes e subsMap %+% substitute ~holes e' subsMap
  | And  (e, e') -> substitute ~holes e subsMap %&% substitute ~holes e' subsMap
  | Impl (e, e') -> substitute ~holes e subsMap %=>% substitute ~holes e' subsMap
  | Iff  (e, e') -> substitute ~holes e subsMap %<=>% substitute ~holes e' subsMap
  (* Do the work *)
  | Eq (e,e') ->  substituteE ~holes subsMap e %=% substituteE ~holes subsMap e'
  | Le (e,e') ->  substituteE ~holes subsMap e %<=% substituteE ~holes subsMap e'

let substV ?holes:(holes = false) ex substMap =
  StringMap.fold substMap ~init:StringMap.empty ~f:(fun ~key ~data acc ->
      StringMap.set acc ~key ~data:(Value data))
  |> substitute ~holes ex

let rec substitute_cmd ?holes:(holes = false) cmd subst =
  match cmd with
  | Skip -> Skip
  | Assume t -> substitute ~holes t subst |> mkAssume
  | Assign (f,e) ->
     begin match StringMap.find subst f with
     | Some e' -> failwith @@ Printf.sprintf "Tried to substitute %s for %s but it occurs on the right hand side of the assignment %s" f (string_of_expr e') (string_of_cmd cmd)
     | None ->
        f %<-% substituteE ~holes subst e
     end
  | Seq (c1, c2) ->
     substitute_cmd ~holes c1 subst %:% substitute_cmd ~holes c2 subst
  | Select (typ, cs) ->
     List.map cs ~f:(fun (b,c) ->
         substitute ~holes b subst, substitute_cmd ~holes c subst)
     |> mkSelect typ
  | Apply _ -> failwith "[Manip.substitute_cmd] Don't know how to substitute into table"

let rec exact_only t =
  match t with
  | Le _ -> false
  | True | False | Eq _ -> true
  | Neg(a) -> exact_only a
  | And(a,b) | Or(a,b) | Impl(a,b) | Iff(a,b)
    -> exact_only a && exact_only b

let regularize negs cond misses =
  match negs with
  | `NoNegs when cond = True -> cond %&% !%(misses)
  | `NoNegs ->  cond
  | `Negs -> cond %&% !%misses

  (* if exact_only cond
   * then if cond = True
   *      then !%misses
   *      else cond &
   * else failwith "Don't know how to regularize anything but equivalences"  *)
         
                
(* computes weakest pre-condition of condition phi w.r.t command c *)
let rec wp negs c phi =
  (* let guarded_wp (cond, act) = cond %=>% wp ~no_negations act phi in *)
  match c with
  | Skip -> phi
  | Seq (firstdo, thendo) ->
    wp negs firstdo @@ wp negs thendo phi
  | Assign (field, value) ->
     let phi' = substitute phi @@ StringMap.singleton field value in
     (* Printf.printf "replacing %s with %s in %s \n to get %s \n" field (string_of_expr value) (string_of_test phi) (string_of_test phi'); *)
     phi'
  | Assume t -> t %=>% phi
              
  (* requires at least one guard to be true *)
  | Select (Total, []) -> True
  | Select (Total, _) -> failwith "total is unsupported
"
  (* doesn't require at any guard to be true *)
  | Select (Partial, []) -> True
  | Select (Partial, cmds) ->
     let phi' = List.fold cmds ~init:False ~f:(fun wp_so_far (cond, act) ->
         let act_wp = wp negs act phi in
         (* Printf.printf "Combining guard: %s  action: %s and accumulation %s\n%!==%s\n%!"
          *   (string_of_test cond) (string_of_test act_wp) (string_of_test wp_so_far)
          *   ((cond %&% act_wp) %+% wp_so_far |> string_of_test); *)
         (cond %&% act_wp) %+% wp_so_far
                  ) in
     (* Printf.printf "{%s}\n %s\n {%s}\n%!" (string_of_test phi') (string_of_cmd c) (string_of_test phi); *)
     phi'

  (* negates the previous conditions *)
  | Select (Ordered, cmds) ->
     let phi' = List.fold cmds ~init:(True, False) ~f:(fun (wp_so_far, misses) (cond, act) ->
         let guard = regularize negs cond misses in
         let act_wp = wp negs act phi in
         (* Printf.printf "Combining guard: %s  action: %s and accumulation %s\n%!==%s\n%!"
          *   (string_of_test guard) (string_of_test act_wp) (string_of_test wp_so_far)
          *   ((guard %=>% act_wp) %&% wp_so_far |> string_of_test); *)
        ((guard %=>% act_wp) %&% wp_so_far
        , cond %+% misses )
      )
                |> fst in
     (* Printf.printf "{%s}\n %s\n {%s}\n%!" (string_of_test phi') (string_of_cmd c) (string_of_test phi); *)
     phi'


  | Apply _
    -> failwith "wp of apply is no good"
  (* concatMap acts ~f:(fun (scope, a) ->
       *      wp ~no_negations (holify (List.map scope ~f:fst) a) phi) ~c:(mkAnd) ~init:(Some True)
       * %&% wp ~no_negations dflt phi *)

let freshen v sz i = (v ^ "$" ^ string_of_int i, sz)

let rec indexVars_expr e (sub : ((int * int) StringMap.t)) =
  let binop f (e1,e2) =
    let (e1', sub1) = indexVars_expr e1 sub in
    let (e2', sub2) = indexVars_expr e2 sub in
    f e1' e2', StringMap.merge sub1 sub2
                 ~f:(fun ~key -> function
                   | `Both (i1, i2) -> if i1 = i2
                                       then Some i1
                                       else failwith @@ Printf.sprintf "collision on %s: (%d,%d) <> (%d,%d)"
                                                          key (fst i1) (snd i1) (fst i2) (snd i2)
                   | `Left i | `Right i -> Some i
                 )
  in
  match e with
  | Value _ -> (e, sub)
  | Var (x,sz) ->
     begin match StringMap.find sub x with
     | None  -> (Var (freshen x sz 0), StringMap.set sub ~key:x ~data:(0,sz))
     | Some (i,_) ->  (Var (freshen x sz i), sub)
     end
  | Hole (x, sz) ->
     begin match StringMap.find sub x with
     | None  -> (Hole (x,sz), sub) (*"couldn't find "^x^" in substitution map " |> failwith*)
     | Some (i,_) ->  (Hole (freshen x sz i), sub)
     end
  | Cast (i,e) ->
     let e', sub' = indexVars_expr e sub in
     mkCast i e', sub'
  | Slice {hi;lo;bits} ->
     let e', sub' = indexVars_expr bits sub in
     mkSlice hi lo e', sub'
  | Plus es | Minus es | Times es | Mask es | Xor es | BOr es | Shl es | Concat es | SatPlus es | SatMinus es
    -> binop (ctor_for_binexpr e) es

let rec indexVars b sub =
  let binop_t op a b = op (indexVars a sub) (indexVars b sub) in
  let binop_e f (e1,e2) =
    let (e1',_) = indexVars_expr e1 sub in
    let (e2',_) = indexVars_expr e2 sub in
    f e1' e2'
  in
  match b with
  | True | False -> b
  | Neg b -> !%(indexVars b sub)
  | And  (a,b) -> binop_t (%&%) a b
  | Or   (a,b) -> binop_t (%+%) a b
  | Impl (a,b) -> binop_t (%=>%) a b
  | Iff  (a,b) -> binop_t (%<=>%) a b
  | Eq es -> binop_e (%=%) es
  | Le es -> binop_e (%<=%) es




let rec passify_aux sub c : ((int * int) StringMap.t * cmd) =
  match c with
  | Skip -> (sub, Skip)
  | Assume b ->
     (sub, Assume (indexVars b sub))
  | Assign (f,e) ->
     begin match StringMap.find sub f with
     | None ->
        let sz = size_of_expr e in
        (StringMap.set sub ~key:f ~data:(1, sz)
        , Assume (Var (freshen f sz 1) %=% fst(indexVars_expr e sub)))
     | Some (idx, sz) ->
        (StringMap.set sub ~key:f ~data:(idx + 1,sz)
        , Assume (Var (freshen f sz (idx + 1)) %=% fst(indexVars_expr e sub)))
     end
  | Seq (c1, c2) ->
     let (sub1, c1') = passify_aux sub  c1 in
     let (sub2, c2') = passify_aux sub1 c2 in
     (sub2, c1' %:% c2')
  | Select (Total, _) -> failwith "Don't know what to do for if total"
  | Select (typ, ss) ->
     let sub_lst = List.map ss ~f:(fun (t,c) ->
                       let sub', c' = passify_aux sub c in
                       (sub', (indexVars t sub, c'))) in
     let merged_subst =
       List.fold sub_lst ~init:StringMap.empty
         ~f:(fun acc (sub', _) ->
           StringMap.merge acc sub'
             ~f:(fun ~key:_ ->
               function
               | `Left i -> Some i
               | `Right i -> Some i
               | `Both ((i,sz),(j,_)) -> Some (max i j, sz)))
     in
     let rewriting sub =
       StringMap.fold sub ~init:Skip
         ~f:(fun ~key:v ~data:(idx,_) acc ->
           let merged_idx,sz = StringMap.find_exn merged_subst v in
           if merged_idx > idx then
             Assume (Var(freshen v sz merged_idx)
                     %=% Var(freshen v sz idx))
             %:% acc
           else acc
         )
     in
     let ss' =
       List.filter_map sub_lst ~f:(fun (sub', (t', c')) ->
           let rc = rewriting sub' in
           Some (t', c' %:% rc)
         )
     in
     (merged_subst, mkSelect typ ss')

  | Apply _ ->
     failwith ("Cannot passify (yet) table applications "^(string_of_cmd c ~depth:0))

let passify fvs c =
  let init_sub =
    List.fold fvs ~init:StringMap.empty ~f:(fun sub (v,sz) ->
        StringMap.set sub ~key:v ~data:(0,sz)
      )
  in
  (* Printf.printf "active : \n %s \n" (string_of_cmd c); *)
  passify_aux init_sub c


let rec good_wp c =
  match c with
  | Skip -> True
  | Assume b -> b
  | Seq (c1,c2) -> good_wp c1 %&% good_wp c2
  | Select(Total, _) -> failwith "Totality eludes me"
  | Select(Partial, ss) ->
     List.fold ss ~init:False
       ~f:(fun cond (t,c) -> cond %+% (t %&% good_wp (c)))
  | Select(Ordered,  ss) ->
     List.fold ss ~init:(False,False)
       ~f:(fun (cond, misses) (t,c) ->
         (cond %+% (
            t %&% !%(misses) %&% good_wp c
          )
         , t %+% misses))
     |> fst
  | Assign _ -> failwith "ERROR: PROGRAM NOT IN PASSIVE FORM! Assignments should have been removed"
  | Apply _ -> failwith "Tables should be applied at this stage"

let rec bad_wp c =
  match c with
  | Skip -> False
  | Assume _ -> False
  | Seq(c1,c2) -> bad_wp c1 %+% (good_wp c1 %&% bad_wp c2)
  | Select (Total, _) -> failwith "totality eludes me "
  | Select (Partial, ss) ->
     List.fold ss ~init:(True)
       ~f:(fun acc (t,c) -> acc %+% (t %&% bad_wp (c)))
  | Select(Ordered,  ss) ->
     List.fold ss ~init:(False,False)
       ~f:(fun (cond, misses) (t,c) ->
         (cond %+% (
            t %&% !%(misses) %&% bad_wp c
          )
         , t %+% misses))
     |> fst
  | Assign _ -> failwith "ERROR: PROGRAM NOT IN PASSIVE FORM! Assignments should have been removed"
  | Apply _ -> failwith "Tables should be applied at this stage"


let good_execs fvs c =
  let merged_sub, passive_c = passify fvs c in
  (* Printf.printf "passive : \n %s\n" (string_of_cmd passive_c); *)
  (* let vc = good_wp passive_c in *)
  (* Printf.printf "good_executions:\n %s\n%!" (string_of_test vc); *)
  (merged_sub, passive_c, good_wp passive_c, bad_wp passive_c)


let inits fvs sub =
  StringMap.fold sub ~init:[]
    ~f:(fun ~key:v ~data:(_,sz) vs ->
      if List.exists fvs ~f:(fun (x,_) -> x = v)
      then (freshen v sz 0) :: vs
      else vs)
  |> List.dedup_and_sort ~compare:(fun (u,_) (v,_) -> Stdlib.compare u v)

let rec apply_init_expr (e : expr) =
  let unop op e = op @@ apply_init_expr e in
  let binop op (e,e') = op (apply_init_expr e) (apply_init_expr e') in
  match e with
  | Value _ | Hole _ -> e
  | Var (v, sz) -> Var (freshen v sz 0)
  | Cast (sz, e) -> unop (mkCast sz) e
  | Slice {hi;lo;bits} -> unop (mkSlice hi lo) bits
  | Plus es
    | Times es
    | Minus es
    | Mask es
    | Xor es
    | BOr es
    | Shl es
    | Concat es
    | SatPlus es
    | SatMinus es -> binop (ctor_for_binexpr e) es

let rec apply_init_test t =
  let ebinop op (e,e') = op (apply_init_expr e) (apply_init_expr e') in
  let tbinop op (t,t') = op (apply_init_test t) (apply_init_test t') in
  let tunop op t = op @@ apply_init_test t in
  match t with
  | True | False -> t
  | Eq es -> ebinop mkEq es
  | Le es -> ebinop mkLe es
  | And ts -> tbinop mkAnd ts
  | Or ts -> tbinop mkAnd ts
  | Impl ts -> tbinop mkAnd ts
  | Iff ts -> tbinop mkAnd ts
  | Neg t -> tunop mkNeg t

let finals fvs sub =
  StringMap.fold sub ~init:[]
    ~f:(fun ~key:v ~data:(i,sz) vs ->
      if List.exists fvs ~f:(fun (x,_) -> x = v)
      then (freshen v sz i) :: vs
      else vs)
  |> List.sort ~compare:(fun (u,_) (v,_) -> Stdlib.compare u v)

let apply_finals_sub_packet pkt sub =
  StringMap.fold pkt ~init:StringMap.empty
    ~f:(fun ~key ~data acc ->
      let key =
        match StringMap.find sub key with
        | Some (i,_) -> fst (freshen key (size_of_value data) i)
        | None -> key
      in
      let data = Value data in
      StringMap.set acc ~key ~data
    )


let rec apply_finals_sub_expr e sub =
  let unop op e = op @@ apply_finals_sub_expr e sub in
  let binop op (e,e') = op (apply_finals_sub_expr e sub) (apply_finals_sub_expr e' sub) in
  match e with
  | Value _ | Hole _ -> e
  | Var (v, sz) ->
     begin match StringMap.find sub v with
     | Some (i,_) -> Var (freshen v sz i)
     | None -> Var (v,sz)
     end
  | Cast (sz, e) -> unop (mkCast sz) e
  | Slice {hi;lo;bits} -> unop (mkSlice hi lo) bits
  | Plus es
    | Times es
    | Minus es
    | Mask es
    | Xor es
    | BOr es
    | Shl es
    | Concat es
    | SatPlus es
    | SatMinus es -> binop (ctor_for_binexpr e) es


let rec apply_finals_sub_test t sub =
  let ebinop op (e,e') = op (apply_finals_sub_expr e sub) (apply_finals_sub_expr e' sub) in
  let tbinop op (t,t') = op (apply_finals_sub_test t sub) (apply_finals_sub_test t' sub) in
  let tunop op t = op @@ apply_finals_sub_test t sub in
  match t with
  | True | False -> t
  | Eq es -> ebinop mkEq es
  | Le es -> ebinop mkLe es
  | And ts -> tbinop mkAnd ts
  | Or ts -> tbinop mkAnd ts
  | Impl ts -> tbinop mkAnd ts
  | Iff ts -> tbinop mkAnd ts
  | Neg t -> tunop mkNeg t

let zip_eq_exn xs ys =
  List.fold2_exn xs ys ~init:True ~f:(fun acc x y -> acc %&% (Var x %=% Var y) )

let prepend_str = Printf.sprintf "%s%s"

let rec prepend_expr pfx e =
  let binop op (e,e') = op (prepend_expr pfx e) (prepend_expr pfx e') in
  match e with
  | Value _ -> e
  | Var (v,sz) -> Var(prepend_str pfx v, sz)
  | Hole(v, sz) -> Var(prepend_str pfx v, sz)
  | Cast (i,e) -> mkCast i @@ prepend_expr pfx e
  | Slice {hi;lo;bits} -> mkSlice hi lo @@ prepend_expr pfx bits
  | Plus es | Minus es | Times es | Mask es | Xor es | BOr es | Shl es | Concat es | SatPlus es | SatMinus es
    -> binop (ctor_for_binexpr e) es

let rec prepend_test pfx b =
  match b with
  | True | False -> b
  | Neg b -> !%(prepend_test pfx b)
  | Eq(e1,e2) -> prepend_expr pfx e1 %=% prepend_expr pfx e2
  | Le(e1,e2) -> prepend_expr pfx e1 %<=% prepend_expr pfx e2
  | And(b1,b2) -> prepend_test pfx b1 %&% prepend_test pfx b2
  | Or(b1,b2) -> prepend_test pfx b1 %+% prepend_test pfx b2
  | Impl(b1,b2) -> prepend_test pfx b1 %=>% prepend_test pfx b2
  | Iff (b1,b2) -> prepend_test pfx b1 %<=>% prepend_test pfx b2

let rec prepend pfx c =
  match c with
  | Skip -> Skip
  | Assign(f,e) -> Assign(prepend_str pfx f, prepend_expr pfx e)
  | Assume b -> prepend_test pfx b |> Assume
  | Seq(c1,c2) -> prepend pfx c1 %:% prepend pfx c2
  | Select(typ, cs) ->
     List.map cs ~f:(fun (t,c) -> (prepend_test pfx t, prepend pfx c))
     |> mkSelect typ
  | Apply t ->
     Apply {name = prepend_str pfx t.name;
            keys = List.map t.keys ~f:(fun (k,sz,v_opt) -> (prepend_str pfx k, sz,v_opt));
            actions = List.map t.actions ~f:(fun (n, scope, act) -> (n, List.map scope ~f:(fun (x,sz) -> (prepend_str pfx x, sz)), prepend pfx act));
            default = prepend pfx t.default}
  
                 
let equivalent ?neg:(neg = True) (data : ProfData.t ref) eq_fvs l p =
  (* Printf.printf "assuming %s\n%!" (string_of_test neg); *)
  let l = Assume neg %:% l in
  let p = Assume neg %:% p in
  let phys_prefix = "phys_" in
  let p' = prepend phys_prefix p in
  let st = Time.now () in
  let fvs =
    free_of_cmd `Hole l
    @ free_of_cmd `Var l
    @ free_of_cmd `Hole p
    @ free_of_cmd `Var p
  in
  ProfData.update_time !data.prefixing_time st;
  let prefix_list =  List.map ~f:(fun (x,sz) -> (phys_prefix ^ x, sz)) in
  let fvs_p = prefix_list fvs in
  let eq_fvs_p = prefix_list eq_fvs in

  let st = Time.now() in
  let sub_l, _, gl, _ = good_execs fvs l in
  let sub_p, _, gp, _ = good_execs fvs_p p' in
  ProfData.update_time !data.good_execs_time st;
  let st = Time.now () in
  let lin = inits eq_fvs sub_l in
  let pin = inits eq_fvs_p sub_p in
  let lout = finals eq_fvs sub_l in
  let pout = finals eq_fvs_p sub_p in
  let in_eq = zip_eq_exn lin pin in
  let out_eq = zip_eq_exn lout pout in
  ProfData.update_time !data.ingress_egress_time st;
  ((gl %&% gp %&% in_eq) %=>% out_eq)


let hoare_triple_passified_relabelled assum good_n conseq =
  mkImplies assum @@ mkImplies good_n conseq

let hoare_triple_passified sub assum good_n conseq =
  hoare_triple_passified_relabelled
    (apply_init_test assum)
    good_n
    (apply_finals_sub_test conseq sub)


let passive_hoare_triple_pkt ~fvs in_pkt cmd out_pkt =
  let sub, _, good_N, _ = good_execs fvs cmd in
  hoare_triple_passified sub
    (Packet.to_test in_pkt ~fvs)
    good_N
    (Packet.to_test out_pkt ~fvs)

let passive_hoare_triple ~fvs assum cmd conseq =
  let sub, _, good_N, _ = good_execs fvs cmd  in
  hoare_triple_passified sub
    assum
    good_N
    conseq


(** [fill_holes(|_value|_test]) replace the applies the substitution
   [subst] to the supplied cmd|value|test. It only replaces HOLES, and
   has no effect on vars *)
let rec fill_holes_expr e (subst : value StringMap.t) =
  let fill_holesS e = fill_holes_expr e subst in
  let binop op (e,e') = op (fill_holesS e) (fill_holesS e') in
  match e with
  | Value _ | Var _ -> e
  | Hole (h,sz) ->
     begin match StringMap.find subst h with
     | None -> e
     | Some v -> let sz' = size_of_value v in
                 let strv = string_of_value v in
                 (if sz <> sz' then (Printf.printf "[Warning] replacing %s#%d with %s#%d, but the sizes may be different, taking the size of %s to be ground truth" h sz strv (size_of_value v) strv));
                 Value v
     end
  | Cast (i,e) -> mkCast i @@ fill_holesS e
  | Slice {hi;lo;bits} -> mkSlice hi lo @@ fill_holesS bits
  | Plus es | Minus es | Times es | Mask es | Xor es | BOr es | Shl es | Concat es  | SatPlus es | SatMinus es
    -> binop (ctor_for_binexpr e) es


(* Fills in first-order holes according to subst  *)                  
let rec fill_holes_test t subst =
  let binop cnstr rcall left right = cnstr (rcall left subst) (rcall right subst) in
  match t with
  | True | False -> t
  | Neg a -> mkNeg (fill_holes_test a subst)
  | And  (a, b)   -> binop (%&%)   fill_holes_test  a b
  | Or   (a, b)   -> binop (%+%)   fill_holes_test  a b
  | Impl (a, b)   -> binop (%=>%)  fill_holes_test  a b
  | Iff  (a, b)   -> binop (%<=>%) fill_holes_test  a b
  | Le   (a, b)   -> binop (%<=%)  fill_holes_expr a b
  | Eq   (a, b)   -> binop (%=%)   fill_holes_expr a b

let rec fill_holes (c : cmd) subst =
  let rec_select = concatMap ~c:(@)
                     ~f:(fun (cond, act) ->
                       [(fill_holes_test cond subst, fill_holes act subst)]) in
  match c with
  | Assign (f, Hole (h,sz)) ->
     begin match StringMap.find subst h with
     | None -> c
     | Some v -> let sz' = size_of_value v in
                 let strv = string_of_value v in
                 (if sz <> sz' then (Printf.printf "[Warning] replacing %s#%d with %s#%d, but the sizes may be different, taking the size of %s to be ground truth" h sz strv (size_of_value v) strv));
                 Assign (f, Value v)
     end
  | Assign (_, _) -> c
  | Seq (firstdo, thendo) ->
     fill_holes firstdo subst %:% fill_holes thendo subst
  | Assume t ->
     fill_holes_test t subst |> Assume
  | Select (_,[]) | Skip ->
     c
  | Select (styp, cmds) ->
     rec_select cmds |> mkSelect styp
  | Apply t
    -> Apply { t with
               actions = List.map t.actions ~f:(fun (n, scope, a) -> (n, scope, fill_holes a subst));
               default = fill_holes t.default subst }

let rec wp_paths negs c phi : (cmd * test) list =
  match c with
  | Skip -> [(c, phi)]
  | Seq (c1, c2) ->
     List.map (wp_paths negs c2 phi)
       ~f:(fun (trace2, phi) ->
         List.map (wp_paths negs c1 phi)
           ~f:(fun (trace1, phi') ->
             (trace1 %:% trace2, phi')
           ) 
       ) |> List.join
       
  | Assign (field, e) ->
     let phi' = substitute phi (StringMap.singleton field e) in
     (* Printf.printf "substituting %s |-> %s\n into %s to get %s \n%!" field (string_of_expr e) (string_of_test phi') (string_of_test phi'); *)
     [(c,phi')]
  | Assume t -> [(c, t %=>% phi)]
                  
  (* requires at least one guard to be true *)
  | Select (Total, []) -> [(Skip, True)]
  | Select (Total, cmds) ->
     let open List in
     (cmds >>| fun (t,c) -> mkAssume t %:% c)
     >>= flip (wp_paths negs) phi

                  
  (* doesn't require at any guard to be true *)
  | Select (Partial, []) -> [(Skip, True)]
  | Select (Partial, cmds) ->
     List.fold cmds ~init:([]) ~f:(fun wp_so_far (cond, act) ->
          List.fold (wp_paths negs act phi) ~init:wp_so_far
             ~f:(fun acc (trace, act_wp) ->
               acc @ [(mkAssume cond %:% trace, cond %&% act_wp)]))

  (* negates the previous conditions *)
  | Select (Ordered, cmds) ->
     (* let open List in
      * (cmds >>| fun (t,c) -> Assume t %:% c)
      * >>= Fun.flip wp_paths phi *)
     List.fold cmds ~init:([], False) ~f:(fun (wp_so_far, prev_conds) (cond, act) ->
         List.fold (wp_paths negs act phi) ~init:wp_so_far
           ~f:(fun acc (trace, act_wp) ->
             let misses =
               match negs with
               | `NoNegs ->  True
               | `Negs -> !%(prev_conds)
             in
             (mkAssume (cond %&% misses) %:% trace, cond %&% misses %&% act_wp) :: acc), prev_conds %+% cond)
     |> fst

  | Apply t ->
     let open List in
     (t.default :: List.map ~f:(fun (_, sc, a) -> holify (List.map sc ~f:fst) a) t.actions) >>= flip (wp_paths negs) phi

let bind_action_data vals (_, scope, cmd) : cmd =
  let holes = fsts scope in
  let subst =
    List.fold2 holes vals
      ~init:StringMap.empty
      ~f:(fun acc x v -> StringMap.set acc ~key:x ~data:(Value v))
  in
  match subst with
  | Ok subst -> substitute_cmd cmd subst
  | Unequal_lengths ->
     Printf.sprintf
       "Incorrect number of action arguments: (%s) vs (%s) for %s"
       (List.map vals ~f:string_of_value
        |> List.reduce ~f:(Printf.sprintf "%s,%s")
        |> Option.value ~default:"")
       (List.reduce holes ~f:(Printf.sprintf "%s,%s")
        |> Option.value ~default:"")
       (string_of_cmd cmd)
     |> failwith

let rec fixup_expr (model : value StringMap.t) (e : expr)  : expr =
  (* let _ = Printf.printf "FIXUP\n%!" in *)
  let binop op (e,e') = op (fixup_expr model e) (fixup_expr model e') in
  match e with
  | Value _ | Var _ -> e
  | Hole (h,sz) -> 
     begin match StringMap.find model h with
     | None -> e
     | Some v -> let sz' = size_of_value v in
                 let strv = string_of_value v in
                 (if sz <> sz' then
                    (Printf.printf "[Warning] replacing %s#%d with %s, \
                                    but the sizes may be different, \
                                    taking the size of %s to be ground \
                                    truth\n%!" h sz strv strv));
                 Value v
     end
  | Cast (i,e) -> mkCast i @@ fixup_expr model e
  | Slice {hi;lo;bits} -> mkSlice hi lo @@ fixup_expr model bits
  | Plus es | Times es | Minus es | Mask es | Xor es | BOr es | Shl es | Concat es  | SatPlus es | SatMinus es
    -> binop (ctor_for_binexpr e) es

let rec fixup_test (model : value StringMap.t) (t : test) : test =
  let binop ctor call left right = ctor (call left) (call right) in 
  match t with
  | True | False -> t
  | Neg p -> mkNeg (fixup_test model p)
  | And  (p, q) -> binop (%&%)   (fixup_test model) p q
  | Or   (p, q) -> binop (%+%)   (fixup_test model) p q
  | Impl (p, q) -> binop (%=>%)  (fixup_test model) p q
  | Iff  (p, q) -> binop (%<=>%) (fixup_test model) p q
  | Eq (v, w) -> binop (%=%)  (fixup_expr model) v w
  | Le (v, w) -> binop (%<=%) (fixup_expr model) v w

let rec fixup_selects (model : value StringMap.t) (es : (test * cmd) list) =
  match es with
  | [] -> []
  | (cond, act)::es' ->
    let cond' = fixup_test model cond in
    let act' = fixup act model in
    (* Printf.printf "  [fixup] replacing %s with %s\n%!"
     *   (string_of_test cond) (string_of_test cond');
     * Printf.printf "  [fixup] replacing %s with %s\n%!" *)
      (* (string_of_cmd act) (string_of_cmd act'); *)
    (cond', act') :: (
      if cond = cond' && act = act' then
        fixup_selects model es'
      else
        (cond, act) :: fixup_selects model es'
    )    
and fixup (real:cmd) (model : value StringMap.t) : cmd =
  (* Printf.printf "FIXUP WITH MODEL: %s\n%!\n" (string_of_map model); *)
  match real with
  | Skip -> Skip
  | Assign (f, v) -> Assign(f, fixup_expr model v)
  | Assume t -> Assume (fixup_test model t)
  | Seq (p, q) -> Seq (fixup p model, fixup q model)
  | Select (styp,cmds) -> fixup_selects model cmds |> mkSelect styp
  | Apply t
    -> Apply {t with
              actions = List.map t.actions ~f:(fun (n, data, a) -> (n, data, fixup a model));
              default = fixup t.default model}





let rec action_reads (nm, data, cmd) =
  let to_set = StringSet.of_list %. fsts in
  let data_s = to_set data in
  match cmd with
  | Skip -> StringSet.empty
  | Assume b -> StringSet.diff
                  (to_set @@ free_of_test `Var b)
                  (data_s)
  | Assign (_,e) ->
     StringSet.diff
       (StringSet.of_list @@ fsts @@ free_of_expr `Var e)
       (data_s)
  | Seq (c1,c2) ->
     action_reads (nm, data, c1)
     |> StringSet.union @@ action_reads (nm, data,c2)
  | Select(_,cs) ->
     concatMap cs ~c:StringSet.union
       ~f:(fun (b,c) ->
         to_set (free_of_test `Var b)
         |> StringSet.union @@ action_reads (nm,data,c)
       )
  | Apply t ->
     Printf.sprintf "table %s not appeard in action %s" t.name nm
     |> failwith


let is_zero = function
  | Value (Int(i,_)) -> Bigint.(i = zero)
  | _ -> false


let rec is_a_sequence_of_zero_assignments = function
  | Skip | Assume _ -> true
  | Assign (_, e) -> is_zero e
  | Seq(c1,c2) -> is_a_sequence_of_zero_assignments c1
                  && is_a_sequence_of_zero_assignments c2
  | _ -> false
