open Core
open Ast
open Util



(* computes the product of two lists of disjuncitons *)
let multiply orlist orlist' =
  let foil outer inner =
    List.fold outer ~init:[] ~f:(fun acc x ->
        List.map inner ~f:(mkAnd x)
        @ acc
      )
  in
  foil orlist orlist'
  @ foil orlist' orlist
    
  
(* Computes the Negation Normal Form of a test*)               
let rec nnf t : test =
  match t with
  | Eq(_, _)
  | Le(_, _)
  | True
  | False
  | Neg(Eq(_, _))
  | Neg(Le(_, _))
  | Neg(True)
  | Neg(False) -> t
  | Neg (Neg t) -> nnf t
  | And (a, b) -> mkAnd (nnf a) (nnf b)
  | Or (a, b) -> mkOr (nnf a) (nnf b)
  | Impl (a, b) -> nnf (!%(a) %+% b)
  | Iff (a, b) -> nnf (mkAnd (Impl(a,b)) (Impl (b,a)))
  | Neg(And(a, b)) -> mkOr (mkNeg a) (mkNeg b) |> nnf
  | Neg(Or(a, b)) -> mkAnd (mkNeg a) (mkNeg b) |> nnf
  | Neg(Impl(a, b)) -> mkAnd a (mkNeg b) |> nnf
  | Neg(Iff (a, b)) -> mkOr (mkAnd a (mkNeg b)) (mkAnd (mkNeg b) a)



                         

(* Computes the Disjunctive Normal form of a test *)
let rec dnf t : test list =
  let t' = nnf t in
  match t' with
  | And(a, b) -> multiply (dnf a) (dnf b)
  | Or (a, b) -> dnf a @ dnf b
  | Impl(a, b) -> dnf (nnf (!%(a) %+% b))
  | Iff(a,b) -> dnf (Impl (a,b) %&% Impl(b,a))
  | Eq _
  | Le _ 
  | Neg _ (* will not be And/Or because NNF*)
  | True
  | False  ->  [t']


                 
                 
(* Unrolls all loops in the program p n times *)
let rec unroll n p =
  match p, n with
  | While (_, _), 0 -> Skip
  | While (cond, body), _ -> 
    mkPartial [cond , (body %:% unroll (n-1) p)]
    (* %:% Assert (!% cond) *)
  | Seq (firstdo, thendo), _ ->
    Seq (unroll n firstdo, unroll n thendo)
  | Select (styp, cmds), _ ->
    List.map cmds ~f:(fun (cond, action) -> (cond, unroll n action))
    |> mkSelect styp
  | _ -> p (* Assign, Test, Assert cannot be unrolled *)

let get_val subsMap str default =
  StringMap.find subsMap str |> Option.value ~default

                                             
(** computes ex[xs -> vs], replacing only Vars whenever holes is false, replacing both whenever holes is true *)
let rec substitute ?holes:(holes = false) ex subsMap =
  let subst = get_val subsMap in
  let rec substituteE e =
    let binop op (e,e') = op (substituteE e) (substituteE e') in
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
    | Cast (i,e) -> mkCast i @@ substituteE e
    | Slice {hi;lo;bits} -> mkSlice hi lo @@ substituteE bits
    | Plus es | Times es | Minus es | Mask es | Xor es | BOr es | Shl es
      -> binop (ctor_for_binexpr e) es
  in
  match ex with
  | True | False -> ex
  (* Homomorphic Rules*)               
  | Neg e       -> !%(substitute ~holes e subsMap)
  | Or   (e, e') -> substitute ~holes e subsMap %+% substitute ~holes e' subsMap
  | And  (e, e') -> substitute ~holes e subsMap %&% substitute ~holes e' subsMap
  | Impl (e, e') -> substitute ~holes e subsMap %=>% substitute ~holes e' subsMap
  | Iff  (e, e') -> substitute ~holes e subsMap %<=>% substitute ~holes e' subsMap
  (* Do the work *)
  | Eq (e,e') ->  substituteE e %=% substituteE e'
  | Le (e,e') ->  substituteE e %<=% substituteE e'

let substV ?holes:(holes = false) ex substMap =
  StringMap.fold substMap ~init:StringMap.empty ~f:(fun ~key ~data acc ->
      Printf.printf "  [%s -> %s ]\n" key (string_of_value data);
      StringMap.set acc ~key ~data:(Value data))
  |> substitute ~holes ex

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
  | Assert t -> t %&% phi
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
  | While _ ->
    Printf.printf "[WARNING] skipping While loop, because loops must be unrolled\n%!";
    phi

let freshen v sz i = (v ^ "$" ^ string_of_int i, sz)
      
let good_execs fvs c =
  let rec indexVars_expr e (sub : ((int * int) StringMap.t)) =
    let binop f (e,e') = f (indexVars_expr e sub) (indexVars_expr e' sub) in
    match e with
    | Value _ -> e
    | Var (x,sz) ->
       begin match StringMap.find sub x with
       | None  -> "couldn't find "^x^" in substitution map with keys"
                  ^ (StringMap.keys sub |> List.fold ~init:"" ~f:(fun acc k -> Printf.sprintf "%s %s" acc k))
                  |> failwith
       | Some (i,_) ->  Var (freshen x sz i)
       end
    | Hole (x, sz) ->
       begin match StringMap.find sub x with
       | None  -> "couldn't find "^x^" in substitution map " |> failwith
       | Some (i,_) ->  Hole (freshen x sz i)
       end
    | Cast (i,e) -> mkCast i @@ indexVars_expr e sub
    | Slice {hi;lo;bits} -> mkSlice hi lo @@ indexVars_expr bits sub
    | Plus es | Minus es | Times es | Mask es | Xor es | BOr es | Shl es
      -> binop (ctor_for_binexpr e) es
  in
  let rec indexVars b sub =
    let binop_t op a b = op (indexVars a sub) (indexVars b sub) in
    let binop_e op a b = op (indexVars_expr a sub) (indexVars_expr b sub) in
    match b with
    | True | False -> b
    | Neg b -> !%(indexVars b sub)
    | And  (a,b) -> binop_t (%&%) a b
    | Or   (a,b) -> binop_t (%+%) a b
    | Impl (a,b) -> binop_t (%=>%) a b
    | Iff  (a,b) -> binop_t (%<=>%) a b
    | Eq (e1,e2) -> binop_e (%=%) e1 e2
    | Le (e1,e2) -> binop_e (%<=%) e1 e2
  in
  let rec passify sub c : ((int * int) StringMap.t * cmd) =
    match c with
    | Skip -> (sub, Skip)
    | Assert b ->
       (sub, Assert (indexVars b sub))
    | Assume b ->
       (sub, Assert (indexVars b sub))
    | Assign (f,e) ->
       begin match StringMap.find sub f with
       | None ->
          let sz = size_of_expr e in
          (StringMap.set sub ~key:f ~data:(1, sz)
          , Assume (Var (freshen f sz 0) %=% indexVars_expr e sub))
       | Some (idx, sz) ->
          (StringMap.set sub ~key:f ~data:(idx + 1,sz)
          , Assume (Var (freshen f sz (idx + 1)) %=% (indexVars_expr e sub)))
       end
    | Seq (c1, c2) ->
       let (sub1, c1') = passify sub  c1 in
       let (sub2, c2') = passify sub1 c2 in
       (sub2, c1' %:% c2')
    | Select (Total, _) -> failwith "Don't know what to do for if total"
    | Select (typ, ss) ->
       let sub_lst = List.map ss ~f:(fun (t,c) ->
                         let sub', c' = passify sub c in
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
         
    | While _ ->
       failwith "Cannot passify While loops Unsupported"
    | Apply _ ->
       failwith "Cannot passify (yet) table applications"
  in
  let rec good_wp c =
    match c with
    | Skip -> True
    | Assert b
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
    | While _ -> failwith "While Loops Deprecated"
    | Apply _ -> failwith "Tables should be applied at this stage"
  in
  let rec bad_wp c =
    match c with
    | Skip -> False
    | Assume _ -> False
    | Assert t -> !%t
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
    | While _ -> failwith "While Loops Deprecated"
    | Apply _ -> failwith "Tables should be applied at this stage"
  in
  let init_sub = List.fold fvs ~init:StringMap.empty ~f:(fun sub (v,sz) ->
                     StringMap.set sub ~key:v ~data:(0,sz)
                   ) in
  (* Printf.printf "active : \n %s \n" (string_of_cmd c); *)
  let merged_sub, passive_c = passify init_sub c  in
  (* Printf.printf "passive : \n %s\n" (string_of_cmd passive_c); *)
  (* let vc = good_wp passive_c in *)
  (* Printf.printf "good_executions:\n %s\n%!" (string_of_test vc); *)
  (merged_sub, good_wp passive_c, bad_wp passive_c)


let inits fvs sub =
  StringMap.fold sub ~init:[]
    ~f:(fun ~key:v ~data:(_,sz) vs ->
      if List.exists fvs ~f:(fun (x,_) -> x = v)
      then (freshen v sz 0) :: vs
      else vs)
  |> List.sort ~compare:(fun (u,_) (v,_) -> Stdlib.compare u v)

let finals fvs sub =
  StringMap.fold sub ~init:[]
    ~f:(fun ~key:v ~data:(i,sz) vs ->
      if List.exists fvs ~f:(fun (x,_) -> x = v)
      then (freshen v sz i) :: vs
      else vs)
  |> List.sort ~compare:(fun (u,_) (v,_) -> Stdlib.compare u v)

let zip_eq_exn xs ys =
  List.fold2_exn xs ys ~init:True ~f:(fun acc x y -> acc %&% (Var x %=% Var y) )

let rec prepend_expr pfx e =
  let binop op (e,e') = op (prepend_expr pfx e) (prepend_expr pfx e') in
  match e with
  | Value _ -> e
  | Var (v,sz) -> Var(pfx^v, sz)
  | Hole(v, sz) -> Var(pfx^v, sz)
  | Cast (i,e) -> mkCast i @@ prepend_expr pfx e
  | Slice {hi;lo;bits} -> mkSlice hi lo @@ prepend_expr pfx bits
  | Plus es | Minus es | Times es | Mask es | Xor es | BOr es | Shl es
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
  | Assign(f,e) -> Assign(pfx^f, prepend_expr pfx e)
  | Assert b -> prepend_test pfx b |> Assert
  | Assume b -> prepend_test pfx b |> Assume
  | Seq(c1,c2) -> prepend pfx c1 %:% prepend pfx c2
  | While (b,c) -> mkWhile (prepend_test pfx b) (prepend pfx c)
  | Select(typ, cs) ->
     List.map cs ~f:(fun (t,c) -> (prepend_test pfx t, prepend pfx c))
     |> mkSelect typ
  | Apply t ->
     Apply {name = pfx ^ t.name;
            keys = List.map t.keys ~f:(fun (k,sz) -> (pfx ^ k, sz));
            actions = List.map t.actions ~f:(fun (scope, act) -> (List.map scope ~f:(fun (x,sz) -> (pfx ^ x, sz)), prepend pfx act));
            default = prepend pfx t.default}
  
                 
let equivalent ?neg:(neg = True) eq_fvs l p =
  (* Printf.printf "assuming %s\n%!" (string_of_test neg); *)
  let l = Assume neg %:% l in
  let p = Assume neg %:% p in
  let phys_prefix = "phys_"in
  let p' = prepend phys_prefix p in
  let fvs =
    free_of_cmd `Hole l
    @ free_of_cmd `Var l
    @ free_of_cmd `Hole p
    @ free_of_cmd `Var p
  in
  let prefix_list =  List.map ~f:(fun (x,sz) -> (phys_prefix ^ x, sz)) in
  let fvs_p = prefix_list fvs in
  let eq_fvs_p = prefix_list eq_fvs in
  let sub_l, gl, _ = good_execs fvs l in
  let sub_p, gp, _ = good_execs fvs_p p' in
  let lin = inits eq_fvs sub_l in
  let pin = inits eq_fvs_p sub_p in
  let lout = finals eq_fvs sub_l in
  let pout = finals eq_fvs_p sub_p in
  (* let _ = Printf.printf "lin: ";
   *         List.iter lin ~f:(fun (v, _) -> Printf.printf " %s" v);
   *         Printf.printf "\n";
   *         Printf.printf "pin: ";
   *         List.iter pin ~f:(fun (v, _) -> Printf.printf " %s" v);
   *         Printf.printf "\n"
   * in *)
  let in_eq = zip_eq_exn lin pin in
  let out_eq = zip_eq_exn lout pout in
  (* Printf.printf "===Verifying===\n%s\nand\n%s\nand\n%s\nimplies\n%s"
   *   (string_of_test gl)
   *   (string_of_test gp)
   *   (string_of_test in_eq)
   *   (string_of_test out_eq); *)
  (* match StringMap.find sub_l "drop", StringMap.find sub_p "phys_drop" with
   * | Some (i,_), Some (j,_) ->
   *    (\* let ldrop = Var(freshen "drop" 1 i) in
   *     * let pdrop = Var(freshen "phys_drop" 1 j) in
   *     * let tt = mkVInt(1,1) in
   *     * let ff = mkVInt(0,1) in
   *     * let cond = ((ldrop %=% ff) %&% (pdrop %=% ff) %&% out_eq)
   *     *            %+% ((ldrop %=% tt) %&% (pdrop %=% tt)) in *\)
   *    (\* Printf.printf "============ DROP VC =========\n%!"; *\)
   *    (gl %&% gp %&% (!%bl) %&% (!%bl) %&% in_eq) %=>% out_eq
   * | _, _ -> *)
     (* Printf.printf "============ NORMAL VC =========\n%!"; *)
  ((gl %&% gp (*%&% (!%bl) %&% (!%bp)*) %&% in_eq) %=>% out_eq)
  (* %&% (bl %<=>% bp) *)
    
  

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
  | Plus es | Minus es | Times es | Mask es | Xor es | BOr es | Shl es
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
  | Assert t ->
     fill_holes_test t subst |> Assert
  | Assume t ->
     fill_holes_test t subst |> Assume
  | Select (_,[]) | Skip ->
     c
  | Select (styp, cmds) ->
     rec_select cmds |> mkSelect styp
  | While (cond, body) -> While (fill_holes_test cond subst, fill_holes body subst)
  | Apply t
    -> Apply { t with
               actions = List.map t.actions ~f:(fun (scope, a) -> (scope, fill_holes a subst));
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
  | Assert t -> [(c, t %&% phi)]
  | Assume t -> [(c, t %=>% phi)]
                  
  (* requires at least one guard to be true *)
  | Select (Total, []) -> [(Skip, True)]
  | Select (Total, cmds) ->
     let open List in
     (cmds >>| fun (t,c) -> Assert t %:% c)
     >>= flip (wp_paths negs) phi

                  
  (* doesn't require at any guard to be true *)
  | Select (Partial, []) -> [(Skip, True)]
  | Select (Partial, cmds) ->
     List.fold cmds ~init:([]) ~f:(fun wp_so_far (cond, act) ->
          List.fold (wp_paths negs act phi) ~init:wp_so_far
             ~f:(fun acc (trace, act_wp) ->
               acc @ [(Assert cond %:% trace, cond %&% act_wp)]))

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
             (Assert (cond %&% misses) %:% trace, cond %&% misses %&% act_wp) :: acc), prev_conds %+% cond)
     |> fst

  | Apply t ->
     let open List in
     (t.default :: List.map ~f:(fun (sc, a) -> holify (List.map sc ~f:fst) a) t.actions) >>= flip (wp_paths negs) phi
  | While _ ->
     failwith "[Error] loops must be unrolled\n%!"

let bind_action_data vals (scope, cmd) : cmd =
  let holes = List.map scope ~f:fst in
  (* Printf.printf "holes:";
   * List.iter holes ~f:(Printf.printf " %s");
   * Printf.printf "\n%!";
   * Printf.printf "vals:";
   * List.iter holes ~f:(Printf.printf " %s");
   * Printf.printf "\n%!"; *)
  List.fold2_exn holes vals
    ~init:StringMap.empty
    ~f:(fun acc x v -> StringMap.set acc ~key:x ~data:v)
  |> fill_holes (holify holes cmd) 


let rec fixup_val (model : value StringMap.t) (e : expr)  : expr =
  (* let _ = Printf.printf "FIXUP\n%!" in *)
  let binop op (e,e') = op (fixup_val model e) (fixup_val model e') in
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
  | Cast (i,e) -> mkCast i @@ fixup_val model e
  | Slice {hi;lo;bits} -> mkSlice hi lo @@ fixup_val model bits
  | Plus es | Times es | Minus es | Mask es | Xor es | BOr es | Shl es
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
  | Eq (v, w) -> binop (%=%)  (fixup_val model) v w
  | Le (v, w) -> binop (%<=%) (fixup_val model) v w

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
  | Assign (f, v) -> Assign(f, fixup_val model v)
  | Assert t -> Assert (fixup_test model t)
  | Assume t -> Assume (fixup_test model t)
  | Seq (p, q) -> Seq (fixup p model, fixup q model)
  | While (cond, body) -> While (fixup_test model cond, fixup body model)
  | Select (styp,cmds) -> fixup_selects model cmds |> mkSelect styp
  | Apply t
    -> Apply {t with
              actions = List.map t.actions ~f:(fun (data, a) -> (data, fixup a model));
              default = fixup t.default model}
