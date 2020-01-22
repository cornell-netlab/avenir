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
  | Lt(_, _)
  | Member(_,_)
  | True
  | False
  | Neg(Eq(_, _))
  | Neg(Lt(_, _))
  | Neg(Member(_,_))
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
  | Member (_,_)
  | Eq _
  | Lt _ 
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
    match e with
    | Var1 (field,_) -> subst field e
    | Hole1 (field,_) ->
       if holes then
         let e' = subst field e in
         (Printf.printf "%s -> %s \n%!" field (string_of_expr1 e');
          e')
       else (Printf.printf "NO SUBST\n%!";  e)
    | Value1 _ -> e
    | Plus (e, e') -> Plus (substituteE e, substituteE e')
    | Times (e, e') -> Times (substituteE e, substituteE e')
    | Minus (e, e') -> Minus (substituteE e, substituteE e')
    | Tuple es -> List.map es ~f:substituteE |> Tuple
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
  | Lt (e,e') ->  substituteE e %<% substituteE e'
  | Member(e,set) -> Member(substituteE e, set)

let substV ?holes:(holes = false) ex substMap =
  StringMap.map substMap ~f:(fun v -> Value1 v)
  |> substitute ~holes ex

let rec exact_only t =
  match t with
  | Lt _ | Member _ -> false
  | True | False | Eq _ -> true
  | Neg(a) -> exact_only a
  | And(a,b) | Or(a,b) | Impl(a,b) | Iff(a,b)
    -> exact_only a && exact_only b

let regularize cond misses =
  if exact_only cond
  then if cond = True
       then !%misses
       else cond
  else failwith "Don't know how to regularize anything but equivalences" 
         
                
(* computes weakest pre-condition of condition phi w.r.t command c *)
let rec wp c phi =
  let guarded_wp (cond, act) = cond %=>% wp act phi in
  match c with
  | Skip -> phi
  | Seq (firstdo, thendo) ->
    wp firstdo (wp thendo phi)
  | Assign (field, value) ->
     substitute phi (StringMap.singleton field value)
  | Assert t -> t %&% phi
  | Assume t -> t %=>% phi
              
  (* requires at least one guard to be true *)
  | Select (Total, []) -> True
  | Select (Total, cmds) ->
    concatMap cmds ~c:(%+%) ~f:fst 
    %&% concatMap cmds ~c:(%&%) ~f:guarded_wp
    
  (* doesn't require at any guard to be true *)
  | Select (Partial, []) -> True
  | Select (Partial, cmds) ->
     concatMap cmds ~c:(%&%) ~f:guarded_wp

  (* negates the previous conditions *)
  | Select (Ordered, cmds) ->
     List.fold cmds ~init:(True, False) ~f:(fun (wp_so_far, misses) (cond, act) ->
         let guard = regularize cond misses in
        (guard %=>% wp act phi %&% wp_so_far
        , cond %+% misses )
      )
    |> fst

  | Apply (_, _, acts, dflt)
    -> concatMap acts  ~f:(fun a -> wp a phi) ~c:(mkAnd) ~init:(Some True)
      %&% wp dflt phi
  | While _ ->
    Printf.printf "[WARNING] skipping While loop, because loops must be unrolled\n%!";
    phi

let good_execs c =
  let rec passify sub c =
    match c with
    | Skip -> (Some sub, Skip)
    | Assert b ->
       if b = False
       then (None, Assert False)
       else (Some sub, Assert (indexVars b sub))
    | Assume b ->
       if b = False
       then (None, Assert False)
       else (Some sub, Assert (indexVars b sub))
    | Assign (v,e) ->
       (StringMap.update sub v ~f:(fun idx ->
            match idx with
            | None -> 0
            | Some i -> i +1 ) |> Some
       , v %<-% (indexVars_expr1 sub e))
    | Seq (c1, c2) ->
       let (sub1, c1') = passify sub c1 in
       let (sub2, c2') = passify sub c2 in
       (sub2, c1' %:% c2)
    | Select (Total, _) -> failwith "Don't know what to do for if total"
    | Select (_, ss) ->
       (* let passify_demon_bin (t1,c1) (t2,c2) =
        *   let t1' = indexVars sub t1 in
        *   let t2' = indexVars sub t2 in
        *   let sub1, c1' = passify sub c1 in
        *   let sub2, c2' = passify sub c2 in
        *   let (sub', r1, r2) = merge sub1 sub2 in
        *   (sub', (t1, c1' %:% r1, c2' %:% r2))
        * in *)
       let subs = List.map ss ~f:(fun (t,c) ->
                      let sub, c' = passify sub c in
                      (sub, (indexVars sub t, c'))) in
       let conflicts = in
       let 
       
    | While _ ->
       failwith "Cannot passify While loops Unsupported"
    | Apply _ ->
       failwith "Cannot passify (yet) table applications"
       

  in
  let rec good_wp _ = True in
  passify c
  |> good_wp
  
  
  

(** [fill_holes(|_value|_test]) replace the applies the substitution
   [subst] to the supplied cmd|value|test. It only replaces HOLES, and
   has no effect on vars *)
let rec fill_holes_expr1 e (subst : value1 StringMap.t) =
  let fill_holesS e = fill_holes_expr1 e subst in
  let binop op e e' = op (fill_holesS e) (fill_holesS e') in
  match e with
  | Value1 _ | Var1 _ -> e
  | Hole1 (h,sz) ->
     begin match StringMap.find subst h with
     | None -> e
     | Some v -> let sz' = size_of_value1 v in
                 let strv = string_of_value1 v in
                 (if sz <> sz' then (Printf.printf "[Warning] replacing %s#%d with %s#%d, but the sizes may be different, taking the size of %s to be ground truth" h sz strv (size_of_value1 v) strv));
                 Value1 v
     end
  | Plus (e, e') -> binop mkPlus e e'
  | Minus (e, e') -> binop mkMinus e e'
  | Times (e, e') -> binop mkTimes e e'
  | Tuple es -> List.map es ~f:(fill_holesS) |> Tuple


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
  | Lt   (a, b)   -> binop (%<=%)  fill_holes_expr1 a b
  | Eq   (a, b)   -> binop (%=%)   fill_holes_expr1 a b
  | Member (a, s) -> Member(fill_holes_expr1 a subst, s)

let rec fill_holes (c : cmd) subst =
  let rec_select = concatMap ~c:(@)
                     ~f:(fun (cond, act) ->
                       [(fill_holes_test cond subst, fill_holes act subst)]) in
  match c with
  | Assign (f, Hole1 (h,sz)) ->
     begin match StringMap.find subst h with
     | None -> c
     | Some v -> let sz' = size_of_value1 v in
                 let strv = string_of_value1 v in
                 (if sz <> sz' then (Printf.printf "[Warning] replacing %s#%d with %s#%d, but the sizes may be different, taking the size of %s to be ground truth" h sz strv (size_of_value1 v) strv));
                 Assign (f, Value1 v)
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
  | Apply (n,keys, acts, dflt)
    -> Apply(n, keys
             , List.map acts ~f:(fun act -> fill_holes act subst)
             , fill_holes dflt subst)
            


let rec wp_paths c phi : test list =
  match c with
  | Skip -> [phi]
  | Seq (firstdo, thendo) ->
     List.(wp_paths thendo phi >>= wp_paths firstdo)
  | Assign (field, value) ->
     [substitute phi (StringMap.singleton field value)]
  | Assert t -> [t %&% phi]
  | Assume t -> [t %=>% phi]
                  
  (* requires at least one guard to be true *)
  | Select (Total, []) -> [True]
  | Select (Total, cmds) ->
     let open List in
     (cmds >>| fun (t,c) -> Assert t %:% c)
     >>= Fun.flip wp_paths phi

                  
  (* doesn't require at any guard to be true *)
  | Select (Partial, []) -> [True]
  | Select (Partial, cmds) ->
     let open List in
     (cmds >>| fun (t,c) -> Assume t %:% c)
     >>= Fun.flip wp_paths phi
                  
  (* negates the previous conditions *)
  | Select (Ordered, cmds) ->
     let open List in
     (cmds >>| fun (t,c) -> Assume t %:% c)
     >>= Fun.flip wp_paths phi
  (* List.fold cmds ~init:(True, False) ~f:(fun (wp_so_far, prev_conds) (cond, act) ->
   *     guarded_wp (cond %&% !%prev_conds, act) %&% wp_so_far
   *                                        , prev_conds %+% cond
   *   )
   * |> fst *)

  | Apply (_, _, acts, dflt) ->
     let open List in
     (dflt :: acts) >>= Fun.flip wp_paths phi
  | While _ ->
     Printf.printf "[WARNING] skipping While loop, because loops must be unrolled\n%!";
     [phi]
