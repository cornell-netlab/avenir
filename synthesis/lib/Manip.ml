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

let rec fixup_expr (model : Model.t) (e : expr)  : expr =
  (* let _ = Printf.printf "FIXUP\n%!" in *)
  let binop op (e,e') = op (fixup_expr model e) (fixup_expr model e') in
  match e with
  | Value _ | Var _ -> e
  | Hole (h,sz) -> 
     begin match Model.find model h with
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

let rec fixup_test (model : Model.t) (t : test) : test =
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

let rec fixup_selects (model : Model.t) (es : (test * cmd) list) =
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
and fixup (real:cmd) (model : Model.t) : cmd =
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
