open Core
open Util

let get_val subsMap str default =
  StringMap.find subsMap str |> Option.value ~default

let rec substituteE ?(holes = false) substMap e =
  let binop op (e, e') =
    op (substituteE ~holes substMap e) (substituteE ~holes substMap e')
  in
  let subst = get_val substMap in
  match e with
  | Expr.Value _ -> e
  | Expr.Var (field, _) ->
      let replacement = subst field e in
      (* Printf.printf "substituting %s for %s;\n%!" field (string_of_expr
         e); *)
      replacement
  | Expr.Hole (field, _) ->
      if holes then
        let e' = subst field e in
        (*Printf.printf "%s -> %s \n%!" field (string_of_expr e');*)
        e'
      else (*Printf.printf "NO SUBST\n%!";*) e
  | Expr.Cast (i, e) -> Expr.cast i @@ substituteE ~holes substMap e
  | Expr.Slice {hi; lo; bits} ->
      Expr.slice hi lo @@ substituteE ~holes substMap bits
  | Expr.Plus es
   |Expr.Times es
   |Expr.Minus es
   |Expr.Mask es
   |Expr.Xor es
   |Expr.BOr es
   |Expr.Shl es
   |Expr.Concat es
   |Expr.SatPlus es
   |Expr.SatMinus es ->
      binop (Expr.bin_ctor e) es

(** computes ex[xs -> vs], replacing only Vars whenever holes is false,
    replacing both whenever holes is true *)
let rec substitute ?(holes = false) ex subsMap =
  match ex with
  | Test.True | Test.False -> ex
  | Test.Neg e -> Test.neg (substitute ~holes e subsMap)
  | Test.Or (e, e') ->
      Test.or_ (substitute ~holes e subsMap) (substitute ~holes e' subsMap)
  | Test.And (e, e') ->
      Test.and_ (substitute ~holes e subsMap) (substitute ~holes e' subsMap)
  | Test.Impl (e, e') ->
      Test.impl (substitute ~holes e subsMap) (substitute ~holes e' subsMap)
  | Test.Iff (e, e') ->
      Test.iff (substitute ~holes e subsMap) (substitute ~holes e' subsMap)
  | Test.Eq (e, e') ->
      Test.eq (substituteE ~holes subsMap e) (substituteE ~holes subsMap e')
  | Test.Le (e, e') ->
      Test.leq (substituteE ~holes subsMap e) (substituteE ~holes subsMap e')

let substV ?(holes = false) ex substMap =
  StringMap.fold substMap ~init:StringMap.empty ~f:(fun ~key ~data acc ->
      StringMap.set acc ~key ~data:(Expr.Value data) )
  |> substitute ~holes ex

let rec substitute_cmd ?(holes = false) cmd subst =
  match cmd with
  | Cmd.Skip -> Cmd.Skip
  | Cmd.Assume t -> substitute ~holes t subst |> Cmd.assume
  | Cmd.Assign (f, e) -> (
    match StringMap.find subst f with
    | Some e' ->
        failwith
        @@ Printf.sprintf
             "Tried to substitute %s for %s but it occurs on the right hand \
              side of the assignment %s"
             f (Expr.to_string e') (Cmd.to_string cmd)
    | None -> Cmd.assign f @@ substituteE ~holes subst e )
  | Cmd.Seq (c1, c2) ->
      Cmd.seq
        (substitute_cmd ~holes c1 subst)
        (substitute_cmd ~holes c2 subst)
  | Cmd.Select (typ, cs) ->
      List.map cs ~f:(fun (b, c) ->
          (substitute ~holes b subst, substitute_cmd ~holes c subst) )
      |> Cmd.select typ
  | Cmd.Apply _ ->
      failwith
        "[Manip.substitute_cmd] Don't know how to substitute into table"

let rec exact_only t =
  let open Test in
  match t with
  | Le _ -> false
  | True | False | Eq _ -> true
  | Neg a -> exact_only a
  | And (a, b) | Or (a, b) | Impl (a, b) | Iff (a, b) ->
      exact_only a && exact_only b

let regularize negs cond misses =
  let open Test in
  match negs with
  | `NoNegs when equals cond True -> cond %&% !%misses
  | `NoNegs -> cond
  | `Negs -> cond %&% !%misses

(* computes weakest pre-condition of condition phi w.r.t command c *)
let rec wp negs c phi =
  let open Cmd in
  let open Test in
  match c with
  | Skip -> phi
  | Seq (firstdo, thendo) -> wp negs firstdo @@ wp negs thendo phi
  | Assign (field, value) ->
      let phi' = substitute phi @@ StringMap.singleton field value in
      (* Printf.printf "replacing %s with %s in %s \n to get %s \n" field
         (string_of_expr value) (string_of_test phi) (string_of_test phi'); *)
      phi'
  | Assume t -> Test.(t %=>% phi)
  (* requires at least one guard to be true *)
  | Select (Total, []) -> True
  | Select (Total, _) -> failwith "total is unsupported\n"
  (* doesn't require at any guard to be true *)
  | Select (Partial, []) -> True
  | Select (Partial, cmds) ->
      let phi' =
        List.fold cmds ~init:False ~f:(fun wp_so_far (cond, act) ->
            let act_wp = wp negs act phi in
            (* Printf.printf "Combining guard: %s  action: %s and accumulation %s\n%!==%s\n%!"
             *   (string_of_test cond) (string_of_test act_wp) (string_of_test wp_so_far)
             *   ((cond %&% act_wp) %+% wp_so_far |> string_of_test); *)
            cond %&% act_wp %+% wp_so_far )
      in
      (* Printf.printf "{%s}\n %s\n {%s}\n%!" (string_of_test phi')
         (string_of_cmd c) (string_of_test phi); *)
      phi'
  (* negates the previous conditions *)
  | Select (Ordered, cmds) ->
      let phi' =
        List.fold cmds ~init:(True, False)
          ~f:(fun (wp_so_far, misses) (cond, act) ->
            let guard = regularize negs cond misses in
            let act_wp = wp negs act phi in
            (* Printf.printf "Combining guard: %s  action: %s and accumulation %s\n%!==%s\n%!"
             *   (string_of_test guard) (string_of_test act_wp) (string_of_test wp_so_far)
             *   ((guard %=>% act_wp) %&% wp_so_far |> string_of_test); *)
            (guard %=>% act_wp %&% wp_so_far, cond %+% misses) )
        |> fst
      in
      (* Printf.printf "{%s}\n %s\n {%s}\n%!" (string_of_test phi')
         (string_of_cmd c) (string_of_test phi); *)
      phi'
  | Apply _ -> failwith "wp of apply is no good"

(* concatMap acts ~f:(fun (scope, a) -> * wp ~no_negations (holify (List.map
   scope ~f:fst) a) phi) ~c:(mkAnd) ~init:(Some True) * %&% wp ~no_negations
   dflt phi *)

(** [fill_holes(|_value|_test]) replace the applies the substitution [subst]
    to the supplied cmd|value|test. It only replaces HOLES, and has no effect
    on vars *)
let rec fill_holes_expr e (subst : Model.t) =
  let fill_holesS e = fill_holes_expr e subst in
  let binop op (e, e') = op (fill_holesS e) (fill_holesS e') in
  let open Expr in
  match e with
  | Value _ | Var _ -> e
  | Hole (h, sz) -> (
    match Model.find subst h with
    | None -> e
    | Some v ->
        let sz' = Value.size v in
        if sz <> sz' then
          Printf.printf
            "[Warning] replacing %s#%d with %s, but the sizes may be \
             different, taking the size of %s to be ground truth"
            h sz (Value.to_string v) (Value.to_string v) ;
        Value v )
  | Cast (i, e) -> cast i @@ fill_holesS e
  | Slice {hi; lo; bits} -> slice hi lo @@ fill_holesS bits
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

(* Fills in first-order holes according to subst *)
let rec fill_holes_test t subst =
  let open Test in
  let binop cnstr rcall left right =
    cnstr (rcall left subst) (rcall right subst)
  in
  match t with
  | True | False -> t
  | Neg a -> !%(fill_holes_test a subst)
  | And (a, b) -> binop ( %&% ) fill_holes_test a b
  | Or (a, b) -> binop ( %+% ) fill_holes_test a b
  | Impl (a, b) -> binop ( %=>% ) fill_holes_test a b
  | Iff (a, b) -> binop ( %<=>% ) fill_holes_test a b
  | Le (a, b) -> binop ( %<=% ) fill_holes_expr a b
  | Eq (a, b) -> binop ( %=% ) fill_holes_expr a b

let rec fill_holes (c : Cmd.t) subst =
  let open Cmd in
  let rec_select =
    concatMap ~c:( @ ) ~f:(fun (cond, act) ->
        [(fill_holes_test cond subst, fill_holes act subst)] )
  in
  match c with
  | Assign (f, Hole (h, sz)) -> (
    match Model.find subst h with
    | None -> c
    | Some v ->
        let sz' = Value.size v in
        if sz <> sz' then
          Printf.printf
            "[Warning] replacing %s#%d with %s, but the sizes may be \
             different, taking the size of %s to be ground truth"
            h sz (Value.to_string v) (Value.to_string v) ;
        Assign (f, Value v) )
  | Assign (_, _) -> c
  | Seq (firstdo, thendo) ->
      fill_holes firstdo subst %:% fill_holes thendo subst
  | Assume t -> fill_holes_test t subst |> Assume
  | Select (_, []) | Skip -> c
  | Select (styp, cmds) -> rec_select cmds |> select styp
  | Apply t ->
      Apply
        { t with
          actions=
            List.map t.actions ~f:(fun (n, scope, a) ->
                (n, scope, fill_holes a subst) )
        ; default= fill_holes t.default subst }

let rec wp_paths negs c phi : (Cmd.t * Test.t) list =
  let open Cmd in
  let open Test in
  match c with
  | Skip -> [(c, phi)]
  | Seq (c1, c2) ->
      List.map (wp_paths negs c2 phi) ~f:(fun (trace2, phi) ->
          List.map (wp_paths negs c1 phi) ~f:(fun (trace1, phi') ->
              (trace1 %:% trace2, phi') ) )
      |> List.join
  | Assign (field, e) ->
      let phi' = substitute phi (StringMap.singleton field e) in
      (* Printf.printf "substituting %s |-> %s\n into %s to get %s \n%!"
         field (string_of_expr e) (string_of_test phi') (string_of_test
         phi'); *)
      [(c, phi')]
  | Assume t -> [(c, t %=>% phi)]
  (* requires at least one guard to be true *)
  | Select (Total, []) -> [(Skip, True)]
  | Select (Total, cmds) ->
      let open List in
      cmds >>| (fun (t, c) -> assume t %:% c) >>= flip (wp_paths negs) phi
  (* doesn't require at any guard to be true *)
  | Select (Partial, []) -> [(Skip, True)]
  | Select (Partial, cmds) ->
      List.fold cmds ~init:[] ~f:(fun wp_so_far (cond, act) ->
          List.fold (wp_paths negs act phi) ~init:wp_so_far
            ~f:(fun acc (trace, act_wp) ->
              acc @ [(assume cond %:% trace, cond %&% act_wp)] ) )
  (* negates the previous conditions *)
  | Select (Ordered, cmds) ->
      (* let open List in
       * (cmds >>| fun (t,c) -> Assume t %:% c)
       * >>= Fun.flip wp_paths phi *)
      List.fold cmds ~init:([], False)
        ~f:(fun (wp_so_far, prev_conds) (cond, act) ->
          ( List.fold (wp_paths negs act phi) ~init:wp_so_far
              ~f:(fun acc (trace, act_wp) ->
                let misses =
                  match negs with
                  | `NoNegs -> True
                  | `Negs -> !%prev_conds
                in
                ( assume (cond %&% misses) %:% trace
                , cond %&% misses %&% act_wp )
                :: acc )
          , prev_conds %+% cond ) )
      |> fst
  | Apply t ->
      let open List in
      t.default
      :: List.map
           ~f:(fun (_, sc, a) -> Cmd.holify (List.map sc ~f:fst) a)
           t.actions
      >>= flip (wp_paths negs) phi

let bind_action_data (vals : Value.t list) (_, scope, cmd) : Cmd.t =
  let holes = fsts scope in
  let subst =
    List.fold2 holes vals ~init:StringMap.empty ~f:(fun acc x v ->
        StringMap.set acc ~key:x ~data:(Expr.Value v) )
  in
  match subst with
  | Ok subst -> substitute_cmd cmd subst
  | Unequal_lengths ->
      Printf.sprintf
        "Incorrect number of action arguments: (%s) vs (%s) for %s"
        ( List.map vals ~f:Value.to_string
        |> List.reduce ~f:(Printf.sprintf "%s,%s")
        |> Option.value ~default:"" )
        ( List.reduce holes ~f:(Printf.sprintf "%s,%s")
        |> Option.value ~default:"" )
        (Cmd.to_string cmd)
      |> failwith

let rec fixup_expr (model : Model.t) (e : Expr.t) : Expr.t =
  (* let _ = Printf.printf "FIXUP\n%!" in *)
  let binop op (e, e') = op (fixup_expr model e) (fixup_expr model e') in
  match e with
  | Value _ | Var _ -> e
  | Hole (h, sz) -> (
    match Model.find model h with
    | None -> e
    | Some v ->
        let sz' = Value.size v in
        let strv = Value.to_string v in
        if sz <> sz' then
          Printf.printf
            "[Warning] replacing %s#%d with %s, but the sizes may be \
             different, taking the size of %s to be ground truth\n\
             %!"
            h sz strv strv ;
        Value v )
  | Cast (i, e) -> Expr.cast i @@ fixup_expr model e
  | Slice {hi; lo; bits} -> Expr.slice hi lo @@ fixup_expr model bits
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
      binop (Expr.bin_ctor e) es

let rec fixup_test (model : Model.t) (t : Test.t) : Test.t =
  let binop ctor call left right = ctor (call left) (call right) in
  match t with
  | True | False -> t
  | Neg p -> Test.neg (fixup_test model p)
  | And (p, q) -> binop Test.( %&% ) (fixup_test model) p q
  | Or (p, q) -> binop Test.( %+% ) (fixup_test model) p q
  | Impl (p, q) -> binop Test.( %=>% ) (fixup_test model) p q
  | Iff (p, q) -> binop Test.( %<=>% ) (fixup_test model) p q
  | Eq (v, w) -> binop Test.( %=% ) (fixup_expr model) v w
  | Le (v, w) -> binop Test.( %<=% ) (fixup_expr model) v w

let rec fixup_selects (model : Model.t) (es : (Test.t * Cmd.t) list) =
  match es with
  | [] -> []
  | (cond, act) :: es' ->
      let cond' = fixup_test model cond in
      let act' = fixup act model in
      (* Printf.printf "  [fixup] replacing %s with %s\n%!"
       *   (string_of_test cond) (string_of_test cond');
       * Printf.printf "  [fixup] replacing %s with %s\n%!" *)
      (* (string_of_cmd act) (string_of_cmd act'); *)
      (cond', act')
      ::
      ( if Test.equals cond cond' && Cmd.equals act act' then
        fixup_selects model es'
      else (cond, act) :: fixup_selects model es' )

and fixup (real : Cmd.t) (model : Model.t) : Cmd.t =
  (* Printf.printf "FIXUP WITH MODEL: %s\n%!\n" (string_of_map model); *)
  let open Cmd in
  match real with
  | Skip -> Skip
  | Assign (f, v) -> Assign (f, fixup_expr model v)
  | Assume t -> Assume (fixup_test model t)
  | Seq (p, q) -> Seq (fixup p model, fixup q model)
  | Select (styp, cmds) -> fixup_selects model cmds |> select styp
  | Apply t ->
      Apply
        { t with
          actions=
            List.map t.actions ~f:(fun (n, data, a) ->
                (n, data, fixup a model) )
        ; default= fixup t.default model }

let rec action_reads (nm, data, cmd) =
  let open Cmd in
  let to_set = StringSet.of_list %. fsts in
  let data_s = to_set data in
  match cmd with
  | Skip -> StringSet.empty
  | Assume b -> StringSet.diff (to_set @@ Test.vars b) data_s
  | Assign (_, e) ->
      StringSet.diff (StringSet.of_list @@ fsts @@ Expr.frees `Var e) data_s
  | Seq (c1, c2) ->
      action_reads (nm, data, c1)
      |> StringSet.union @@ action_reads (nm, data, c2)
  | Select (_, cs) ->
      concatMap cs ~c:StringSet.union ~f:(fun (b, c) ->
          to_set (Test.vars b)
          |> StringSet.union @@ action_reads (nm, data, c) )
  | Apply t ->
      Printf.sprintf "table %s not appeard in action %s" t.name nm
      |> failwith

let is_zero = function
  | Expr.Value v -> Bigint.(Value.get_bigint v = zero)
  | _ -> false

let rec is_a_sequence_of_zero_assignments =
  let open Cmd in
  function
  | Skip | Assume _ -> true
  | Assign (_, e) -> is_zero e
  | Seq (c1, c2) ->
      is_a_sequence_of_zero_assignments c1
      && is_a_sequence_of_zero_assignments c2
  | _ -> false
