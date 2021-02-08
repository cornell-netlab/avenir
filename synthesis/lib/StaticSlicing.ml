open Core
open Util

let fvs_to_set = StringSet.of_list %. fsts

let string_of_facts facts =
  StringMap.fold facts ~init:"" ~f:(fun ~key ~data acc ->
      List.fold data ~init:"" ~f:(fun acc v ->
          Printf.sprintf "%s %s" acc (Value.to_string v))
      |> Printf.sprintf "%s\n\t%s->[%s ]" acc key)
  |> Printf.sprintf "{%s\n}"

let rec static_slice_aux (fvs : StringSet.t) (c : Cmd.t) :
    StringSet.t * Cmd.t =
  match c with
  | Skip -> (fvs, Skip)
  | Assign (f, e) ->
      if StringSet.mem fvs f then
        ( ( StringSet.remove fvs f
          |> StringSet.(union (fvs_to_set (Expr.frees `Var e)))
          |> StringSet.(union (fvs_to_set (Expr.frees `Hole e))) )
        , c )
      else (fvs, Skip)
  | Assume t ->
      ( fvs
        |> StringSet.union (fvs_to_set @@ Test.frees `Var t)
        |> StringSet.union (fvs_to_set @@ Test.frees `Hole t)
      , c )
  | Seq (c1, c2) ->
      let fvs2', c2' = static_slice_aux fvs c2 in
      let fvs1', c1' = static_slice_aux fvs2' c1 in
      (fvs1', Cmd.seq c1' c2')
  | Select (styp, ss) ->
      let tfx =
        List.map ss ~f:(fun (b, c) ->
            let fvs', c' = static_slice_aux fvs c in
            (b, c', fvs'))
      in
      if List.exists tfx ~f:(fun (_, c, _) -> not (Cmd.is_skip c)) then
        let fvs', ss' =
          List.fold tfx ~init:(StringSet.empty, [])
            ~f:(fun (fvs_acc, ss_acc) (b, c, fvs) ->
              ( StringSet.union fvs_acc fvs
                |> StringSet.union (fvs_to_set @@ Test.frees `Var b)
                |> StringSet.union (fvs_to_set @@ Test.frees `Hole b)
              , ss_acc @ [(b, c)] ))
        in
        (fvs', Cmd.select styp ss')
      else (fvs, Skip)
  | Apply {name; keys; actions; default; _} ->
      let tfx =
        List.map actions ~f:(fun (n, vars, a) ->
            let fvs', a' = static_slice_aux fvs a in
            ( n
            , a'
            , vars
            , StringSet.filter fvs' ~f:(fun s ->
                  not (List.exists vars ~f:(fun (s', _) -> String.(s = s'))))
            ))
      in
      let def_fvs, def' = static_slice_aux fvs default in
      if
        List.exists tfx ~f:(fun (_, c, _, _) ->
            not Cmd.(is_skip c && is_skip def'))
      then
        let fvs =
          List.fold tfx ~init:StringSet.empty ~f:(fun acc (_, _, _, fvs) ->
              StringSet.union acc fvs)
          |> StringSet.(union (of_list @@ List.map ~f:Cmd.Key.var_name keys))
          |> StringSet.union def_fvs
        in
        let actions' =
          List.map tfx ~f:(fun (n, a, vars, _) -> (n, vars, a))
        in
        (fvs, Apply {name; keys; actions= actions'; default= def'})
      else (fvs, Skip)

let static_slice fvs cmd = snd @@ static_slice_aux fvs cmd

let rec flows_to (vs : StringSet.t) cmd : StringSet.t =
  let open Cmd in
  let in_vs v = StringSet.mem vs v in
  match cmd with
  | Skip -> vs
  | Assign (f, e) ->
      let e_vs = fsts @@ Expr.frees `Var e in
      if List.exists e_vs ~f:in_vs then
        let () =
          Log.debug
          @@ lazy
               (Printf.sprintf "tainting %s from %s via %s\n%!" f
                  (Expr.to_string e) (string_of_strset vs))
        in
        StringSet.add vs f
      else StringSet.remove vs f
  | Assume b ->
      (* TODO:: This might be wrong? *)
      (* Printf.printf "\tassuming %s\n%!" (string_of_test b); *)
      Test.frees `Var b |> fsts |> StringSet.of_list |> StringSet.union vs
  | Seq (c1, c2) -> flows_to (flows_to vs c1) c2
  | Select (_, cs) ->
      concatMap cs ~c:StringSet.union ~f:(fun (b, c) ->
          if
            List.exists (fsts @@ Test.frees `Var b) ~f:in_vs
            && not
                 ( Test.(
                     equals b
                       ( Var ("standard_metadata.egress_spec", 9)
                       %=% Expr.value (0, 9) ))
                 && Manip.is_a_sequence_of_zero_assignments c )
          then
            (* let () = Printf.printf "adding lvars of %s to taint
               analysis\n%!" (string_of_cmd c) in *)
            assigned_vars c
          else flows_to vs c)
  | Apply t ->
      let open StringSet in
      let keys_vs = t.keys |> List.map ~f:Key.var |> of_list |> inter vs in
      let dflt_flows = flows_to vs t.default in
      if is_empty keys_vs then dflt_flows
      else
        List.fold t.actions ~init:dflt_flows ~f:(fun acc (_, params, act) ->
            let paramset = of_list (fsts params) in
            let act_flows = flows_to (union vs paramset) act in
            diff act_flows paramset |> union acc)

let ghost_static_slice (ghosts : int list StringMap.t) cmd =
  let ghost_vars =
    StringMap.fold ghosts ~init:StringSet.empty ~f:(fun ~key ~data acc ->
        List.fold data ~init:acc ~f:(fun acc i ->
            StringSet.add acc @@ Printf.sprintf "%s_hits_row_%d" key i))
  in
  let spooked_vars = flows_to ghost_vars cmd in
  Printf.printf "spooked vars %s\n%!" (string_of_strset spooked_vars) ;
  static_slice spooked_vars cmd

let could_hit facts (matches, _, _) =
  List.for_all matches ~f:(fun m ->
      match StringMap.find facts (Match.get_key m) with
      | None -> true
      | Some possible_values ->
          List.exists possible_values
            ~f:(( && ) (Fn.non Match.is_wildcard m) %. Match.hits m))

let size_of_facts =
  StringMap.fold ~init:0 ~f:(fun ~key:_ ~data acc ->
      1 + List.length data + acc)

let edit_slice_table (params : Parameters.t)
    (name, (keys : Cmd.Key.t list), actions, default)
    (facts : Value.t list StringMap.t) (inst : Instance.t)
    (edits : Edit.t list) =
  let eliminable =
    List.find (List.map ~f:Cmd.Key.var_name keys) ~f:(fun k ->
        StringMap.find facts k
        |> Option.value_map ~f:(Fn.non List.is_empty) ~default:false)
  in
  let edits_to_add =
    List.filter edits ~f:(String.( = ) name %. Edit.table)
  in
  let sliced_inst =
    if Option.is_none eliminable then (
      Log.debug
      @@ lazy
           (Printf.sprintf "We can eliminate extant rows in table %s\n%!"
              name) ;
      Instance.of_edits params edits_to_add )
    else (
      Log.debug
      @@ lazy
           (Printf.sprintf
              "we know about %s, so we can't immediately eliminate extant \
               rows in %s\n\
               %!"
              (Option.value_exn eliminable)
              name) ;
      Log.debug @@ lazy (string_of_facts facts) ;
      let extant_rows =
        Instance.get_rows (Instance.update_list params inst edits) name
      in
      let relevant_extant_rows =
        List.filter extant_rows ~f:(could_hit facts)
      in
      let relevant_extant_inst =
        Instance.(set_rows empty ~table:name ~rows:relevant_extant_rows)
      in
      Instance.update_list params relevant_extant_inst edits_to_add )
  in
  (* if params.debug then Printf.printf "New %s Instance is:%s\n%!" name
     (Instance.to_string sliced_inst); *)
  let table = Cmd.Apply {name; keys; actions; default} in
  (* if params.debug then Printf.printf "Applying it to table %s\n%!"
     (string_of_cmd table); *)
  let table = Instance.verify_apply ~no_miss:true params sliced_inst table in
  let facts' = ConstantProp.propogate_choices facts table in
  Log.debug
  @@ lazy
       (Printf.sprintf "slicing %s to \n%s\n%!" name (Cmd.to_string table)) ;
  (table, facts')

let project_to_exprfacts (multifacts : Value.t list StringMap.t) :
    Expr.t StringMap.t =
  StringMap.filter_map multifacts ~f:(function
    | [v] -> Some (Expr.Value v)
    | _ -> None)

let rec edit_slice_aux (params : Parameters.t) facts inst edits cmd =
  let open Cmd in
  Log.debug
  @@ lazy
       (Printf.sprintf "Slice loop with %d facts across %d keys\n%!"
          (size_of_facts facts)
          (StringMap.keys facts |> List.length)) ;
  match cmd with
  | Skip | Assume _ -> (cmd, facts)
  | Assign _ -> (cmd, ConstantProp.propogate_choices facts cmd)
  | Seq (c1, c2) ->
      let c1', facts1 = edit_slice_aux params facts inst edits c1 in
      let c2', facts2 = edit_slice_aux params facts1 inst edits c2 in
      let c' = c1' %:% c2' in
      (c', facts2)
  | Select (typ, cs) ->
      let cs, facts' =
        List.fold cs ~init:([], StringMap.empty)
          ~f:(fun (acc_cs, acc_facts) (b, c) ->
            let c', facts = edit_slice_aux params facts inst edits c in
            let b' =
              ConstantProp.propogate_test (project_to_exprfacts facts) b
            in
            (acc_cs @ [(b', c')], multimap_union acc_facts facts))
      in
      let c' = select typ cs in
      (c', facts')
  | Apply t ->
      edit_slice_table params
        (t.name, t.keys, t.actions, t.default)
        facts inst edits

let edit_slice params inst edits cmd =
  edit_slice_aux params StringMap.empty inst edits cmd |> fst

let rec restrict ~dir params inst reads cmd :
    StringSet.t * int list StringMap.t =
  let open Cmd in
  match cmd with
  | Skip -> (reads, StringMap.empty)
  | Assume t ->
      let tvars = Test.frees `Var t |> fvs_to_set in
      (tvars |> StringSet.union reads, StringMap.empty)
  | Assign (f, e) ->
      ( Expr.frees `Var e
        |> fvs_to_set
        |> StringSet.union @@ StringSet.remove reads f
      , StringMap.empty )
  | Seq (c1, c2) ->
      let proc_fst, proc_snd =
        match dir with
        | `Bck -> (c2, c1)
        | `Fwd -> (c1, c2)
      in
      let inters, slice_f = restrict ~dir params inst reads proc_fst in
      let reads', slice_s = restrict ~dir params inst inters proc_snd in
      (reads', disjoint_union slice_f slice_s)
  | Select (_, cs) ->
      concatMap cs
        ~c:(fun (r, i) (r', i') ->
          (StringSet.union r r', disjoint_union i i'))
        ~f:(fun (b, c) ->
          let reads, inters = restrict ~dir params inst reads c in
          ( StringSet.(union (of_list (fsts (Test.frees `Var b))) reads)
          , inters ))
  | Apply t ->
      (* let printall = true in
       * if printall then Printf.printf "%s (%s) are %s\n%!" (if dir = `Bck then "Reads" else "Writes")  t.name (string_of_strset reads); *)
      let rows, reads =
        List.foldi (Instance.get_rows inst t.name)
          ~init:([], StringSet.empty)
          ~f:(fun i (rowidxs, reads_acc) (ms, _, aid) ->
            let keys = StringSet.of_list @@ Match.relevant_keys ms in
            let lvars =
              List.nth_exn t.actions aid |> trd3 |> assigned_vars
            in
            let pre, post =
              match dir with
              | `Bck -> (lvars, keys)
              | `Fwd -> (keys, lvars)
            in
            if StringSet.(are_disjoint reads pre) then (rowidxs, reads_acc)
            else (rowidxs @ [i], StringSet.(union (diff reads_acc pre) post)))
      in
      let def_reads, def_inst = restrict ~dir params inst reads t.default in
      ( StringSet.union def_reads reads
      , StringMap.(set def_inst ~key:t.name ~data:rows) )

let rec truncate_opt ?(dir = `Bck) cmd table : Cmd.t option =
  let open Option in
  let open Cmd in
  match cmd with
  | Apply t -> if String.(t.name = table) then Some Skip else None
  | Seq (c1, c2) -> (
    match (truncate_opt ~dir c1 table, truncate_opt ~dir c2 table, dir) with
    | Some c, None, `Bck -> Some c
    | Some c, None, `Fwd -> Some (c %:% c2)
    | None, Some c, `Bck -> Some (c1 %:% c)
    | None, Some c, `Fwd -> Some c
    | None, None, _ -> None
    | Some _, Some _, _ -> failwith "Found %s multiply" )
  | Select (typ, cs) ->
      List.find_map cs ~f:(fun (b, c) ->
          truncate_opt ~dir c table >>| mkPair b)
      >>| select typ %. List.return
  | Skip | Assign _ | Assume _ -> None

let truncate ?(dir = `Bck) cmd table : Cmd.t =
  truncate_opt ~dir cmd table
  |> Option.value_exn
       ~message:
         (Printf.sprintf
            "[StaticSlicing.truncate] Couldn't find table %s in command"
            table)

let print_slice slice inst =
  StringMap.fold slice ~init:"" ~f:(fun ~key ~data acc ->
      Printf.sprintf "%s\n%s -> %s of %d" acc key (string_of_intlist data)
        (Instance.get_rows inst key |> List.length))
  |> Printf.printf "keep %s\n"

let restrict_inst_for_edit params cmd inst e =
  let gets = Edit.read_vars cmd e in
  let sets = Edit.write_vars cmd e in
  let bck_slice =
    truncate ~dir:`Bck cmd (Edit.table e)
    |> restrict ~dir:`Bck params inst gets
    |> snd
  in
  (* Printf.printf "%s backwards requires\n%!" (Edit.table e);
   * print_slice bck_slice inst; *)
  let fwd_slice =
    truncate ~dir:`Fwd cmd (Edit.table e)
    |> restrict ~dir:`Fwd params inst sets
    |> snd
  in
  (* Printf.printf "%s forwards requires \n%!" (Edit.table e); *)
  (* print_slice fwd_slice inst; *)
  let rows =
    Instance.get_rows inst (Edit.table e)
    |> get_indices_matching ~f:(Row.equals (Edit.get_row_exn e))
  in
  multimap_union bck_slice fwd_slice
  |> multimap_union (StringMap.of_alist_exn [(Edit.table e, rows)])

let rule_slice (params : Parameters.t) inst edits cmd =
  (* Printf.printf "slicing w.r.t. \n%s%!" @@ Edit.list_to_string edits; *)
  let slice =
    concatMap edits ~c:multimap_union
      ~f:(restrict_inst_for_edit params cmd inst)
  in
  (* Printf.printf "edits: %s\n" (Edit.list_to_string edits);
   * print_slice slice inst; *)
  Instance.project slice inst
