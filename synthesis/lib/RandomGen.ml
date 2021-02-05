open Core

let gen_key width curr =
  let varname = Printf.sprintf "x%d" curr in
  Cmd.Key.make (varname, width)

let rec gen_keys width curr max =
  if curr >= max then []
  else if curr < 0 then
    failwith
    @@ Printf.sprintf "Cannot generate variable with negative index %d" curr
  else gen_key width curr :: gen_keys width (curr + 1) max

let gen_actname = Printf.sprintf "action%d"

let gen_argname = Printf.sprintf "arg%d"

let gen_arg width curr = (gen_argname curr, width)

let gen_outname = Printf.sprintf "y%d"

let rec bucket_distrib ~f curr buckets max =
  if curr >= max then buckets
  else
    let bkt_idx = curr % List.length buckets in
    let buckets =
      List.mapi buckets ~f:(fun i ->
          if i = bkt_idx then List.cons (f curr, curr) else Fn.id)
    in
    bucket_distrib ~f (curr + 1) buckets max

let rec init_buckets num_buckets =
  if num_buckets = 0 then [] else [] :: init_buckets (num_buckets - 1)

let gen_var_assigns num_acts num_vars =
  bucket_distrib ~f:gen_outname 0 (init_buckets num_acts) num_vars

let gen_data_params num_acts num_data =
  bucket_distrib ~f:gen_argname 0 (init_buckets num_acts) num_data

let make_action_cmd bitwidth vars params =
  Util.map2l vars params ~f:(fun (var, default) p_opt ->
      let open Cmd in
      match p_opt with
      | None -> var %<-% Expr.value (default, bitwidth)
      | Some (param, _) -> var %<-% Expr.Var (param, bitwidth))
  |> Cmd.sequence

let make_action bitwidth i (vars, params) =
  let act = make_action_cmd bitwidth vars params in
  (gen_actname i, List.map params ~f:(fun (p, _) -> (p, bitwidth)), act)

let gen_actions bitwidth num_vars num_data num_acts =
  let vars_for_acts = gen_var_assigns num_acts num_vars in
  let acts_for_vars = gen_data_params num_acts num_data in
  List.zip_exn vars_for_acts acts_for_vars
  |> List.mapi ~f:(make_action bitwidth)

let gen_act use_data width curr =
  let assigned =
    if use_data then Expr.Var (Printf.sprintf "arg%d" curr, width)
    else
      Expr.value (curr, width)
  in
  let data = if use_data then [gen_arg width curr] else [] in
  (gen_actname curr, data, Cmd.assign (gen_outname curr) assigned)

let rec gen_acts use_data width curr max =
  if curr >= max then []
  else if curr < 0 then
    failwith
    @@ Printf.sprintf "Cannot generate action with negative index %d" curr
  else gen_act use_data width curr :: gen_acts use_data width (curr + 1) max

let rand_match width curr =
  Match.exact_ (gen_key width curr |> Cmd.Key.var_name) (Value.random width)

let rec rand_matches width curr nkeys =
  if curr >= nkeys then []
  else if curr < 0 then
    failwith
    @@ Printf.sprintf "cannot generate negatively indexed key %i" curr
  else rand_match width curr :: rand_matches width (curr + 1) nkeys

let rand_act num_acts = Random.int num_acts

let rand_act_data width = Value.random ~lo:1 width

module Obt = struct
  let tblname = "onebigtable"

  let rand_row use_data width nkeys num_acts =
    let matches = rand_matches width 0 nkeys in
    let acts = rand_act num_acts in
    let act_data = if use_data then [rand_act_data width] else [] in
    (matches, act_data, acts)

  let rand_edit use_data width nkeys num_acts =
    Edit.Add (tblname, rand_row use_data width nkeys num_acts)

  let rec rand_edits use_data width nkeys num_acts num =
    if num <= 0 then []
    else
      rand_edit use_data width nkeys num_acts
      :: rand_edits use_data width nkeys num_acts (num - 1)

  let rec rand_data_list width num =
    if num <= 0 then []
    else rand_act_data width :: rand_data_list width (num - 1)

  let rand_big_row actions width nkeys : Row.t =
    let matches = rand_matches width 0 nkeys in
    let act_idx = rand_act (List.length actions) in
    let _, params, _ = List.nth_exn actions act_idx in
    let data = rand_data_list width (List.length params) in
    (matches, data, act_idx)

  let rand_big_edit actions width nkeys =
    Edit.Add (tblname, rand_big_row actions width nkeys)

  let rec rand_big_edits actions width nkeys num =
    if num <= 0 then []
    else
      rand_big_edit actions width nkeys
      :: rand_big_edits actions width nkeys (num - 1)

  let gen use_data bitwidth nkeys num_acts =
    let open Cmd in
    let name = tblname in
    let keys = gen_keys bitwidth 0 nkeys in
    let actions = gen_acts use_data bitwidth 0 num_acts in
    let default = "meta" %<-% Value (Value.zero bitwidth) in
    let obt = Cmd.Apply {name; keys; actions; default} in
    let fvs =
      List.filter (Cmd.vars obt) ~f:(fun (x, _) -> String.(x <> "meta"))
    in
    Printf.printf "There are %d fvs %d keys and %d acts\n%!"
      (List.length fvs) nkeys num_acts ;
    (* assert (List.length fvs = nkeys + num_acts) ; *)
    (obt, fvs)

  let gen_big bitwidth nkeys nvars ndata nacts =
    let open Cmd in
    let name = tblname in
    let keys = gen_keys bitwidth 0 nkeys in
    let actions = gen_actions bitwidth nvars ndata nacts in
    let default = "meta" %<-% Value (Value.zero bitwidth) in
    let obt = Apply {name; keys; actions; default} in
    let fvs =
      List.filter (Cmd.vars obt) ~f:(fun (x, _) -> String.(x <> "meta"))
    in
    (* assert (List.length fvs = nkeys + num_acts) ; *)
    (obt, fvs, actions)
end

(* GENERATE ALIASED PIPELINE *)
module Pipe = struct
  let gen_staging bitwidth nkeys =
    let open Cmd in
    let name = "staging" in
    let keys = gen_keys bitwidth 0 nkeys in
    let actions =
      [("set_meta", [("m", bitwidth)], "meta" %<-% Var ("m", bitwidth))]
    in
    let default = "meta" %<-% Value (Value.zero bitwidth) in
    Cmd.Apply {name; keys; actions; default}

  let gen_table_name = Printf.sprintf "meta%d"

  let gen_table width curr actions =
    let name = gen_table_name curr in
    let keys = [Cmd.Key.make ("meta", width)] in
    let default = Cmd.Skip in
    Cmd.Apply {name; keys; actions; default}

  let gen_pipe_table use_data width curr =
    [gen_act use_data width curr] |> gen_table width curr

  let rec gen_pipeline use_data width curr max_tables =
    if curr >= max_tables then []
    else if curr < 0 then
      failwith @@ Printf.sprintf "Cannot generate negative tables: %d" curr
    else
      gen_pipe_table use_data width curr
      :: gen_pipeline use_data width (curr + 1) max_tables

  let gen_big_pipeline bitwidth ntables nvars ndata nacts =
    let actions = gen_actions bitwidth nvars ndata nacts in
    let tables = init_buckets ntables in
    let tables_actions =
      bucket_distrib 0 tables (List.length actions) ~f:(fun i ->
          List.nth_exn actions i)
      |> List.map ~f:(List.map ~f:fst)
    in
    List.mapi tables_actions ~f:(gen_table bitwidth)

  let gen use_data bitwidth nkeys num_acts =
    let open Cmd in
    let stg = gen_staging bitwidth nkeys in
    let pipe = gen_pipeline use_data bitwidth 0 num_acts in
    stg :: pipe |> sequence

  let gen_big bitwidth nkeys ntables nvars ndata nacts =
    let open Cmd in
    let stg = gen_staging bitwidth nkeys in
    let pipe = gen_big_pipeline bitwidth ntables nvars ndata nacts in
    Log.debug @@ lazy (Printf.sprintf "Generated %d pipeline tables\n" (List.length pipe));
    Printf.printf "There are %d keys %d tables %d vars %d data and %d acts\n%!"
      nkeys ntables nvars ndata nacts ;
    stg :: pipe |> sequence
end

(* END GENERATE ALIASED PIPELINE*)
