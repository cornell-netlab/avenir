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

let gen_outname curr = Printf.sprintf "y%d" curr

let gen_act width curr =
  ( gen_actname curr
  , [gen_arg width curr]
  , Cmd.assign (gen_outname curr) (Var (Printf.sprintf "arg%d" curr, width))
  )

let rec gen_acts width curr max =
  if curr >= max then []
  else if curr < 0 then
    failwith
    @@ Printf.sprintf "Cannot generate action with negative index %d" curr
  else gen_act width curr :: gen_acts width (curr + 1) max

let rand_match width curr =
  Match.exact_ (gen_key width curr |> Cmd.Key.var_name) (Value.random width)

let rec rand_matches width curr num_keys =
  if curr >= num_keys then []
  else if curr < 0 then
    failwith
    @@ Printf.sprintf "cannot generate negatively indexed key %i" curr
  else rand_match width curr :: rand_matches width (curr + 1) num_keys

let rand_act num_acts = Random.int num_acts

let rand_act_data width = [Value.random ~lo:1 width]

module Obt = struct
  let tblname = "onebigtable"

  let rand_row width num_keys num_acts =
    let matches = rand_matches width 0 num_keys in
    let acts = rand_act num_acts in
    let act_data = rand_act_data width in
    (matches, act_data, acts)

  let rand_edit width num_keys num_acts =
    Edit.Add (tblname, rand_row width num_keys num_acts)

  let rec rand_edits width num_keys num_acts num =
    if num <= 0 then []
    else
      rand_edit width num_keys num_acts
      :: rand_edits width num_keys num_acts (num - 1)

  let gen bitwidth num_keys num_acts =
    let open Cmd in
    let name = tblname in
    let keys = gen_keys bitwidth 0 num_keys in
    let actions = gen_acts bitwidth 0 num_acts in
    let default = "meta" %<-% Value (Value.zero bitwidth) in
    let obt = Cmd.Apply {name; keys; actions; default} in
    let fvs =
      List.filter (Cmd.vars obt) ~f:(fun (x, _) -> String.(x <> "meta"))
    in
    Printf.printf "There are %d fvs %d keys and %d acts\n%!"
      (List.length fvs) num_keys num_acts ;
    assert (List.length fvs = num_keys + num_acts) ;
    (obt, fvs)
end

(* GENERATE ALIASED PIPELINE *)
module Pipe = struct
  let gen_staging bitwidth num_keys =
    let open Cmd in
    let name = "staging" in
    let keys = gen_keys bitwidth 0 num_keys in
    let actions =
      [("set_meta", [("m", bitwidth)], "meta" %<-% Var ("m", bitwidth))]
    in
    let default = "meta" %<-% Value (Value.zero bitwidth) in
    Cmd.Apply {name; keys; actions; default}

  let gen_table_name = Printf.sprintf "meta%d"

  let gen_pipe_table width curr =
    let name = gen_table_name curr in
    let keys = [Cmd.Key.make ("meta", width)] in
    let actions = [gen_act width curr] in
    let default = Cmd.Skip in
    Cmd.Apply {name; keys; actions; default}

  let rec gen_pipeline width curr max_tables =
    if curr >= max_tables then []
    else if curr < 0 then
      failwith @@ Printf.sprintf "Cannot generate negative tables: %d" curr
    else
      gen_pipe_table width curr :: gen_pipeline width (curr + 1) max_tables

  let gen bitwidth num_keys num_acts =
    let open Cmd in
    let stg = gen_staging bitwidth num_keys in
    let pipe = gen_pipeline bitwidth 0 num_acts in
    stg :: pipe |> sequence
end

(* END GENERATE ALIASED PIPELINE*)
