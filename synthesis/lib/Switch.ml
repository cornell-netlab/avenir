open Core
open Util
open Ast
open Tables

type t =
  {
    pipeline : cmd ref;
    inst : Instance.t ref;
    edits : Edit.t list;
    gcl : cmd option ref;
    gcl_holes : cmd option ref;
    dels : Instance.interp option ref;
    tag : [`Mask | `Exact] option ref;
    edited_inst : Instance.t option ref;
  }

let make pipeline inst edits : t =
  {pipeline = ref pipeline;
   inst = ref inst;
   edits;
   gcl = ref None;
   gcl_holes = ref None;
   dels = ref None;
   tag = ref None;
   edited_inst = ref None
  }

let pipeline p = !(p.pipeline)
let inst p = !(p.inst)
let edits p = p.edits

let edited_instance (p : t) =
  match !(p.edited_inst) with
  | None ->
     let i = Instance.update_list !(p.inst) p.edits in
     p.edited_inst := Some i;
     i
  | Some i -> i

let to_gcl (p : t) =
  match !(p.gcl) with
  | None ->
     let i = Instance.apply NoHoles `Exact (edited_instance p) !(p.pipeline) |> fst in
     p.gcl := Some i;
     i
  | Some i -> i


let to_gcl_holes (p : t) dels tag =

  match !(p.gcl_holes), !(p.dels), !(p.tag) with
  | Some i, Some d, Some t when d = dels && t = tag -> i
  | _ ->
     let i = Instance.apply ~no_miss:false dels tag (edited_instance p) !(p.pipeline) |> fst in
     p.gcl_holes := Some i;
     p.dels := Some dels;
     p.tag := Some tag;
     i

let to_string (p : t) = to_gcl p |> string_of_cmd

let clear_cache (p : t) = {p with
                            gcl = ref None;
                            gcl_holes = ref None;
                            edited_inst = ref None}

let replace_edits (p : t) (edits : Edit.t list) = {p with edits} |> clear_cache
let append_edits (p : t) (edits : Edit.t list) = {p with edits = p.edits @ edits} |> clear_cache

let update_inst (p : t) (edits : Edit.t list) =
  p.inst := Instance.update_list !(p.inst) edits;
  p |> clear_cache

let replace_inst (p : t) (i : Instance.t) =
  {p with inst = ref i}
  |> clear_cache


let replace_pipeline (p : t) (c : cmd) =
  {p with pipeline = ref c}
  |> clear_cache
