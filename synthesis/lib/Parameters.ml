open Core
open Util

type t =
  {
    (* interaction *)
    debug: bool;
    interactive : bool;
    (*bounded search*)
    edits_depth : int;
    search_width : int;
    timeout : (Time.t * Time.Span.t ) option;
    (*Verfications*)
    fastcx : bool;
    vcache : bool;
    ecache : bool;
    minimize : bool;
    do_slice : bool;
    shortening : bool;

    (*Model finding*)
    widening : bool;
    monotonic: bool;
    injection: bool;
    hints : bool;
    only_holes : bool;
    unique_edits : bool;
    domain : bool;
    restrict_mask : bool;
    no_defaults : bool;
    no_deletes : bool;

    (* outddated *)
    allow_annotations : bool;
    nlp : bool;
    above: bool;
  }

let default =
  { widening = false;
    do_slice = false;
    vcache = false;
    ecache = false;
    shortening = false;
    edits_depth = 6;
    search_width = 100;
    debug = false;
    monotonic = false;
    injection = false;
    interactive = false;
    fastcx = false;
    above = false;
    minimize = false;
    hints = false;
    only_holes = false;
    restrict_mask = false;
    no_defaults = false;
    no_deletes = false;

    allow_annotations = false;
    nlp = false;
    unique_edits = false;
    domain = false;
    timeout = None;
  }

let to_string params =
  let s = if params.widening then "w_" else ""in
  let s = s ^ if params.monotonic then "m_" else "" in
  let s = s ^ if params.injection then "inj_" else "" in
  let s = s ^ if params.hints then "hints_" else "" in
  let s = s ^ if params.only_holes then "only_holes_" else "" in
  let s = s ^ if params.unique_edits then "unique_edits_" else "" in
  let s = s ^ if params.domain then "domain_" else "" in
  let s = s ^ if params.restrict_mask then "restrict_mask" else "" in
  let s = s ^ if params.no_deletes then "no_delets" else "" in
  s

let all_params params =
  List.map (range_ex 0 256)
    ~f:(fun i ->
      {params with widening = i land 1 > 0;
                   monotonic = i land 2 > 0;
                   injection = i land 4 > 0;
                   hints = i land 8 > 0;
                   only_holes = i land 16 > 0;
                   unique_edits = i land 32 > 0;
                   domain = i land 64 > 0;
                   restrict_mask = i land 128 > 0})
