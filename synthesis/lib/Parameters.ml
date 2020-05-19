open Core
open Util

type t =
  {
    widening : bool;
    do_slice : bool;
    edits_depth : int;
    search_width : int;
    debug: bool;
    monotonic: bool;
    interactive : bool;
    injection: bool;
    fastcx : bool;
    vcache : bool;
    ecache : bool;
    shortening : bool;
    above: bool;
    minimize : bool;
    hints : bool;
    only_holes : bool;
    allow_annotations : bool;
    nlp : bool;
    unique_edits : bool;
    domain : bool;
    restrict_mask : bool;
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
    allow_annotations = false;
    nlp = false;
    unique_edits = false;
    domain = false;
  }
