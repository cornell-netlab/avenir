open Core
open Util

type t =
  {
    widening : bool;
    gas : int;
    hints : (CandidateMap.trace -> CandidateMap.trace list) option;
    debug: bool;
    interactive : bool;
  }

let default =
  { widening = false;
    gas = 1000;
    hints = None;
    debug = true;
    interactive = false }  


