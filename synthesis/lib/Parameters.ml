open Core
open Util

type t =
  {
    widening : bool;
    do_slice : bool;
    gas : int;
    debug: bool;
    monotonic: bool;
    interactive : bool;
    injection: bool;
    fastcx : bool;
    cache : bool;
    del_pushdown: bool;
    above:bool;
    minimize : bool;
  }

let default =
  { widening = false;
    do_slice = false;
    cache = true;
    gas = 1000;
    debug = false;
    monotonic = false;
    injection = false;
    interactive = false;
    fastcx = false;
    del_pushdown = false;
    above = false;
    minimize = false
  }
