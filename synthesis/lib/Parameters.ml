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
    fastcx : bool;
  }

let default =
  { widening = false;
    do_slice = false;
    gas = 1000;
    debug = true;
    monotonic = true;
    interactive = false;
    fastcx = false }
