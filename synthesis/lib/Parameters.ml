open Core
open Util

type t =
  {
    widening : bool;
    do_slice : bool;
    gas : int;
    debug: bool;
    interactive : bool;
  }

let default =
  { widening = false;
    do_slice = false;
    gas = 1000;
    debug = true;
    interactive = false }  


