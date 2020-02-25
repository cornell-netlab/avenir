open Core
open Util

type t =
  {
    widening : bool;
    gas : int;
    debug: bool;
    interactive : bool;
  }

let default =
  { widening = false;
    gas = 1000;
    debug = true;
    interactive = false }  


