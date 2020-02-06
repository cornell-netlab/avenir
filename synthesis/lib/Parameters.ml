open Core
open Util
open Tables

type t =
  {
    widening : bool;
    gas : int;
    hints : (CandidateMap.trace -> CandidateMap.trace list) option
  }
