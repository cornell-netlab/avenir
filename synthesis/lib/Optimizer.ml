open Core
open Ast

type t =
  { encode : (Parameters.t -> Instance.t -> cmd -> cmd) option ;
    query_tfx : test -> test;
  }
