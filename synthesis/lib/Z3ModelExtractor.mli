open Util

val of_smt_model :
  (Z3.Smtlib.identifier * Z3.Smtlib.term) list -> Value.t StringMap.t
