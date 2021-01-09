open Ast
open Util

type t

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or

val exact_ : string -> value -> t
val between_ : string -> value -> value -> t
val mask_ : string -> value -> value -> t
val wildcard : string -> int -> t
val equal : t -> t -> bool
val get_key : t -> string
val to_string : t -> string
val to_bmv2_string : t -> string
val to_test : t -> test
val to_model_alist : string -> t -> (string * value) list
val list_to_model_alist : string -> t list -> (string * value) list
val list_to_model : string -> t list -> value StringMap.t

val to_test_hole : string -> t -> test
val test_hole_of_lists : string -> t list -> test
val to_valuation_test :  string ->  [`Mask | `Exact] -> t -> test
val list_to_string : t list -> string
val list_to_test : t list -> test
val mk_ipv6_match : string -> string -> t
val is_wildcard : t -> bool
val cap : t -> t -> t list
val has_inter : t -> t -> bool
val has_inter_l : t list -> t list -> bool
val is_subset : t -> t -> bool
val get_bitmask : t -> value
val get_exact_val : t -> value
val get_base_value : t -> value
val exactify : t -> t
val exactify_list : t list -> t list
val to_model : ?typ:[`Vals | `NoVals] -> string -> t -> value StringMap.t
val relevant_matches : t list -> t list
val relevant_keys : t list -> string list
val hits : t -> value -> bool
