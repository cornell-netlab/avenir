open Ast

type t

val exact_ : string -> value -> t
val between_ : string -> value -> value -> t
val mask_ : string -> value -> value -> t
val equal : t -> t -> bool
val to_string : t -> string
val to_test : t -> test
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
