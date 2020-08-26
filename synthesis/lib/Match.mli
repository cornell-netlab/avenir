open Ast

type t

val exact_ : value -> t
val between_ : value -> value -> t
val mask_ : value -> value -> t
val equal : t -> t -> bool
val to_string : t -> string
val to_test : string * size -> t -> test
val to_test_hole : string -> string -> t -> test
val test_hole_of_lists : string -> (string * 'a * 'b option) list -> t list -> test
val to_valuation_test :  string -> (string * size) -> [`Mask | `Exact] -> t -> test
val list_to_string : t list -> string
val list_to_test : (string * size * value option) list -> t list -> test
val mk_ipv6_match : string -> t
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
