type t

val exact_ : string -> Value.t -> t

val between_ : string -> Value.t -> Value.t -> t

val mask_ : string -> Value.t -> Value.t -> t

val wildcard : string -> int -> t

val equal : t -> t -> bool

val get_key : t -> string

val to_string : t -> string

val to_bmv2_string : t -> string

val to_test : t -> Test.t

val to_model_alist : string -> t -> (string * Value.t) list

val list_to_model_alist : string -> t list -> (string * Value.t) list

val list_to_model : string -> t list -> Model.t

val to_test_hole : string -> t -> Test.t

val test_hole_of_lists : string -> t list -> Test.t

val to_valuation_test : string -> [`Mask | `Exact] -> t -> Test.t

val list_to_string : t list -> string

val list_to_test : t list -> Test.t

val mk_ipv6_match : string -> string -> t

val is_wildcard : t -> bool

val cap : t -> t -> t list

val has_inter : t -> t -> bool

val has_inter_l : t list -> t list -> bool

val is_subset : t -> t -> bool

val get_bitmask : t -> Value.t

val get_exact_val : t -> Value.t

val get_base_value : t -> Value.t

val exactify : t -> t

val exactify_list : t list -> t list

val to_model : ?typ:[`Vals | `NoVals] -> string -> t -> Model.t

val relevant_matches : t list -> t list

val relevant_keys : t list -> string list

val hits : t -> Value.t -> bool
