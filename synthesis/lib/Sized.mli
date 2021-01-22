type 'a t [@@deriving sexp, compare]

val make : 'a -> int -> 'a t

val get : 'a t -> 'a

val size : 'a t -> int

val to_string : f:('a -> string) -> 'a t -> string

val map : f:('a -> 'b) -> 'a t -> 'b

val maps : f:('a -> int -> 'b) -> 'a t -> 'b

val map2 : f:('a -> 'a -> 'b) -> 'a t -> 'a t -> 'b

val map2s2 : f:('a -> int -> 'a -> int -> 'b) -> 'a t -> 'a t -> 'b
(** [map2s2 f x y] is just like map2 except it doesnt require x and y to have
    the same size *)

val fmap : f:('a -> 'b) -> 'a t -> 'b t

val fmaps : f:('a -> int -> 'b) -> 'a t -> 'b t

val fmap2 : f:('a -> 'a -> 'b) -> 'a t -> 'a t -> 'b t

val fmap2s : f:('a -> 'a -> int -> 'b) -> 'a t -> 'a t -> 'b t

val resize : int -> 'a t -> 'a t
