type t =
  | Value of Value.t
  | Var of (string * int)
  | Hole of (string * int)
  | Plus of (t * t)
  | SatPlus of (t * t)
  | SatMinus of (t * t)
  | Times of (t * t)
  | Minus of (t * t)
  | Mask of (t * t)
  | Xor of (t * t)
  | BOr of (t * t)
  | Shl of (t * t)
  | Concat of (t * t)
  | Cast of (int * t)
  | Slice of {hi: int; lo: int; bits: t}

val to_string : t -> string
(** [to_string e] produces a string for an expression [e] *)

val to_sexp_string : t -> string
(** [to_sexp_string e] produces a string for an expression [e] *)

val equals : t -> t -> bool
(** [equals e1 e2] is true if [e1] and [e2] are equivalent ASTs *)

(* [value] is a smart constructor for [Value] expressions *)
val value : int * int -> t

(* [cast] is a smart constructor for [Cast] expressions *)
val cast : int -> t -> t

(* [plus] is a smart constructor for [plus] expressions *)
val plus : t -> t -> t

(* [minus] is a smart constructor for [Minus] expressions *)
val minus : t -> t -> t

(* [times] is a smart constructor for [Times] expressions *)
val times : t -> t -> t

(* [sat_plus] is a smart constructor for [SatPlus] expressions *)
val sat_plus : t -> t -> t

(* [sat_minus] is a smart constructor for [SatMinus] expressions *)
val sat_minus : t -> t -> t

(* [mask] is a smart constructor for [Mask] expressions *)
val mask : t -> t -> t

(* [xor] is a smart constructor for [Xor] expressions *)
val xor : t -> t -> t

(* [or_] is a smart constructor for [BOr] expressions *)
val or_ : t -> t -> t

(* [shl] is a smart constructor for [Shl] expressions *)
val shl : t -> t -> t

(* [shl] is a smart constructor for [Concat] expressions *)
val concat : t -> t -> t

(* [shl] is a smart constructor for [Slice] expressions *)
val slice : int -> int -> t -> t

val bin_ctor : t -> t -> t -> t
(** [bin_ctor e e1 e2] produces a function that combines [e1] and [e2] using
    the ** same toplevel constructor as [e] *)

val un_ctor : t -> t -> t
(** [un_ctor e e1] produces a function that processes [e1] using the ** same
    toplevel constructor as [e], including integer arguments *)

val bin_eval : t -> Value.t -> Value.t -> Value.t
(** [bin_eval e v1 v2] applies the interpretation function for the toplevel
    ast node of [e] to [v1] and [v2]*)

val un_eval : t -> Value.t -> Value.t
(** [bin_eval e v] applies the interpretation function for the toplevel ast
    node of [e] to [v], keeping integer arguments in place*)

val size : t -> int
(** [size e] is the bitwidth of the expression [e] *)

val num_nodes : t -> int
(** [num_nodes e] is the size of the ast for [e] *)

val ord : t -> int
(** [ord e] associates each Ast node type with an integer -- used for normal
    forms *)

val frees : [`Hole | `Var] -> t -> (string * int) list
(** [frees typ e] computes the list of sized identifiers. If [typ = `Hole] it
    returns all [Hole]s. If [type = `Var] it returns all [Var]s. *)

val vars : t -> (string * int) list
(** [vars e == frees `Var e]*)

val holes : t -> (string * int) list
(** [holes e == frees `Holes e]*)

val has_hole : t -> bool
(** [has_hole e] is true iff [e] contains a [Hole] AST node*)

val multi_vals : t -> Value.t list
(** [multi_vals e] is the list of values that occur in [e]. The [multi_]
    prefix indicates that the list may contain duplicates *)

val holify : f:(string * int -> string * int) -> string list -> t -> t
(** [holify f vs e] replaces each sized variable [(v,sz)] in [frees e ∩ vs]
    with ** [f (v,sz)] ** *)
